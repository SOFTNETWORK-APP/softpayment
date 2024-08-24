package app.softnetwork.payment.service

import akka.http.scaladsl.model.{RemoteAddress, StatusCodes}
import akka.http.scaladsl.model.headers.{`User-Agent`, `X-Forwarded-For`}
import app.softnetwork.api.server.ApiRoutes
import app.softnetwork.api.server.config.ServerSettings.RootPath
import app.softnetwork.payment.config.PaymentSettings.PaymentConfig._
import app.softnetwork.payment.config.{PaymentSettings, StripeApi, StripeSettings}
import app.softnetwork.payment.data.{
  bic,
  birthday,
  cardId,
  cardPreRegistration,
  debitedAmount,
  feesAmount,
  firstName,
  iban,
  lastName,
  legalUser,
  naturalUser,
  orderUuid,
  ownerAddress,
  ownerName,
  preAuthorizationId,
  recurringPaymentRegistrationId,
  sellerBankAccountId,
  ubo,
  uboDeclarationId
}
import app.softnetwork.payment.handlers.MockPaymentDao
import app.softnetwork.payment.message.PaymentMessages.{
  BankAccountCommand,
  CardPreAuthorized,
  IbanMandate,
  PaidIn,
  Payment,
  PaymentRedirection,
  PaymentRequired,
  PaymentResult,
  PreRegisterCard,
  RecurringPaymentRegistered,
  RegisterRecurringPayment,
  Schedule4PaymentTriggered
}
import app.softnetwork.payment.model.{
  computeExternalUuidWithProfile,
  BankAccount,
  CardPreRegistration,
  LegalUser,
  RecurringPayment,
  RecurringPaymentView,
  Transaction,
  UboDeclaration
}
import app.softnetwork.payment.scalatest.StripePaymentRouteTestKit
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.persistence.now
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import app.softnetwork.time._
import com.stripe.model.PaymentIntent
import com.stripe.param.PaymentIntentConfirmParams
import org.scalatest.wordspec.AnyWordSpecLike
import org.slf4j.{Logger, LoggerFactory}
import com.stripe.model.SetupIntent
import com.stripe.param.SetupIntentConfirmParams
import org.json4s.Formats
import org.openqa.selenium.{By, WebDriver, WebElement}
import org.openqa.selenium.htmlunit.HtmlUnitDriver

import java.net.{InetAddress, URLEncoder}
import java.time.LocalDate
import scala.util.{Failure, Success, Try}
import collection.JavaConverters._

trait StripePaymentServiceSpec[SD <: SessionData with SessionDataDecorator[SD]]
    extends AnyWordSpecLike
    with StripePaymentRouteTestKit[SD] { _: ApiRoutes with SessionMaterials[SD] =>

  lazy val log: Logger = LoggerFactory getLogger getClass.getName

  override implicit lazy val providerConfig: StripeApi.Config = StripeSettings.StripeApiConfig

  import app.softnetwork.serialization._

  var customer: String = _

  var payInTransactionId: Option[String] = None

  var payOutTransactionId: Option[String] = None

  val currency = "EUR"

  override implicit def formats: Formats = paymentFormats

  "individual account" should {
    "be created or updated" in {
      externalUserId = "individual"
      createNewSession(sellerSession(externalUserId))
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$bankRoute",
          BankAccountCommand(
            BankAccount(None, ownerName, ownerAddress, iban, bic),
            naturalUser.withExternalUuid(externalUserId),
            Some(true)
          )
        ).withHeaders(
          `X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)),
          `User-Agent`("test")
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val bankAccount = loadBankAccount()
        sellerBankAccountId = bankAccount.bankAccountId
      }
    }
    "accept mandate" in {
      validateKycDocuments()
      withHeaders(
        Post(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$mandateRoute", IbanMandate(iban))
          .withHeaders(
            `X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)),
            `User-Agent`("test")
          )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val paymentAccount = loadPaymentAccount()
        assert(paymentAccount.mandate.map(_.id).isDefined)
        assert(paymentAccount.mandate.map(_.status).isDefined)
      }
    }

    val probe = createTestProbe[PaymentResult]()
    subscribeProbe(probe)

    "register recurring direct debit payment" in {
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$recurringPaymentRoute",
          RegisterRecurringPayment(
            "",
            `type` = RecurringPayment.RecurringPaymentType.DIRECT_DEBIT,
            frequency = Some(RecurringPayment.RecurringPaymentFrequency.DAILY),
            endDate = Some(now()),
            fixedNextAmount = Some(true),
            nextDebitedAmount = Some(1000),
            nextFeesAmount = Some(100)
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        recurringPaymentRegistrationId =
          responseAs[RecurringPaymentRegistered].recurringPaymentRegistrationId
        withHeaders(
          Get(
            s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$recurringPaymentRoute/$recurringPaymentRegistrationId"
          )
        ) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val recurringPayment = responseAs[RecurringPaymentView]
          assert(recurringPayment.`type`.isDirectDebit)
          assert(recurringPayment.frequency.exists(_.isDaily))
          assert(recurringPayment.firstDebitedAmount == 0)
          assert(recurringPayment.firstFeesAmount == 0)
          assert(recurringPayment.fixedNextAmount.exists(_.self))
          assert(recurringPayment.nextDebitedAmount.contains(1000))
          assert(recurringPayment.nextFeesAmount.contains(100))
          assert(recurringPayment.numberOfRecurringPayments.getOrElse(0) == 0)
          assert(
            recurringPayment.nextRecurringPaymentDate.exists(
              LocalDate.now().isEqual(_)
            )
          )
        }
      }
    }

    "execute direct debit automatically for next recurring payment" in {
      withHeaders(
        Delete(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$mandateRoute")
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
      probe.expectMessageType[Schedule4PaymentTriggered]
      withHeaders(
        Get(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$recurringPaymentRoute/$recurringPaymentRegistrationId"
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val recurringPayment = responseAs[RecurringPaymentView]
        assert(recurringPayment.cumulatedDebitedAmount.contains(1000))
        assert(recurringPayment.cumulatedFeesAmount.contains(100))
        assert(
          recurringPayment.lastRecurringPaymentDate.exists(
            LocalDate.now().isEqual(_)
          )
        )
        assert(recurringPayment.lastRecurringPaymentTransactionId.isDefined)
        assert(recurringPayment.numberOfRecurringPayments.contains(1))
        assert(recurringPayment.nextRecurringPaymentDate.isEmpty)
      }
    }
  }

  "sole trader account" should {
    "be created or updated" in {
      externalUserId = "soleTrader"
      createNewSession(sellerSession(externalUserId))
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$bankRoute",
          BankAccountCommand(
            BankAccount(
              Option(sellerBankAccountId),
              ownerName,
              ownerAddress,
              iban,
              bic
            ),
            legalUser.withLegalRepresentative(naturalUser.withExternalUuid(externalUserId)),
            Some(true)
          )
        ).withHeaders(
          `X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)),
          `User-Agent`("test")
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val bankAccount = loadBankAccount()
        sellerBankAccountId = bankAccount.bankAccountId
      }
    }
  }

  "business account" should {
    "be created or updated" in {
      externalUserId = "business"
      createNewSession(sellerSession(externalUserId))
      val bank =
        BankAccountCommand(
          BankAccount(
            Option(sellerBankAccountId),
            ownerName,
            ownerAddress,
            iban,
            bic
          ),
          legalUser
            .withLegalUserType(LegalUser.LegalUserType.BUSINESS)
            .withLegalRepresentative(naturalUser.withExternalUuid(externalUserId)),
          Some(true)
        )
      log.info(serialization.write(bank))
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$bankRoute",
          bank
        ).withHeaders(
          `X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)),
          `User-Agent`("test")
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val bankAccount = loadBankAccount()
        sellerBankAccountId = bankAccount.bankAccountId
      }
    }
    "declare beneficial owner(s)" in {
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$declarationRoute",
          ubo
            .withFirstName(legalUser.legalRepresentative.firstName)
            .withLastName(legalUser.legalRepresentative.lastName)
            .withPercentOwnership(50)
        ).withHeaders(
          `X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)),
          `User-Agent`("test")
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val declaration = loadDeclaration()
        assert(declaration.ubos.size == 1)
        uboDeclarationId = declaration.uboDeclarationId
      }
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$declarationRoute",
          ubo.withPercentOwnership(50)
        ).withHeaders(
          `X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)),
          `User-Agent`("test")
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val declaration = loadDeclaration()
        assert(declaration.ubos.size == 2)
        uboDeclarationId = declaration.uboDeclarationId
      }
    }
    "ask for declaration validation" in {
      withHeaders(
        Put(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$declarationRoute"
        ).withHeaders(
          `X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)),
          `User-Agent`("test")
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val declaration = loadDeclaration()
        assert(
          declaration.status == UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_VALIDATED
        )
      }
    }
  }

  "checkout" should {
    "pre register 3ds card" in {
      createNewSession(customerSession)
      withHeaders(
        Get(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$cardRoute")
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$cardRoute",
          PreRegisterCard(
            orderUuid,
            naturalUser
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        cardPreRegistration = responseAs[CardPreRegistration]
        log.info(serialization.write(cardPreRegistration))
      }

      val paymentAccount = loadPaymentAccount()
      assert(paymentAccount.naturalUser.flatMap(_.userId).isDefined)

      // front end simulation
      // confirm setup intent
      Try {
        val requestOptions = StripeApi().requestOptions

        SetupIntent
          .retrieve(
            cardPreRegistration.id,
            requestOptions
          )
          .confirm(
            SetupIntentConfirmParams
              .builder()
              .setPaymentMethod("pm_card_authenticationRequired") // simulate 3DS
              .build(),
            requestOptions
          )
      } match {
        case Success(_) =>
        case Failure(f) =>
          log.error("Error while confirming setup intent", f)
          fail(f)
      }
    }

    "pre authorize 3ds card" in {
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$preAuthorizeCardRoute",
          Payment(
            orderUuid,
            debitedAmount,
            currency,
            Option(cardPreRegistration.id),
            Option(cardPreRegistration.preregistrationData),
            registerCard = true,
            printReceipt = true
          )
        ).withHeaders(`X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)))
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.Accepted
        val redirection = responseAs[PaymentRedirection]
        log.info(redirection.redirectUrl)
        val params = redirection.redirectUrl
          .split("\\?")
          .last
          .split("[&=]")
          .grouped(2)
          .map(a => (a(0), a(1)))
          .toMap
        preAuthorizationId = params.get("payment_intent")
        assert(preAuthorizationId.isDefined)
//        assert(params.getOrElse("registerCard", "false").toBoolean)
//        assert(params.getOrElse("printReceipt", "false").toBoolean)
        Get(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$callbacksRoute/$preAuthorizeCardRoute/$orderUuid?preAuthorizationIdParameter=payment_intent&payment_intent=$preAuthorizationId&registerCard=true&printReceipt=true"
        ) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val paymentAccount = loadPaymentAccount()
          log.info(serialization.write(paymentAccount))
          assert(paymentAccount.cards.nonEmpty)
        }
        loadCards().find(_.getActive) match {
          case Some(card) =>
            assert(card.firstName == firstName)
            assert(card.lastName == lastName)
            assert(card.birthday == birthday)
            assert(card.getActive)
            assert(!card.expired)
            cardId = card.id
          case _ => fail("No active card found")
        }
      }
    }

    "cancel 3ds card pre authorization" in {
      MockPaymentDao.cancelPreAuthorization(orderUuid, preAuthorizationId) await {
        case Right(result) => assert(result.preAuthorizationCanceled)
        case Left(f)       => fail(f)
      }
    }

    "pay in with 3ds card" in {
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$payInRoute/${URLEncoder
            .encode(computeExternalUuidWithProfile(externalUserId, Some("seller")), "UTF-8")}",
          Payment(
            orderUuid,
            debitedAmount,
            currency,
            Some(cardPreRegistration.id),
            None,
            registerCard = true,
            printReceipt = true
          )
        ).withHeaders(`X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)))
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.Accepted
        val redirection = responseAs[PaymentRedirection]
        val params = redirection.redirectUrl
          .split("\\?")
          .last
          .split("[&=]")
          .grouped(2)
          .map(a => (a(0), a(1)))
          .toMap
        payInTransactionId = params.get("payment_intent")
        assert(payInTransactionId.isDefined)
        /*val registerCard = params.getOrElse("registerCard", "false").toBoolean
        assert(registerCard)
        val printReceipt = params.getOrElse("printReceipt", "false").toBoolean
        assert(printReceipt)*/
      }
    }

    "pay out with 3ds card" in {
      paymentClient.payOut(
        orderUuid,
        computeExternalUuidWithProfile(externalUserId, Some("seller")),
        debitedAmount,
        feesAmount,
        currency,
        Some("reference"),
        payInTransactionId
      ) complete () match {
        case Success(result) =>
          log.info(serialization.write(result))
          assert(result.transactionId.isDefined)
          assert(result.error.isEmpty)
          assert(result.transactionStatus.isTransactionSucceeded)
          payOutTransactionId = result.transactionId
          paymentClient.loadPayOutTransaction(
            orderUuid,
            result.transactionId.get
          ) complete () match {
            case Success(result) =>
              log.info(serialization.write(result))
              assert(result.transactionId.getOrElse("") == payOutTransactionId.getOrElse("unknown"))
              assert(result.error.isEmpty)
              assert(result.transactionStatus.isTransactionSucceeded)
            case Failure(f) => fail(f)
          }
        case Failure(f) => fail(f)
      }
    }

    "disable 3ds card" in {
      withHeaders(
        Delete(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$cardRoute?cardId=$cardId"
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        loadCards().find(_.id == cardId) match {
          case Some(card) =>
            assert(!card.getActive)
          case _ => fail("No card found")
        }
      }
    }

    "pre register card" in {
      createNewSession(customerSession)
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$cardRoute",
          PreRegisterCard(
            orderUuid,
            naturalUser
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        cardPreRegistration = responseAs[CardPreRegistration]
      }

      val paymentAccount = loadPaymentAccount()
      assert(paymentAccount.naturalUser.flatMap(_.userId).isDefined)
      customer = paymentAccount.naturalUser.flatMap(_.userId).get

      // front end simulation
      // confirm setup intent
      Try {
        val requestOptions = StripeApi().requestOptions
        SetupIntent
          .retrieve(cardPreRegistration.id, requestOptions)
          .confirm(
            SetupIntentConfirmParams
              .builder()
              .setPaymentMethod("pm_card_visa")
              .build(),
            requestOptions
          )
      } match {
        case Success(_) =>
        case Failure(f) =>
          log.error("Error while confirming setup intent", f)
          fail(f)
      }
    }

    "pre authorize card" in {
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$preAuthorizeCardRoute",
          //${URLEncoder.encode(computeExternalUuidWithProfile(externalUserId, Some("seller")), "UTF-8")}
          Payment(
            orderUuid,
            debitedAmount,
            currency,
            Option(cardPreRegistration.id),
            Option(cardPreRegistration.preregistrationData),
            registerCard = true,
            printReceipt = true,
            feesAmount = Some(feesAmount)
          )
        ).withHeaders(
          `X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)),
          `User-Agent`("test")
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val result = responseAs[CardPreAuthorized]
        preAuthorizationId = result.transactionId
        loadCards().find(_.getActive) match {
          case Some(card) =>
            assert(card.firstName == firstName)
            assert(card.lastName == lastName)
            assert(card.birthday == birthday)
            assert(card.getActive)
            assert(!card.expired)
            cardId = card.id
          case _ => fail("No active card found")
        }
      }
    }

    "pay in with pre authorized card" in {
      paymentClient.payInWithCardPreAuthorized(
        preAuthorizationId,
        computeExternalUuidWithProfile(externalUserId, Some("seller")),
        None,
        Some(feesAmount)
      ) complete () match {
        case Success(result) =>
          log.info(serialization.write(result))
          assert(result.transactionId.isDefined)
          assert(result.error.isEmpty)
          assert(result.transactionStatus.isTransactionSucceeded)
          payInTransactionId = result.transactionId
          paymentClient.loadPayInTransaction(orderUuid, payInTransactionId.get) complete () match {
            case Success(result) =>
              log.info(serialization.write(result))
              assert(result.transactionId.getOrElse("") == payInTransactionId.getOrElse("unknown"))
              assert(result.error.isEmpty)
              assert(result.transactionStatus.isTransactionSucceeded)
            case Failure(f) => fail(f)
          }
        case Failure(f) => fail(f)
      }
    }

    "pay out with pre authorized card" in {
      paymentClient.payOut(
        orderUuid,
        computeExternalUuidWithProfile(externalUserId, Some("seller")),
        debitedAmount,
        feesAmount,
        currency,
        Some("reference"),
        payInTransactionId
      ) complete () match {
        case Success(result) =>
          log.info(serialization.write(result))
          assert(result.transactionId.isDefined)
          assert(result.error.isEmpty)
          assert(result.transactionStatus.isTransactionSucceeded)
          payOutTransactionId = result.transactionId
          paymentClient.loadPayOutTransaction(
            orderUuid,
            result.transactionId.get
          ) complete () match {
            case Success(result) =>
              log.info(serialization.write(result))
              assert(result.transactionId.getOrElse("") == payOutTransactionId.getOrElse("unknown"))
              assert(result.error.isEmpty)
              assert(result.transactionStatus.isTransactionSucceeded)
            case Failure(f) => fail(f)
          }
        case Failure(f) => fail(f)
      }
    }

    "pay in with pre registered card" in {
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$payInRoute/${URLEncoder
            .encode(computeExternalUuidWithProfile(externalUserId, Some("seller")), "UTF-8")}",
          Payment(
            orderUuid,
            debitedAmount,
            currency,
            Some(cardPreRegistration.id),
            None,
            registerCard = true,
            printReceipt = true,
            feesAmount = Some(feesAmount)
          )
        ).withHeaders(
          `X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)),
          `User-Agent`("test")
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val result = responseAs[PaidIn]
        payInTransactionId = result.transactionId
        paymentClient.loadPayInTransaction(orderUuid, payInTransactionId.get) complete () match {
          case Success(result) =>
            log.info(serialization.write(result))
            assert(result.transactionId.getOrElse("") == payInTransactionId.getOrElse("unknown"))
            assert(result.error.isEmpty)
            assert(result.transactionStatus.isTransactionSucceeded)
          case Failure(f) => fail(f)
        }
        loadCards().find(_.getActive) match {
          case Some(card) =>
            assert(card.firstName == firstName)
            assert(card.lastName == lastName)
            assert(card.birthday == birthday)
            assert(card.getActive)
            assert(!card.expired)
            cardId = card.id
          case _ => fail("No active card found")
        }
      }
    }

    "pay out with pre registered card" in {
      paymentClient.payOut(
        orderUuid,
        computeExternalUuidWithProfile(externalUserId, Some("seller")),
        debitedAmount,
        feesAmount,
        currency,
        Some("reference"),
        payInTransactionId
      ) complete () match {
        case Success(result) =>
          log.info(serialization.write(result))
          assert(result.transactionId.isDefined)
          assert(result.error.isEmpty)
          assert(result.transactionStatus.isTransactionSucceeded)
          payOutTransactionId = result.transactionId
          paymentClient.loadPayOutTransaction(
            orderUuid,
            result.transactionId.get
          ) complete () match {
            case Success(result) =>
              log.info(serialization.write(result))
              assert(result.transactionId.getOrElse("") == payOutTransactionId.getOrElse("unknown"))
              assert(result.error.isEmpty)
              assert(result.transactionStatus.isTransactionSucceeded)
            case Failure(f) => fail(f)
          }
        case Failure(f) => fail(f)
      }
    }

    "disable card" in {
      withHeaders(
        Delete(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$cardRoute?cardId=$cardId"
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        loadCards().find(_.id == cardId) match {
          case Some(card) =>
            assert(!card.getActive)
          case _ => fail("No card found")
        }
      }
    }

    "pay in with PayPal" in {
      createNewSession(customerSession)
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$payInRoute/${URLEncoder
            .encode(computeExternalUuidWithProfile(externalUserId, Some("seller")), "UTF-8")}",
          Payment(
            orderUuid,
            debitedAmount,
            paymentType = Transaction.PaymentType.PAYPAL,
            printReceipt = true,
            feesAmount = Some(feesAmount),
            user = Some(naturalUser)
          )
        ).withHeaders(
          `X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)),
          `User-Agent`("test")
        )
      ) ~> routes ~> check {
        if (status == StatusCodes.PaymentRequired) {
          val payment = responseAs[PaymentRequired]
          log.info(s"Payment required -> ${serialization.write(payment)}")

          val paymentClientReturnUrl = payment.paymentClientReturnUrl
          log.info(paymentClientReturnUrl)
          assert(Option(paymentClientReturnUrl).isDefined)

          val clientSecret = payment.paymentClientSecret
          log.info(clientSecret)
          assert(Option(clientSecret).isDefined)

          /*val requestOptions = StripeApi().requestOptions

          PaymentIntent
            .retrieve(payment.transactionId, requestOptions)
            .confirm(
              PaymentIntentConfirmParams
                .builder()
                .setPaymentMethod("pm_paypal") // FIXME
                .setReturnUrl(paymentClientReturnUrl)
                .build(),
              requestOptions
            )

          val index = paymentClientReturnUrl.indexOf(RootPath)
          assert(index > 0)
          val payInUri = paymentClientReturnUrl.substring(index)
          log.info(payInUri)
          withHeaders(
            Get(s"/$payInUri")
          ) ~> routes ~> check {
            status shouldEqual StatusCodes.OK
            val result = responseAs[PaidIn]
            payInTransactionId = result.transactionId
            assert(result.transactionStatus.isTransactionSucceeded)
          }*/
        } else if (status == StatusCodes.Accepted) {
          val redirection = responseAs[PaymentRedirection].redirectUrl
          log.info(redirection)

          // Create a new instance of the HtmlUnit driver
          val driver: WebDriver = new HtmlUnitDriver()
          Try {
            // Navigate to the URL
            driver.get(redirection)
            // Find the button to click
            driver
              .findElements(By.xpath("//*[@id=\"main-content\"]/div[3]/section[1]/div[2]/div/a[1]"))
              .asScala
              .headOption match {
              case Some(button: WebElement) =>
                // Click the button
                button.click()
              case _ => fail("No button found")
            }
          } match {
            case _ =>
              val returnUrl = driver.getCurrentUrl
              log.info(returnUrl)
              driver.quit()
              val index = returnUrl.indexOf(RootPath)
              assert(index > 0)
              val payInUri = returnUrl.substring(index)
              log.info(payInUri)
              withHeaders(
                Get(s"/$payInUri")
              ) ~> routes ~> check {
                status shouldEqual StatusCodes.OK
                val result = responseAs[PaidIn]
                payInTransactionId = result.transactionId
                assert(result.transactionStatus.isTransactionSucceeded)
              }
          }
        } else {
          fail(s"Unexpected status -> $status")
        }
      }
    }

    /*"pay out with PayPal" in {
      paymentClient.payOut(
        orderUuid,
        computeExternalUuidWithProfile(externalUserId, Some("seller")),
        debitedAmount,
        feesAmount,
        currency,
        Some("reference"),
        payInTransactionId
      ) complete () match {
        case Success(result) =>
          log.info(serialization.write(result))
          assert(result.transactionId.isDefined)
          assert(result.error.isEmpty)
          assert(result.transactionStatus.isTransactionSucceeded)
          payOutTransactionId = result.transactionId
          paymentClient.loadPayOutTransaction(
            orderUuid,
            result.transactionId.get
          ) complete () match {
            case Success(result) =>
              log.info(serialization.write(result))
              assert(result.transactionId.getOrElse("") == payOutTransactionId.getOrElse("unknown"))
              assert(result.error.isEmpty)
              assert(result.transactionStatus.isTransactionSucceeded)
            case Failure(f) => fail(f)
          }
        case Failure(f) => fail(f)
      }
    }*/

    "pay in without pre registered card" in {
      createNewSession(customerSession)
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$payInRoute/${URLEncoder
            .encode(computeExternalUuidWithProfile(externalUserId, Some("seller")), "UTF-8")}",
          Payment(
            orderUuid,
            debitedAmount,
            paymentType = Transaction.PaymentType.CARD,
            printReceipt = true,
            feesAmount = Some(feesAmount),
            user = Some(naturalUser)
          )
        ).withHeaders(`X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)))
      ) ~> routes ~> check {
        if (status == StatusCodes.PaymentRequired) {
          val payment = responseAs[PaymentRequired]

          val paymentClientReturnUrl = payment.paymentClientReturnUrl
          log.info(paymentClientReturnUrl)

          val clientSecret = payment.paymentClientSecret
          log.info(clientSecret)

          val requestOptions = StripeApi().requestOptions

          PaymentIntent
            .retrieve(payment.transactionId, requestOptions)
            .confirm(
              PaymentIntentConfirmParams
                .builder()
                .setPaymentMethod("pm_card_visa")
                .build(),
              requestOptions
            )

          val index = paymentClientReturnUrl.indexOf(RootPath)
          assert(index > 0)
          val payInUri = paymentClientReturnUrl.substring(index)
          log.info(payInUri)
          withHeaders(
            Get(s"/$payInUri")
          ) ~> routes ~> check {
            status shouldEqual StatusCodes.OK
            val result = responseAs[PaidIn]
            payInTransactionId = result.transactionId
            assert(result.transactionStatus.isTransactionSucceeded)
          }
        } else {
          status shouldEqual StatusCodes.OK
          val result = responseAs[PaidIn]
          payInTransactionId = result.transactionId
        }
      }
    }

    "pay out without pre registered card" in {
      paymentClient.payOut(
        orderUuid,
        computeExternalUuidWithProfile(externalUserId, Some("seller")),
        debitedAmount,
        feesAmount,
        currency,
        Some("reference"),
        payInTransactionId
      ) complete () match {
        case Success(result) =>
          log.info(serialization.write(result))
          assert(result.transactionId.isDefined)
          assert(result.error.isEmpty)
          assert(result.transactionStatus.isTransactionSucceeded)
          payOutTransactionId = result.transactionId
          paymentClient.loadPayOutTransaction(
            orderUuid,
            result.transactionId.get
          ) complete () match {
            case Success(result) =>
              log.info(serialization.write(result))
              assert(result.transactionId.getOrElse("") == payOutTransactionId.getOrElse("unknown"))
              assert(result.error.isEmpty)
              assert(result.transactionStatus.isTransactionSucceeded)
            case Failure(f) => fail(f)
          }
        case Failure(f) => fail(f)
      }
    }

    "refund pay in and reverse transfer" in {
      paymentClient.refund(
        orderUuid,
        payOutTransactionId,
        debitedAmount,
        Some(feesAmount),
        currency,
        "reason message",
        initializedByClient = false
      ) complete () match {
        case Success(result) =>
          log.info(serialization.write(result))
          assert(result.transactionId.isDefined)
          assert(result.error.isEmpty)
          assert(result.transactionStatus.isTransactionSucceeded)
        case Failure(f) => fail(f)
      }
    }

  }
}
