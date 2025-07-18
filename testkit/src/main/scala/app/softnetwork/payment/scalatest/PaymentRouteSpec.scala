package app.softnetwork.payment.scalatest

import akka.http.scaladsl.model.headers.{`User-Agent`, `X-Forwarded-For`}
import akka.http.scaladsl.model.{RemoteAddress, StatusCodes}
import app.softnetwork.api.server.ApiRoutes
import app.softnetwork.api.server.config.ServerSettings.RootPath
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.config.PaymentSettings.PaymentConfig._
import app.softnetwork.payment.data._
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.SoftPayAccount.Client.Provider
import app.softnetwork.payment.model._
import app.softnetwork.persistence.now
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import app.softnetwork.time._
import org.scalatest.wordspec.AnyWordSpecLike
import org.slf4j.{Logger, LoggerFactory}

import java.net.{InetAddress, URLEncoder}
import java.time.LocalDate
import scala.language.implicitConversions
import scala.util.{Failure, Success}

trait PaymentRouteSpec[SD <: SessionData with SessionDataDecorator[SD]]
    extends AnyWordSpecLike
    with PaymentRouteTestKit[SD] {
  _: ApiRoutes with SessionMaterials[SD] =>

  override lazy val log: Logger = LoggerFactory getLogger getClass.getName

  import app.softnetwork.serialization._

  "Payment service" must {
    "not create bank account with wrong iban" in {
      createNewSession(sellerSession())
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$bankRoute",
          BankAccountCommand(
            BankAccount(None, ownerName, ownerAddress, "", bic),
            naturalUser,
            None,
            None,
            None
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        assert(responseAs[PaymentError].message == WrongIban.message)
      }
    }

    "not create bank account with wrong bic" in {
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$bankRoute",
          BankAccountCommand(
            BankAccount(None, ownerName, ownerAddress, iban, "WRONG"),
            naturalUser,
            None,
            None,
            None
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        assert(responseAs[PaymentError].message == WrongBic.message)
      }
    }

    "create bank account with natural user" in {
      val command =
        BankAccountCommand(
          BankAccount(None, ownerName, ownerAddress, iban, bic),
          naturalUser.withExternalUuid(externalUserId),
          Some(true),
          None,
          None
        )
      log.info(s"create bank account with natural user command: ${serialization.write(command)}")
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$bankRoute",
          command
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

    "update bank account with natural user" in {
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$bankRoute",
          BankAccountCommand(
            BankAccount(Option(sellerBankAccountId), ownerName, ownerAddress, iban, bic),
            naturalUser.withLastName("anotherLastName").withExternalUuid(externalUserId),
            None,
            None,
            None
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val bankAccount = loadBankAccount()
//        val previousBankAccountId = sellerBankAccountId
        sellerBankAccountId = bankAccount.bankAccountId
//        assert(sellerBankAccountId != previousBankAccountId)
      }
    }

    "not update bank account with wrong siret" in {
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
            legalUser.withSiret(""),
            None,
            None,
            None
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        assert(responseAs[PaymentError].message == WrongSiret.message)
      }
    }

    "not update bank account with empty legal name" in {
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
            legalUser.withLegalName(""),
            None,
            None,
            None
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        assert(responseAs[PaymentError].message == LegalNameRequired.message)
      }
    }

    "not update bank account without accepted terms of PSP" in {
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
            legalUser,
            None,
            None,
            None
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        assert(responseAs[PaymentError].message == AcceptedTermsOfPSPRequired.message)
      }
    }

    "update bank account with sole trader legal user" in {
      externalUserId = "soleTrader"
      val command =
        BankAccountCommand(
          BankAccount(
            Option(sellerBankAccountId),
            ownerName,
            ownerAddress,
            iban,
            bic
          ),
          legalUser.withLegalRepresentative(naturalUser.withExternalUuid(externalUserId)),
          Some(true),
          None,
          None
        )
      log.info(
        s"update bank account with sole trader legal user command: ${serialization.write(command)}"
      )
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$bankRoute",
          command
        ).withHeaders(
          `X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)),
          `User-Agent`("test")
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val bankAccount = loadBankAccount()
//        val previousBankAccountId = sellerBankAccountId
        sellerBankAccountId = bankAccount.bankAccountId
//        assert(sellerBankAccountId != previousBankAccountId)
      }
    }

    "update bank account with business legal user" in {
      externalUserId = "business"
      val command =
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
          Some(true),
          None,
          None
        )
      log.info(
        s"update bank account with business legal user command: ${serialization.write(command)}"
      )
      withHeaders(
        Post(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$bankRoute", command)
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val bankAccount = loadBankAccount()
//        val previousBankAccountId = sellerBankAccountId
        sellerBankAccountId = bankAccount.bankAccountId
//        assert(sellerBankAccountId != previousBankAccountId)
      }
    }

    "add document(s)" in {
      addKycDocuments()
    }

    "update document(s) status" in {
      validateKycDocuments()
    }

    "create or update ultimate beneficial owner" in {
      log.info(s"create or update ultimate beneficial owner command: ${serialization.write(ubo)}")
      withHeaders(
        Post(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$declarationRoute", ubo)
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val declaration = loadDeclaration()
        assert(declaration.ubos.size == 1)
        uboDeclarationId = declaration.uboDeclarationId
      }
    }

    "ask for declaration validation" in {
      withHeaders(
        Put(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$declarationRoute")
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val declaration = loadDeclaration()
        assert(
          declaration.status == UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_VALIDATION_ASKED
        )
      }
    }

    "update declaration status" in {
      Get(
        s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$hooksRoute/${Provider.ProviderType.MOCK.name.toLowerCase}?EventType=UBO_DECLARATION_VALIDATED&RessourceId=$uboDeclarationId"
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val declaration = loadDeclaration()
        assert(declaration.status == UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_VALIDATED)
      }
    }

    "pre register card" in {
      createNewSession(customerSession)
      withHeaders(
        Get(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$cardRoute")
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
      val command =
        PreRegisterPaymentMethod(
          orderUuid,
          naturalUser,
          paymentType = Transaction.PaymentType.CARD
        )
      log.info(s"pre register card command: ${serialization.write(command)}")
      withHeaders(
        Post(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$cardRoute", command)
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        preRegistration = responseAs[PreRegistration]
        log.info(s"card pre registration: ${serialization.write(preRegistration)}")
      }
      val paymentAccount = loadPaymentAccount()
      assert(paymentAccount.naturalUser.isDefined)
    }

    "pre authorize card" in {
      val payment =
        Payment(
          orderUuid,
          debitedAmount,
          "EUR",
          Some(preRegistration.id),
          Some(preRegistration.registrationData),
          registerCard = true,
          printReceipt = true,
          feesAmount = Some(feesAmount)
        )
      log.info(s"pre authorize card payment: ${serialization.write(payment)}")
      withHeaders(
        Post(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$preAuthorizeRoute", payment)
          .withHeaders(`X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)))
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
        preAuthorizationId = params.getOrElse("preAuthorizationId", "")
        assert(params.getOrElse("printReceipt", "") == "true")
      }
    }

    "pre authorize card callback" in {
      Get(
        s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$callbacksRoute/$preAuthorizeRoute/$orderUuid?preAuthorizationId=$preAuthorizationId&registerMeansOfPayment=true&printReceipt=true"
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val paymentAccount = loadPaymentAccount()
        log.info(serialization.write(paymentAccount))
        assert(paymentAccount.cards.nonEmpty)
      }
    }

    "load cards" in {
      val card = loadCards().head
      assert(card.firstName == firstName)
      assert(card.lastName == lastName)
      assert(card.birthday == birthday)
      assert(card.getActive)
      assert(!card.expired)
      cardId = card.id
    }

    "pre authorize card without pre registration" in {
      val payment =
        Payment(
          orderUuid,
          debitedAmount,
          "EUR",
          None,
          None,
          registerCard = true,
          printReceipt = true,
          feesAmount = Some(feesAmount),
          user = Option(naturalUser)
        )
      log.info(s"pre authorize without pre registration: ${serialization.write(payment)}")
      withHeaders(
        Post(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$preAuthorizeRoute", payment)
          .withHeaders(`X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)))
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
        preAuthorizationId = params.getOrElse("preAuthorizationId", "")
        assert(params.getOrElse("printReceipt", "") == "true")
      }
    }

    "pre authorize with registered card" in {
      val payment =
        Payment(
          orderUuid,
          debitedAmount,
          "EUR",
          None,
          None,
          registerCard = true,
          printReceipt = true,
          feesAmount = Some(feesAmount),
          user = Option(naturalUser),
          paymentMethodId = Option(cardId)
        )
      log.info(s"pre authorize with registered card: ${serialization.write(payment)}")
      withHeaders(
        Post(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$preAuthorizeRoute", payment)
          .withHeaders(`X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)))
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
        preAuthorizationId = params.getOrElse("preAuthorizationId", "")
        assert(params.getOrElse("printReceipt", "") == "true")
      }
    }

    "pay in / out with pre authorized card" in {
      createNewSession(customerSession)
      val payment =
        Payment(
          orderUuid,
          100,
          "EUR",
          Some(preRegistration.id),
          Some(preRegistration.registrationData),
          registerCard = true,
          printReceipt = true
        )
      log.info(s"pay in / out with pre authorized card payment: ${serialization.write(payment)}")
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$preAuthorizeRoute/${URLEncoder
            .encode(computeExternalUuidWithProfile(sellerUuid, Some("seller")), "UTF-8")}",
          payment
        ).withHeaders(`X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)))
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        preAuthorizationId = responseAs[PaymentPreAuthorized].transactionId
        paymentClient.payInWithPreAuthorization(
          preAuthorizationId,
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          None
        ) complete () match {
          case Success(result) =>
            assert(result.transactionId.isDefined)
            assert(result.error.isEmpty)
            paymentClient.payOut(
              orderUuid,
              computeExternalUuidWithProfile(sellerUuid, Some("seller")),
              100,
              0,
              "EUR",
              Some("reference"),
              result.transactionId
            ) complete () match {
              case Success(s) =>
                assert(s.transactionId.isDefined)
                assert(s.error.isEmpty)
              case Failure(f) => fail(f.getMessage)
            }
          case Failure(f) => fail(f.getMessage)
        }
      }
    }

    "pay in / out with 3ds" in {
      createNewSession(customerSession)
      val payment =
        Payment(
          orderUuid,
          debitedAmount,
          "EUR",
          Some(preRegistration.id),
          Some(preRegistration.registrationData),
          registerCard = true,
          printReceipt = true,
          feesAmount = Some(feesAmount)
        )
      log.info(s"pay in / out with 3ds payment: ${serialization.write(payment)}")
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$payInRoute/${URLEncoder
            .encode(computeExternalUuidWithProfile(sellerUuid, Some("seller")), "UTF-8")}",
          payment
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
        val transactionId = params.getOrElse("transactionId", "")
        val registerMeansOfPayment = params.getOrElse("registerMeansOfPayment", "")
        assert(registerMeansOfPayment == "true")
        val printReceipt = params.getOrElse("printReceipt", "")
        assert(printReceipt == "true")
        Get(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$callbacksRoute/$payInRoute/$orderUuid?transactionId=$transactionId&registerMeansOfPayment=$registerMeansOfPayment&printReceipt=$printReceipt"
        ) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          assert(responseAs[PaidIn].transactionId == transactionId)
          paymentClient.payOut(
            orderUuid,
            computeExternalUuidWithProfile(sellerUuid, Some("seller")),
            debitedAmount,
            feesAmount,
            "EUR",
            None,
            transactionId
          ) complete () match {
            case Success(s) =>
              assert(s.transactionId.isDefined)
              assert(s.error.isEmpty)
            case Failure(f) => fail(f.getMessage)
          }
        }
      }
    }

    "pay in / out with PayPal" in {
      createNewSession(customerSession)
      val payment =
        Payment(
          orderUuid,
          debitedAmount,
          paymentType = Transaction.PaymentType.PAYPAL,
          printReceipt = true,
          feesAmount = Some(feesAmount)
        )
      log.info(s"pay in / out with PayPal payment: ${serialization.write(payment)}")
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$payInRoute/${URLEncoder
            .encode(computeExternalUuidWithProfile(sellerUuid, Some("seller")), "UTF-8")}",
          payment
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
        val transactionId = params.getOrElse("transactionId", "")
        val printReceipt = params.getOrElse("printReceipt", "")
        assert(printReceipt == "true")
        Get(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$callbacksRoute/$payInRoute/$orderUuid?transactionId=$transactionId&registerMeansOfPayment=false&printReceipt=$printReceipt"
        ) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          assert(responseAs[PaidIn].transactionId == transactionId)
          paymentClient.payOut(
            orderUuid,
            computeExternalUuidWithProfile(sellerUuid, Some("seller")),
            debitedAmount,
            feesAmount,
            "EUR",
            None,
            transactionId
          ) complete () match {
            case Success(s) =>
              assert(s.transactionId.isDefined)
              assert(s.error.isEmpty)
            case Failure(f) => fail(f.getMessage)
          }
        }
      }
    }

    "create mandate" in {
      createNewSession(sellerSession())
      withHeaders(
        Post(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$mandateRoute")
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val paymentAccount = loadPaymentAccount()
        assert(paymentAccount.mandate.map(_.id).isDefined)
        assert(paymentAccount.mandate.map(_.status).isDefined)
      }
    }

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
      val probe = createTestProbe[PaymentResult]()
      subscribeProbe(probe)
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

    "register recurring card payment" in {
      createNewSession(customerSession)
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$recurringPaymentRoute",
          RegisterRecurringPayment(
            "",
            `type` = RecurringPayment.RecurringPaymentType.CARD,
            frequency = Some(RecurringPayment.RecurringPaymentFrequency.DAILY),
            endDate = Some(LocalDate.now().plusDays(1)),
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
          assert(recurringPayment.`type`.isCard)
          assert(recurringPayment.frequency.exists(_.isDaily))
          assert(recurringPayment.cardStatus.exists(_.isCreated))
          assert(recurringPayment.firstDebitedAmount == 0)
          assert(recurringPayment.firstFeesAmount == 0)
          assert(recurringPayment.fixedNextAmount.exists(_.self))
          assert(recurringPayment.nextDebitedAmount.contains(1000))
          assert(recurringPayment.nextFeesAmount.contains(100))
          assert(recurringPayment.numberOfRecurringPayments.getOrElse(0) == 0)
          assert(
            recurringPayment.nextRecurringPaymentDate.exists(LocalDate.now().isEqual(_))
          )
        }
      }
    }

    "execute first recurring card payment" in {
      withHeaders(
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$recurringPaymentRoute/${URLEncoder
            .encode(recurringPaymentRegistrationId, "UTF-8")}",
          Payment(
            "",
            0
          )
        ).withHeaders(`X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)))
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        withHeaders(
          Get(
            s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$recurringPaymentRoute/$recurringPaymentRegistrationId"
          )
        ) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val recurringPayment = responseAs[RecurringPaymentView]
          assert(recurringPayment.`type`.isCard)
          assert(recurringPayment.frequency.exists(_.isDaily))
          assert(recurringPayment.cardStatus.exists(_.isInProgress))
          assert(recurringPayment.firstDebitedAmount == 0)
          assert(recurringPayment.firstFeesAmount == 0)
          assert(recurringPayment.fixedNextAmount.exists(_.self))
          assert(recurringPayment.nextDebitedAmount.contains(1000))
          assert(recurringPayment.nextFeesAmount.contains(100))
          assert(recurringPayment.numberOfRecurringPayments.getOrElse(0) == 1)
          assert(recurringPayment.cumulatedDebitedAmount.contains(0))
          assert(recurringPayment.cumulatedFeesAmount.contains(0))
          assert(
            recurringPayment.nextRecurringPaymentDate.exists(
              _.equals(LocalDate.now().plusDays(1).toDate)
            )
          )
        }
      }
    }

    "cancel mandate" in {
      createNewSession(sellerSession())
      withHeaders(
        Delete(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$mandateRoute")
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        assert(loadPaymentAccount().bankAccount.flatMap(_.mandateId).isEmpty)
        assert(loadPaymentAccount().bankAccount.flatMap(_.mandateStatus).isEmpty)
      }
    }

    "invalidate regular user" in {
      val paymentAccount = loadPaymentAccount()
      assert(paymentAccount.paymentAccountStatus.isCompteOk)
      val userId = paymentAccount.legalUser.flatMap(_.legalRepresentative.userId).getOrElse("")
      Get(
        s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$hooksRoute/${Provider.ProviderType.MOCK.name.toLowerCase}?EventType=USER_KYC_LIGHT&RessourceId=$userId"
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        assert(loadPaymentAccount().paymentAccountStatus.isDocumentsKo)
      }
    }

    "delete bank account" in {
      withHeaders(
        Delete(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$bankRoute")
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        assert(loadPaymentAccount().bankAccount.isEmpty)
      }
    }

    "disable card(s)" in {
      createNewSession(customerSession)
      withHeaders(
        Delete(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$cardRoute?cardId=$cardId")
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
      withHeaders(
        Put(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$recurringPaymentRoute",
          UpdateRecurringCardPaymentRegistration(
            "",
            recurringPaymentRegistrationId,
            status = Some(RecurringPayment.RecurringCardPaymentStatus.ENDED)
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
      }
      withHeaders(
        Delete(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$cardRoute?cardId=$cardId")
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val cards = loadCards()
        val card = cards.find(_.id == cardId)
        assert(card.map(_.firstName).getOrElse("") == firstName)
        assert(card.map(_.lastName).getOrElse("") == lastName)
        assert(card.map(_.birthday).getOrElse("") == birthday)
        assert(card.exists(!_.getActive))
      }
      loadCards()
        .filter(_.getActive)
        .foreach(card =>
          withHeaders(
            Delete(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$cardRoute?cardId=${card.id}")
          ) ~> routes ~> check {
            status shouldEqual StatusCodes.OK
          }
        )
    }

    "pre register payment method" in {
      createNewSession(customerSession)
      val command =
        PreRegisterPaymentMethod(
          orderUuid,
          naturalUser,
          paymentType = Transaction.PaymentType.CARD
        )
      log.info(s"pre register payment method command: ${serialization.write(command)}")
      withHeaders(
        Post(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$paymentMethodRoute", command)
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        preRegistration = responseAs[PreRegistration]
        log.info(s"pre registration: ${serialization.write(preRegistration)}")
      }
    }

    "pre authorize payment method" in {
      val payment =
        Payment(
          orderUuid,
          debitedAmount,
          "EUR",
          Some(preRegistration.id),
          preRegistration.registrationData,
          registerCard = true,
          printReceipt = true,
          feesAmount = Some(feesAmount),
          user = Option(naturalUser)
        )
      log.info(s"pre authorize payment: ${serialization.write(payment)}")
      withHeaders(
        Post(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$preAuthorizeRoute", payment)
          .withHeaders(`X-Forwarded-For`(RemoteAddress(InetAddress.getLocalHost)))
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
        preAuthorizationId = params.getOrElse("preAuthorizationId", "")
        assert(params.getOrElse("printReceipt", "") == "true")
      }
    }

    "pre authorize payment method callback" in {
      Get(
        s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$callbacksRoute/$preAuthorizeRoute/$orderUuid?preAuthorizationId=$preAuthorizationId&registerMeansOfPayment=true&printReceipt=true"
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val paymentAccount = loadPaymentAccount()
        log.info(serialization.write(paymentAccount))
        assert(paymentAccount.cards.nonEmpty)
      }
    }

    "load payment methods" in {
      loadPaymentMethods().cards.find(_.active) match {
        case Some(card) =>
          assert(card.firstName == firstName)
          assert(card.lastName == lastName)
          assert(card.birthday == birthday)
          assert(card.active)
          assert(!card.expired)
          cardId = card.id
        case _ => fail("no card found")
      }
    }

    "disable payment method" in {
      createNewSession(customerSession)
      withHeaders(
        Delete(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$paymentMethodRoute?paymentMethodId=$cardId"
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        loadPaymentMethods().cards.find(_.id == cardId) match {
          case Some(card) =>
            assert(card.firstName == firstName)
            assert(card.lastName == lastName)
            assert(card.birthday == birthday)
            assert(!card.active)
            assert(!card.expired)
          case _ => fail("no card found")
        }
      }
    }

  }

}
