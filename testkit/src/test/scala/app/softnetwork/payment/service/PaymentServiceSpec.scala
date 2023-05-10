package app.softnetwork.payment.service

import akka.http.scaladsl.model.StatusCodes
import app.softnetwork.api.server.config.ServerSettings.RootPath
import app.softnetwork.payment.api.PaymentClient
import app.softnetwork.payment.data._
import app.softnetwork.payment.config.PaymentSettings._
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model._
import app.softnetwork.payment.scalatest.PaymentRouteTestKit
import app.softnetwork.time._
import app.softnetwork.persistence.now
import org.scalatest.wordspec.AnyWordSpecLike
import org.slf4j.{Logger, LoggerFactory}

import java.net.URLEncoder
import java.time.LocalDate
import scala.language.implicitConversions
import scala.util.{Failure, Success}

class PaymentServiceSpec extends AnyWordSpecLike with PaymentRouteTestKit {

  lazy val log: Logger = LoggerFactory getLogger getClass.getName

  import app.softnetwork.serialization._

  lazy val client: PaymentClient = PaymentClient(typedSystem())

  "Payment service" must {
    "pre register card" in {
      createSession(customerUuid, Some("customer"))
      withCookies(
        Post(
          s"/$RootPath/$PaymentPath/$CardRoute",
          PreRegisterCard(
            orderUuid,
            naturalUser
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        cardPreRegistration = responseAs[CardPreRegistration]
      }
    }

    "pre authorize card" in {
      withCookies(
        Post(
          s"/$RootPath/$PaymentPath/$PreAuthorizeCardRoute",
          Payment(
            orderUuid,
            5100,
            "EUR",
            Some(cardPreRegistration.id),
            Some(cardPreRegistration.preregistrationData),
            registerCard = true,
            printReceipt = true
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
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

    "card pre authorization with 3ds" in {
      Get(
        s"/$RootPath/$PaymentPath/$SecureModeRoute/$PreAuthorizeCardRoute/$orderUuid?preAuthorizationId=$preAuthorizationId&registerCard=true&printReceipt=true"
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

    "not create bank account with wrong iban" in {
      createSession(sellerUuid, Some("seller"))
      withCookies(
        Post(
          s"/$RootPath/$PaymentPath/$BankRoute",
          BankAccountCommand(
            BankAccount(None, ownerName, ownerAddress, "", bic),
            naturalUser,
            None
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        assert(responseAs[PaymentError].message == WrongIban.message)
      }
    }

    "not create bank account with wrong bic" in {
      withCookies(
        Post(
          s"/$RootPath/$PaymentPath/$BankRoute",
          BankAccountCommand(
            BankAccount(None, ownerName, ownerAddress, iban, "WRONG"),
            naturalUser,
            None
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        assert(responseAs[PaymentError].message == WrongBic.message)
      }
    }

    "create bank account with natural user" in {
      withCookies(
        Post(
          s"/$RootPath/$PaymentPath/$BankRoute",
          BankAccountCommand(
            BankAccount(None, ownerName, ownerAddress, iban, bic),
            naturalUser,
            None
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val bankAccount = loadBankAccount()
        sellerBankAccountId = bankAccount.bankAccountId
      }
    }

    "update bank account with natural user" in {
      withCookies(
        Post(
          s"/$RootPath/$PaymentPath/$BankRoute",
          BankAccountCommand(
            BankAccount(Some(sellerBankAccountId), ownerName, ownerAddress, iban, bic),
            naturalUser.withLastName("anotherLastName"),
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
      withCookies(
        Post(
          s"/$RootPath/$PaymentPath/$BankRoute",
          BankAccountCommand(
            BankAccount(
              Some(sellerBankAccountId),
              ownerName,
              ownerAddress,
              iban,
              bic
            ),
            legalUser.withSiret(""),
            None
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        assert(responseAs[PaymentError].message == WrongSiret.message)
      }
    }

    "not update bank account with empty legal name" in {
      withCookies(
        Post(
          s"/$RootPath/$PaymentPath/$BankRoute",
          BankAccountCommand(
            BankAccount(
              Some(sellerBankAccountId),
              ownerName,
              ownerAddress,
              iban,
              bic
            ),
            legalUser.withLegalName(""),
            None
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        assert(responseAs[PaymentError].message == LegalNameRequired.message)
      }
    }

    "not update bank account without accepted terms of PSP" in {
      withCookies(
        Post(
          s"/$RootPath/$PaymentPath/$BankRoute",
          BankAccountCommand(
            BankAccount(
              Some(sellerBankAccountId),
              ownerName,
              ownerAddress,
              iban,
              bic
            ),
            legalUser,
            None
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        assert(responseAs[PaymentError].message == AcceptedTermsOfPSPRequired.message)
      }
    }

    "update bank account with sole trader legal user" in {
      withCookies(
        Post(
          s"/$RootPath/$PaymentPath/$BankRoute",
          BankAccountCommand(
            BankAccount(
              Some(sellerBankAccountId),
              ownerName,
              ownerAddress,
              iban,
              bic
            ),
            legalUser,
            Some(true)
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

    "update bank account with business legal user" in {
      val bank =
        BankAccountCommand(
          BankAccount(
            Some(sellerBankAccountId),
            ownerName,
            ownerAddress,
            iban,
            bic
          ),
          legalUser.withLegalUserType(LegalUser.LegalUserType.BUSINESS),
          Some(true)
        )
      log.info(serialization.write(bank))
      withCookies(
        Post(s"/$RootPath/$PaymentPath/$BankRoute", bank)
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
      withCookies(
        Post(s"/$RootPath/$PaymentPath/$DeclarationRoute", ubo)
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val declaration = loadDeclaration()
        assert(declaration.ubos.size == 1)
        uboDeclarationId = declaration.uboDeclarationId
      }
    }

    "ask for declaration validation" in {
      withCookies(
        Put(s"/$RootPath/$PaymentPath/$DeclarationRoute")
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
        s"/$RootPath/$PaymentPath/$HooksRoute?EventType=UBO_DECLARATION_VALIDATED&RessourceId=$uboDeclarationId"
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val declaration = loadDeclaration()
        assert(declaration.status == UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_VALIDATED)
      }
    }

    "pay in / out with pre authorized card" in {
      createSession(customerUuid, Some("customer"))
      withCookies(
        Post(
          s"/$RootPath/$PaymentPath/$PreAuthorizeCardRoute",
          Payment(
            orderUuid,
            100,
            "EUR",
            Some(cardPreRegistration.id),
            Some(cardPreRegistration.preregistrationData),
            registerCard = true
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        preAuthorizationId = responseAs[CardPreAuthorized].transactionId
        client.payInWithCardPreAuthorized(
          preAuthorizationId,
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          None
        ) complete () match {
          case Success(result) =>
            assert(result.transactionId.isDefined)
            assert(result.error.isEmpty)
            client.payOut(
              orderUuid,
              computeExternalUuidWithProfile(sellerUuid, Some("seller")),
              100,
              0,
              "EUR"
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
      createSession(customerUuid, Some("customer"))
      withCookies(
        Post(
          s"/$RootPath/$PaymentPath/$PayInRoute/${URLEncoder
            .encode(computeExternalUuidWithProfile(sellerUuid, Some("seller")), "UTF-8")}",
          Payment(
            orderUuid,
            5100,
            "EUR",
            Some(cardPreRegistration.id),
            Some(cardPreRegistration.preregistrationData),
            registerCard = true,
            printReceipt = true
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val redirection = responseAs[PaymentRedirection]
        val params = redirection.redirectUrl
          .split("\\?")
          .last
          .split("[&=]")
          .grouped(2)
          .map(a => (a(0), a(1)))
          .toMap
        val transactionId = params.getOrElse("transactionId", "")
        val registerCard = params.getOrElse("registerCard", "")
        assert(registerCard == "true")
        val printReceipt = params.getOrElse("printReceipt", "")
        assert(printReceipt == "true")
        Get(
          s"/$RootPath/$PaymentPath/$SecureModeRoute/$PayInRoute/$orderUuid?transactionId=$transactionId&registerCard=$registerCard&printReceipt=$printReceipt"
        ) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          assert(responseAs[PaidIn].transactionId == transactionId)
          client.payOut(
            orderUuid,
            computeExternalUuidWithProfile(sellerUuid, Some("seller")),
            5100,
            0,
            "EUR"
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
      createSession(sellerUuid, Some("seller"))
      withCookies(
        Post(s"/$RootPath/$PaymentPath/$MandateRoute")
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        assert(loadPaymentAccount().bankAccount.flatMap(_.mandateId).isDefined)
        assert(loadPaymentAccount().bankAccount.flatMap(_.mandateStatus).isDefined)
      }
    }

    val probe = createTestProbe[PaymentResult]()
    subscribeProbe(probe)

    "register recurring direct debit payment" in {
      withCookies(
        Post(
          s"/$RootPath/$PaymentPath/$RecurringPaymentRoute",
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
        withCookies(
          Get(s"/$RootPath/$PaymentPath/$RecurringPaymentRoute/$recurringPaymentRegistrationId")
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
      withCookies(
        Delete(s"/$RootPath/$PaymentPath/$MandateRoute")
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
      probe.expectMessageType[Schedule4PaymentTriggered]
      withCookies(
        Get(s"/$RootPath/$PaymentPath/$RecurringPaymentRoute/$recurringPaymentRegistrationId")
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
      createSession(customerUuid, Some("customer"))
      withCookies(
        Post(
          s"/$RootPath/$PaymentPath/$RecurringPaymentRoute",
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
        withCookies(
          Get(s"/$RootPath/$PaymentPath/$RecurringPaymentRoute/$recurringPaymentRegistrationId")
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
      withCookies(
        Post(
          s"/$RootPath/$PaymentPath/$RecurringPaymentRoute/${URLEncoder
            .encode(recurringPaymentRegistrationId, "UTF-8")}",
          Payment(
            "",
            0
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        withCookies(
          Get(s"/$RootPath/$PaymentPath/$RecurringPaymentRoute/$recurringPaymentRegistrationId")
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
      createSession(sellerUuid, Some("seller"))
      withCookies(
        Delete(s"/$RootPath/$PaymentPath/$MandateRoute")
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
        s"/$RootPath/$PaymentPath/$HooksRoute?EventType=USER_KYC_LIGHT&RessourceId=$userId"
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        assert(loadPaymentAccount().paymentAccountStatus.isDocumentsKo)
      }
    }

    "delete bank account" in {
      withCookies(
        Delete(s"/$RootPath/$PaymentPath/$BankRoute")
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        assert(loadPaymentAccount().bankAccount.isEmpty)
      }
    }

    "disable card" in {
      createSession(customerUuid, Some("customer"))
      withCookies(
        Delete(s"/$RootPath/$PaymentPath/$CardRoute?cardId=$cardId")
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
      withCookies(
        Put(
          s"/$RootPath/$PaymentPath/$RecurringPaymentRoute",
          UpdateRecurringCardPaymentRegistration(
            "",
            recurringPaymentRegistrationId,
            status = Some(RecurringPayment.RecurringCardPaymentStatus.ENDED)
          )
        )
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
      }
      withCookies(
        Delete(s"/$RootPath/$PaymentPath/$CardRoute?cardId=$cardId")
      ) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val cards = loadCards()
        val card = cards.find(_.id == cardId)
        assert(card.map(_.firstName).getOrElse("") == firstName)
        assert(card.map(_.lastName).getOrElse("") == lastName)
        assert(card.map(_.birthday).getOrElse("") == birthday)
        assert(card.exists(!_.getActive))
      }
    }

  }

}
