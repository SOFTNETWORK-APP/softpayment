package app.softnetwork.payment.handlers

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.api.{PaymentClient, PaymentGrpcServerTestKit}
import app.softnetwork.payment.data._
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.PaymentAccount.User
import app.softnetwork.payment.model._
import app.softnetwork.payment.scalatest.PostgresPaymentTestKit
import app.softnetwork.time._
import app.softnetwork.persistence.now
import app.softnetwork.session.config.Settings
import org.scalatest.wordspec.AnyWordSpecLike
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.Session

import java.time.LocalDate
import scala.language.implicitConversions
import scala.util.{Failure, Success}

class PaymentHandlerSpec
    extends MockPaymentHandler
    with AnyWordSpecLike
    with PostgresPaymentTestKit
    with PaymentGrpcServerTestKit {

  override lazy val log: Logger = LoggerFactory getLogger getClass.getName

  implicit lazy val ts: ActorSystem[_] = typedSystem()

  override protected def sessionType: Session.SessionType =
    Settings.Session.SessionContinuityAndTransport

  lazy val paymentClient: PaymentClient = PaymentClient(ts)

  val currency = "EUR"

  "Payment handler" must {
    "pre register card" in {
      !?(
        PreRegisterPaymentMethod(
          orderUuid,
          naturalUser.withProfile("customer"),
          paymentType = Transaction.PaymentType.CARD,
          clientId = Some(clientId)
        )
      ) await {
        case preRegistered: PaymentMethodPreRegistered =>
          preRegistration = preRegistered.preRegistration
          !?(
            LoadPaymentAccount(computeExternalUuidWithProfile(customerUuid, Some("customer")))
          ) await {
            case result: PaymentAccountLoaded =>
              val paymentAccount = result.paymentAccount
              val naturalUser = paymentAccount.getNaturalUser
              assert(naturalUser.externalUuid == customerUuid)
              assert(naturalUser.firstName == firstName)
              assert(naturalUser.lastName == lastName)
              assert(naturalUser.birthday == birthday)
              assert(naturalUser.email == email)
              assert(naturalUser.userId.isDefined)
              assert(naturalUser.walletId.isDefined)
              assert(
                naturalUser.naturalUserType.getOrElse(NaturalUser.NaturalUserType.COLLECTOR).isPayer
              )
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "pre authorize card" in {
      !?(
        PreAuthorize(
          orderUuid,
          computeExternalUuidWithProfile(customerUuid, Some("customer")),
          5100,
          currency,
          Some(preRegistration.id),
          Some(preRegistration.registrationData),
          registerCard = true,
          creditedAccount = Some(computeExternalUuidWithProfile(sellerUuid, Some("seller")))
        )
      ) await {
        case result: PaymentRedirection =>
          val params = result.redirectUrl
            .split("\\?")
            .last
            .split("[&=]")
            .grouped(2)
            .map(a => (a(0), a(1)))
            .toMap
          preAuthorizationId = params.getOrElse("preAuthorizationId", "")
          transactionProbe.receiveMessage()
          jdbcTransactionProvider.load(preAuthorizationId) match {
            case Some(transaction) =>
              transaction.orderUuid shouldBe orderUuid
              transaction.preRegistrationId.getOrElse("") shouldBe preRegistration.id
              transaction.`type` shouldBe Transaction.TransactionType.PRE_AUTHORIZATION
              transaction.paymentType shouldBe Transaction.PaymentType.CARD
              transaction.currency shouldBe currency
              transaction.amount shouldBe 5100
              transaction.fees shouldBe 0
              transaction.status shouldBe Transaction.TransactionStatus.TRANSACTION_CREATED
            case _ => fail()
          }
        case other => fail(other.toString)
      }
    }

    "update card pre authorization" in {
      !?(
        PreAuthorizeCallback(
          orderUuid,
          preAuthorizationId
        )
      ) await {
        case cardPreAuthorized: PaymentPreAuthorized =>
          val transactionId = cardPreAuthorized.transactionId
          transactionProbe.receiveMessage()
          jdbcTransactionProvider.load(transactionId) match {
            case Some(transaction) =>
              transaction.orderUuid shouldBe orderUuid
              transaction.preRegistrationId.getOrElse("") shouldBe preRegistration.id
              transaction.`type` shouldBe Transaction.TransactionType.PRE_AUTHORIZATION
              transaction.paymentType shouldBe Transaction.PaymentType.CARD
              transaction.preAuthorizationId.getOrElse("") shouldBe preAuthorizationId
              transaction.currency shouldBe currency
              transaction.amount shouldBe 5100
              transaction.fees shouldBe 0
              transaction.status shouldBe Transaction.TransactionStatus.TRANSACTION_CREATED
            case _ => fail()
          }
          !?(
            LoadPaymentAccount(computeExternalUuidWithProfile(customerUuid, Some("customer")))
          ) await {
            case result: PaymentAccountLoaded =>
              val paymentAccount = result.paymentAccount
              assert(paymentAccount.transactions.exists(t => t.id == transactionId))
              assert(paymentAccount.cards.size == 1)
              cardId = paymentAccount.cards.head.id
              assert(paymentAccount.cards.map(_.firstName).head == firstName)
              assert(paymentAccount.cards.map(_.lastName).head == lastName)
              assert(paymentAccount.cards.map(_.birthday).head == birthday)
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "load cards" in {
      !?(LoadPaymentMethods(computeExternalUuidWithProfile(customerUuid, Some("customer")))) await {
        case result: PaymentMethodsLoaded =>
          val card = PaymentMethodsView(result.paymentMethods).cards.find(_.id == cardId)
          assert(card.isDefined)
          assert(card.map(_.firstName).getOrElse("") == firstName)
          assert(card.map(_.lastName).getOrElse("") == lastName)
          assert(card.map(_.birthday).getOrElse("") == birthday)
          assert(card.exists(_.active))
        case other => fail(other.toString)
      }
    }

    "pre authorize without card pre registration" in {
      // TODO
    }

    "not create bank account with wrong iban" in {
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          BankAccount(None, ownerName, ownerAddress, "", bic),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case WrongIban =>
        case other     => fail(other.toString)
      }
    }

    "not create bank account with wrong bic" in {
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          BankAccount(None, ownerName, ownerAddress, iban, "WRONG"),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case WrongBic =>
        case other    => fail(other.toString)
      }
    }

    "create bank account with natural user and empty bic" in {
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          BankAccount(None, ownerName, ownerAddress, iban, ""),
          Some(User.NaturalUser(naturalUser.withExternalUuid(sellerUuid).withProfile("seller"))),
          clientId = Some(clientId),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case r: BankAccountCreatedOrUpdated =>
          assert(r.userCreated)
          !?(LoadPaymentAccount(computeExternalUuidWithProfile(sellerUuid, Some("seller")))) await {
            case result: PaymentAccountLoaded =>
              val paymentAccount = result.paymentAccount
              assert(paymentAccount.bankAccount.isDefined)
              assert(paymentAccount.documents.size == 1)
              assert(
                paymentAccount.documents.exists(
                  _.`type` == KycDocument.KycDocumentType.KYC_IDENTITY_PROOF
                )
              )
              sellerBankAccountId = paymentAccount.bankAccount.flatMap(_.id).getOrElse("")
              assert(
                paymentAccount.getNaturalUser.naturalUserType
                  .getOrElse(NaturalUser.NaturalUserType.PAYER)
                  .isCollector
              )
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "update bank account with natural user" in {
      // update first name
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          BankAccount(
            Some(sellerBankAccountId),
            ownerName,
            ownerAddress,
            iban,
            bic
          ),
          Some(
            User.NaturalUser(
              naturalUser
                .withFirstName("anotherFirstName")
                .withExternalUuid(sellerUuid)
                .withProfile("seller")
            )
          ),
          clientId = Some(clientId),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case r: BankAccountCreatedOrUpdated =>
          assert(r.kycUpdated && r.userUpdated && r.documentsUpdated)
          !?(LoadPaymentAccount(computeExternalUuidWithProfile(sellerUuid, Some("seller")))) await {
            case result: PaymentAccountLoaded =>
              val paymentAccount = result.paymentAccount
              assert(paymentAccount.bankAccount.isDefined)
              assert(paymentAccount.documents.size == 1)
              assert(
                paymentAccount.documents.exists(
                  _.`type` == KycDocument.KycDocumentType.KYC_IDENTITY_PROOF
                )
              )
//              val previousBankAccountId = sellerBankAccountId
              sellerBankAccountId = paymentAccount.bankAccount.flatMap(_.id).getOrElse("")
//              assert(sellerBankAccountId != previousBankAccountId)
              assert(
                paymentAccount.getNaturalUser.naturalUserType
                  .getOrElse(NaturalUser.NaturalUserType.PAYER)
                  .isCollector
              )
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
      // update last name
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          BankAccount(
            Some(sellerBankAccountId),
            ownerName,
            ownerAddress,
            iban,
            bic
          ),
          Some(
            User.NaturalUser(
              naturalUser
                .withFirstName("anotherFirstName")
                .withLastName("anotherLastName")
                .withExternalUuid(sellerUuid)
                .withProfile("seller")
            )
          ),
          clientId = Some(clientId),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case r: BankAccountCreatedOrUpdated =>
          assert(r.kycUpdated && r.userUpdated && r.documentsUpdated)
        case other => fail(other.toString)
      }
      // update birthday
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          BankAccount(
            Some(sellerBankAccountId),
            ownerName,
            ownerAddress,
            iban,
            bic
          ),
          Some(
            User.NaturalUser(
              naturalUser
                .withFirstName("anotherFirstName")
                .withLastName("anotherLastName")
                .withBirthday("01/01/1980")
                .withExternalUuid(sellerUuid)
                .withProfile("seller")
            )
          ),
          clientId = Some(clientId),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case r: BankAccountCreatedOrUpdated =>
          assert(r.kycUpdated && r.userUpdated && r.documentsUpdated)
        case other => fail(other.toString)
      }
    }

    "update bank account except kyc information with natural user" in {
      // update country of residence
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          BankAccount(
            Some(sellerBankAccountId),
            ownerName,
            ownerAddress,
            iban,
            bic
          ),
          Some(
            User.NaturalUser(
              naturalUser
                .withFirstName("anotherFirstName")
                .withLastName("anotherLastName")
                .withBirthday("01/01/1980")
                .withCountryOfResidence("GA")
                .withExternalUuid(sellerUuid)
                .withProfile("seller")
            )
          ),
          clientId = Some(clientId),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await { case r: BankAccountCreatedOrUpdated =>
        assert(!r.kycUpdated && !r.documentsUpdated && r.userUpdated)
      }
      // update nationality
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          BankAccount(
            Some(sellerBankAccountId),
            ownerName,
            ownerAddress,
            iban,
            bic
          ),
          Some(
            User.NaturalUser(
              naturalUser
                .withFirstName("anotherFirstName")
                .withLastName("anotherLastName")
                .withBirthday("01/01/1980")
                .withCountryOfResidence("GA")
                .withNationality("GA")
                .withExternalUuid(sellerUuid)
                .withProfile("seller")
            )
          ),
          clientId = Some(clientId),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await { case r: BankAccountCreatedOrUpdated =>
        assert(!r.kycUpdated && !r.documentsUpdated && r.userUpdated)
      }
    }

    "not update bank account with wrong siret" in {
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          BankAccount(
            Some(sellerBankAccountId),
            ownerName,
            ownerAddress,
            iban,
            bic
          ),
          Some(User.LegalUser(legalUser.withSiret(""))),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case WrongSiret =>
        case other      => fail(other.toString)
      }
    }

    "not update bank account with empty legal name" in {
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          BankAccount(
            Some(sellerBankAccountId),
            ownerName,
            ownerAddress,
            iban,
            bic
          ),
          Some(User.LegalUser(legalUser.withLegalName(""))),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case LegalNameRequired =>
        case other             => fail(other.toString)
      }
    }

    "not update bank account without accepted terms of PSP" in {
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          BankAccount(
            Some(sellerBankAccountId),
            ownerName,
            ownerAddress,
            iban,
            bic
          ),
          Some(User.LegalUser(legalUser)),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case AcceptedTermsOfPSPRequired =>
        case other                      => fail(other.toString)
      }
    }

    "update bank account with sole trader legal user" in {
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          BankAccount(
            Some(sellerBankAccountId),
            ownerName,
            ownerAddress,
            iban,
            bic
          ),
          Some(
            User.LegalUser(
              legalUser.withLegalRepresentative(legalUser.legalRepresentative.withProfile("seller"))
            )
          ),
          Some(true),
          clientId = Some(clientId),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case r: BankAccountCreatedOrUpdated =>
          assert(r.userTypeUpdated && r.kycUpdated && r.documentsUpdated && r.userUpdated)
          !?(LoadPaymentAccount(computeExternalUuidWithProfile(sellerUuid, Some("seller")))) await {
            case result: PaymentAccountLoaded =>
              val paymentAccount = result.paymentAccount
              assert(paymentAccount.bankAccount.isDefined)
              assert(paymentAccount.documents.size == 2)
              assert(
                paymentAccount.documents.exists(
                  _.`type` == KycDocument.KycDocumentType.KYC_IDENTITY_PROOF
                )
              )
              assert(
                paymentAccount.documents.exists(
                  _.`type` == KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF
                )
              )
//              val previousBankAccountId = sellerBankAccountId
              sellerBankAccountId = paymentAccount.bankAccount.flatMap(_.id).getOrElse("")
//              assert(sellerBankAccountId != previousBankAccountId)
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "update bank account with business legal user" in {
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          BankAccount(
            Some(sellerBankAccountId),
            ownerName,
            ownerAddress,
            iban,
            bic
          ),
          Some(
            User.LegalUser(
              legalUser
                .withLegalUserType(LegalUser.LegalUserType.BUSINESS)
                .withLegalRepresentative(
                  legalUser.legalRepresentative
                    .withProfile("seller")
                )
            )
          ),
          Some(true),
          clientId = Some(clientId),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case r: BankAccountCreatedOrUpdated =>
          assert(r.userTypeUpdated && r.kycUpdated && r.documentsUpdated && r.userUpdated)
          !?(LoadPaymentAccount(computeExternalUuidWithProfile(sellerUuid, Some("seller")))) await {
            case result: PaymentAccountLoaded =>
              val paymentAccount = result.paymentAccount
              assert(paymentAccount.bankAccount.isDefined)
              assert(paymentAccount.documents.size == 4)
              assert(
                paymentAccount.documents.exists(
                  _.`type` == KycDocument.KycDocumentType.KYC_IDENTITY_PROOF
                )
              )
              assert(
                paymentAccount.documents.exists(
                  _.`type` == KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF
                )
              )
              assert(
                paymentAccount.documents.exists(
                  _.`type` == KycDocument.KycDocumentType.KYC_ARTICLES_OF_ASSOCIATION
                )
              )
              assert(
                paymentAccount.documents.exists(
                  _.`type` == KycDocument.KycDocumentType.KYC_SHAREHOLDER_DECLARATION
                )
              )
              assert(paymentAccount.getLegalUser.uboDeclarationRequired)
              assert(paymentAccount.getLegalUser.uboDeclaration.map(_.id).isDefined)
//              val previousBankAccountId = sellerBankAccountId
              sellerBankAccountId = paymentAccount.bankAccount.flatMap(_.id).getOrElse("")
//              assert(sellerBankAccountId != previousBankAccountId)
              uboDeclarationId = paymentAccount.getLegalUser.uboDeclaration.map(_.id).getOrElse("")
              paymentClient.loadLegalUserDetails(
                computeExternalUuidWithProfile(sellerUuid, Some("seller"))
              ) complete () match {
                case Success(value) =>
                  assert(value.legalUserType.isBusiness)
                  assert(value.legalName == legalUser.legalName)
                  assert(value.siret == legalUser.siret)
                  assert(value.legalRepresentativeAddress.isDefined)
                  assert(value.headQuartersAddress.isDefined)
                case Failure(f) => fail(f.getMessage)
              }
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "update bank account except kyc information with business legal user" in {
      var updatedBankAccount =
        BankAccount(
          Some(sellerBankAccountId),
          ownerName,
          ownerAddress,
          iban,
          bic
        )
      var updatedLegalUser =
        legalUser
          .withLegalUserType(LegalUser.LegalUserType.BUSINESS)
          .withLegalRepresentative(
            legalUser.legalRepresentative
              .withProfile("seller")
          )
      // update bank account owner name
      updatedBankAccount = updatedBankAccount.withOwnerName("anotherOwnerName")
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          updatedBankAccount,
          Some(User.LegalUser(updatedLegalUser)),
          Some(true),
          clientId = Some(clientId),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case r: BankAccountCreatedOrUpdated =>
          assert(
            !r.userTypeUpdated && !r.kycUpdated && !r.documentsUpdated && !r.userUpdated && r.bankAccountUpdated
          )
        case other => fail(other.toString)
      }
      // update bank account owner address
      updatedBankAccount =
        updatedBankAccount.withOwnerAddress(ownerAddress.withAddressLine("anotherAddressLine"))
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          updatedBankAccount,
          Some(User.LegalUser(updatedLegalUser)),
          Some(true),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case r: BankAccountCreatedOrUpdated =>
          assert(
            !r.userTypeUpdated && !r.kycUpdated && !r.documentsUpdated && !r.userUpdated && r.bankAccountUpdated
          )
        case other => fail(other.toString)
      }
      // update bank account iban
      updatedBankAccount = updatedBankAccount.withIban("FR8914508000308185764223C20")
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          updatedBankAccount,
          Some(User.LegalUser(updatedLegalUser)),
          Some(true),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case r: BankAccountCreatedOrUpdated =>
          assert(
            !r.userTypeUpdated && !r.kycUpdated && !r.documentsUpdated && !r.userUpdated && r.bankAccountUpdated
          )
        case other => fail(other.toString)
      }
      // update bank account bic
      updatedBankAccount = updatedBankAccount.withBic("AGFBFRCC")
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          updatedBankAccount,
          Some(User.LegalUser(updatedLegalUser)),
          Some(true),
          clientId = Some(clientId),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case r: BankAccountCreatedOrUpdated =>
          assert(
            !r.userTypeUpdated && !r.kycUpdated && !r.documentsUpdated && !r.userUpdated && r.bankAccountUpdated
          )
        case other => fail(other.toString)
      }
      // update bank account with empty bic
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          updatedBankAccount.withBic(""),
          Some(User.LegalUser(updatedLegalUser)),
          Some(true),
          clientId = Some(clientId),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case r: BankAccountCreatedOrUpdated =>
          assert(
            !r.userTypeUpdated && !r.kycUpdated && !r.documentsUpdated && !r.userUpdated /*&& !r.bankAccountUpdated FIXME*/
          )
        /*assert(
            r.paymentAccount.bankAccount.map(_.bic).getOrElse("").nonEmpty
          )FIXME*/
        case other => fail(other.toString)
      }
      // update siret
      updatedLegalUser = updatedLegalUser.withSiret("12345678912345")
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          updatedBankAccount,
          Some(User.LegalUser(updatedLegalUser)),
          Some(true),
          clientId = Some(clientId),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case r: BankAccountCreatedOrUpdated =>
          assert(
            !r.userTypeUpdated && !r.documentsUpdated && r.userUpdated /*&& !r.bankAccountUpdated FIXME*/
          )
        case other => fail(other.toString)
      }
    }

    "add document(s)" in {
      Seq(
        KycDocument.KycDocumentType.KYC_IDENTITY_PROOF,
        KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF,
        KycDocument.KycDocumentType.KYC_ARTICLES_OF_ASSOCIATION,
        KycDocument.KycDocumentType.KYC_SHAREHOLDER_DECLARATION
      ).foreach { `type` =>
        !?(
          AddKycDocument(
            computeExternalUuidWithProfile(sellerUuid, Some("seller")),
            Seq.empty,
            `type`
          )
        ) await {
          case _: KycDocumentAdded =>
            !?(
              LoadPaymentAccount(computeExternalUuidWithProfile(sellerUuid, Some("seller")))
            ) await {
              case result: PaymentAccountLoaded =>
                val paymentAccount = result.paymentAccount
                assert(
                  paymentAccount.documents
                    .find(_.`type` == `type`)
                    .exists(_.status == KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATION_ASKED)
                )
              case other => fail(other.toString)
            }
          case other => fail(other.toString)
        }
      }
    }

    "update document(s) status" in {
      val validated = KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATED
      Seq(
        KycDocument.KycDocumentType.KYC_IDENTITY_PROOF,
        KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF,
        KycDocument.KycDocumentType.KYC_ARTICLES_OF_ASSOCIATION,
        KycDocument.KycDocumentType.KYC_SHAREHOLDER_DECLARATION
      ).foreach { `type` =>
        !?(
          LoadKycDocumentStatus(computeExternalUuidWithProfile(sellerUuid, Some("seller")), `type`)
        ) await {
          case result: KycDocumentStatusLoaded =>
            !?(
              UpdateKycDocumentStatus(
                result.report.id,
                Some(validated)
              )
            ) await {
              case _: KycDocumentStatusUpdated =>
                !?(
                  LoadPaymentAccount(computeExternalUuidWithProfile(sellerUuid, Some("seller")))
                ) await {
                  case result: PaymentAccountLoaded =>
                    val paymentAccount = result.paymentAccount
                    assert(
                      paymentAccount.documents
                        .find(_.`type` == `type`)
                        .exists(_.status == validated)
                    )
                  case other => fail(other.toString)
                }
              case other => fail(other.toString)
            }
          case other => fail(other.toString)
        }
      }
    }

    "create or update ultimate beneficial owner" in {
      !?(CreateOrUpdateUbo(computeExternalUuidWithProfile(sellerUuid, Some("seller")), ubo)) await {
        case _: UboCreatedOrUpdated =>
        case other                  => fail(other.toString)
      }
    }

    "ask for declaration validation" in {
      !?(
        ValidateUboDeclaration(
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          "127.0.0.1",
          Some("UserAgent"),
          None
        )
      ) await {
        case UboDeclarationAskedForValidation =>
          !?(GetUboDeclaration(computeExternalUuidWithProfile(sellerUuid, Some("seller")))) await {
            case result: UboDeclarationLoaded =>
              assert(
                result.declaration.status == UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_VALIDATION_ASKED
              )
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "update declaration status" in {
      !?(
        UpdateUboDeclarationStatus(
          uboDeclarationId,
          Some(UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_VALIDATED)
        )
      ) await {
        case UboDeclarationStatusUpdated =>
          !?(GetUboDeclaration(computeExternalUuidWithProfile(sellerUuid, Some("seller")))) await {
            case result: UboDeclarationLoaded =>
              assert(
                result.declaration.status == UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_VALIDATED
              )
              !?(
                LoadPaymentAccount(computeExternalUuidWithProfile(sellerUuid, Some("seller")))
              ) await {
                case result: PaymentAccountLoaded =>
                  val paymentAccount = result.paymentAccount
                  assert(
                    paymentAccount.paymentAccountStatus == PaymentAccount.PaymentAccountStatus.COMPTE_OK
                  )
                case other => fail(other.toString)
              }
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "cancel pre authorized card" in {
      !?(
        PreAuthorize(
          orderUuid,
          computeExternalUuidWithProfile(customerUuid, Some("customer")),
          100,
          currency,
          Some(preRegistration.id),
          Some(preRegistration.registrationData),
          registerCard = true,
          creditedAccount = Some(computeExternalUuidWithProfile(sellerUuid, Some("seller")))
        )
      ) await {
        case result: PaymentPreAuthorized =>
          val transactionId = result.transactionId
          preAuthorizationId = transactionId
          transactionProbe.receiveMessage()
          jdbcTransactionProvider.load(preAuthorizationId) match {
            case Some(transaction) =>
              transaction.orderUuid shouldBe orderUuid
              transaction.preRegistrationId.getOrElse("") shouldBe preRegistration.id
              transaction.`type` shouldBe Transaction.TransactionType.PRE_AUTHORIZATION
              transaction.paymentType shouldBe Transaction.PaymentType.CARD
              transaction.currency shouldBe currency
              transaction.amount shouldBe 100
              transaction.fees shouldBe 0
              transaction.status shouldBe Transaction.TransactionStatus.TRANSACTION_CREATED
            case _ => fail()
          }
          !?(CancelPreAuthorization(orderUuid, preAuthorizationId)) await {
            case result: PreAuthorizationCanceled =>
              assert(result.preAuthorizationCanceled)
              transactionProbe.receiveMessage()
              jdbcTransactionProvider.load(preAuthorizationId) match {
                case Some(transaction) =>
                  transaction.orderUuid shouldBe orderUuid
                  transaction.preRegistrationId.getOrElse("") shouldBe preRegistration.id
                  transaction.`type` shouldBe Transaction.TransactionType.PRE_AUTHORIZATION
                  transaction.paymentType shouldBe Transaction.PaymentType.CARD
                  transaction.currency shouldBe currency
                  transaction.amount shouldBe 100
                  transaction.fees shouldBe 0
                  transaction.status shouldBe Transaction.TransactionStatus.TRANSACTION_CREATED
                  transaction.preAuthorizationCanceled.getOrElse(false) shouldBe true
                case _ => fail()
              }
            case other => fail(other.getClass)
          }
        case other => fail(other.getClass)
      }
    }

    "pay in / out with pre authorized card" in {
      !?(
        PreAuthorize(
          orderUuid,
          computeExternalUuidWithProfile(customerUuid, Some("customer")),
          100,
          currency,
          Some(preRegistration.id),
          Some(preRegistration.registrationData),
          registerCard = true,
          creditedAccount = Some(computeExternalUuidWithProfile(sellerUuid, Some("seller")))
        )
      ) await {
        case result: PaymentPreAuthorized =>
          var transactionId = result.transactionId
          preAuthorizationId = transactionId
          log.info(s"$transactionId")
          transactionProbe.receiveMessage()
          jdbcTransactionProvider.load(preAuthorizationId) match {
            case Some(transaction) =>
              transaction.orderUuid shouldBe orderUuid
              transaction.preRegistrationId.getOrElse("") shouldBe preRegistration.id
              transaction.`type` shouldBe Transaction.TransactionType.PRE_AUTHORIZATION
              transaction.paymentType shouldBe Transaction.PaymentType.CARD
              transaction.currency shouldBe currency
              transaction.amount shouldBe 100
              transaction.fees shouldBe 0
              transaction.status shouldBe Transaction.TransactionStatus.TRANSACTION_CREATED
            case _ => fail()
          }
          paymentClient.payInWithPreAuthorization(
            preAuthorizationId,
            computeExternalUuidWithProfile(sellerUuid, Some("seller")),
            Some(110)
          ) complete () match {
            case Success(result) =>
              assert(result.transactionId.isEmpty)
              assert(result.error.getOrElse("") == "DebitedAmountAbovePreAuthorizationAmount")
            case Failure(f) => fail(f.getMessage)
          }
          val debitedAmount = 90
          paymentClient.payInWithPreAuthorization(
            preAuthorizationId,
            computeExternalUuidWithProfile(sellerUuid, Some("seller")),
            Some(debitedAmount)
          ) complete () match {
            case Success(result) =>
              transactionId = result.transactionId.getOrElse("")
              assert(transactionId.nonEmpty)
              assert(result.error.isEmpty)
              !?(
                LoadPaymentAccount(computeExternalUuidWithProfile(customerUuid, Some("customer")))
              ) await {
                case r: PaymentAccountLoaded =>
                  val paymentAccount = r.paymentAccount
                  assert(
                    paymentAccount.transactions
                      .find(_.id == preAuthorizationId)
                      .flatMap(_.preAuthorizationValidated)
                      .getOrElse(false)
                  )
                  transactionProbe.receiveMessage()
                  jdbcTransactionProvider.load(transactionId) match {
                    case Some(transaction) =>
                      transaction.orderUuid shouldBe orderUuid
                      transaction.preRegistrationId.getOrElse("") shouldBe preRegistration.id
                      transaction.`type` shouldBe Transaction.TransactionType.PAYIN
                      transaction.paymentType shouldBe Transaction.PaymentType.PREAUTHORIZED
                      transaction.status shouldBe Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
                      transaction.currency shouldBe currency
                      transaction.amount shouldBe 90
                      transaction.fees shouldBe 0
                      transaction.preAuthorizationId.getOrElse("") shouldBe preAuthorizationId
                      transaction.preAuthorizationDebitedAmount.getOrElse(0) shouldBe 100
                    case _ => fail()
                  }
                  transactionProbe.receiveMessage()
                  jdbcTransactionProvider.load(preAuthorizationId) match {
                    case Some(transaction) =>
                      transaction.orderUuid shouldBe orderUuid
                      transaction.preRegistrationId.getOrElse("") shouldBe preRegistration.id
                      transaction.`type` shouldBe Transaction.TransactionType.PRE_AUTHORIZATION
                      transaction.paymentType shouldBe Transaction.PaymentType.CARD
                      transaction.status shouldBe Transaction.TransactionStatus.TRANSACTION_CREATED
                      transaction.currency shouldBe currency
                      transaction.amount shouldBe 100
                      transaction.fees shouldBe 0
                      transaction.preAuthorizationValidated.getOrElse(false) shouldBe true
                    case _ => fail()
                  }
                case other => fail(other.getClass.toString)
              }
              paymentClient.loadBalance(
                currency,
                Option(computeExternalUuidWithProfile(sellerUuid, Some("seller"))),
                None
              ) complete () match {
                case Success(balance) =>
                  assert(balance.getOrElse(0) == debitedAmount)
                case Failure(f) => fail(f.getMessage)
              }
              paymentClient.payOut(
                orderUuid,
                computeExternalUuidWithProfile(sellerUuid, Some("seller")),
                100,
                0,
                currency,
                None,
                result.transactionId
              ) complete () match {
                case Success(s) =>
                  transactionId = s.transactionId.getOrElse("")
                  assert(transactionId.nonEmpty)
                  assert(s.error.isEmpty)
                  transactionProbe.receiveMessage()
                  jdbcTransactionProvider.load(transactionId) match {
                    case Some(transaction) =>
                      transaction.orderUuid shouldBe orderUuid
                      transaction.`type` shouldBe Transaction.TransactionType.PAYOUT
                      transaction.paymentType shouldBe Transaction.PaymentType.BANK_WIRE
                      transaction.status shouldBe Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
                      transaction.currency shouldBe currency
                      transaction.amount shouldBe 100
                      transaction.fees shouldBe 0
                      transaction.payInId shouldBe result.transactionId
                    case _ => fail()
                  }
                case Failure(f) => fail(f.getMessage)
              }
            case Failure(f) => fail(f.getMessage)
          }
        case other => fail(other.toString)
      }
    }

    "pay in / out with Card" in {
      !?(
        PayIn(
          orderUuid,
          computeExternalUuidWithProfile(customerUuid, Some("customer")),
          100,
          currency,
          computeExternalUuidWithProfile(sellerUuid, Some("seller"))
        )
      ) await {
        case result: PaidIn =>
          paymentClient.payOut(
            orderUuid,
            computeExternalUuidWithProfile(sellerUuid, Some("seller")),
            100,
            0,
            currency,
            None,
            Option(result.transactionId)
          ) complete () match {
            case Success(s) =>
              val transactionId = s.transactionId
              assert(transactionId.isDefined)
              assert(s.error.isEmpty)
            case Failure(f) => fail(f.getMessage)
          }
        case other => fail(other.toString)
      }
    }

    "pay in / out with PayPal" in {
      !?(
        PayIn(
          orderUuid,
          computeExternalUuidWithProfile(customerUuid, Some("customer")),
          100,
          currency,
          computeExternalUuidWithProfile(sellerUuid, Some("seller")),
          paymentType = Transaction.PaymentType.PAYPAL,
          printReceipt = true
        )
      ) await {
        case r: PaymentRedirection =>
          val params = r.redirectUrl
            .split("\\?")
            .last
            .split("[&=]")
            .grouped(2)
            .map(a => (a(0), a(1)))
            .toMap
          val transactionId = params.getOrElse("transactionId", "")
          val printReceipt = params.getOrElse("printReceipt", "")
          assert(printReceipt == "true")
          !?(
            PayInCallback(
              orderUuid,
              transactionId,
              printReceipt.toBoolean
            )
          ) await {
            case result: PaidIn =>
              paymentClient.payOut(
                orderUuid,
                computeExternalUuidWithProfile(sellerUuid, Some("seller")),
                100,
                0,
                currency,
                None,
                Option(result.transactionId)
              ) complete () match {
                case Success(s) =>
                  assert(s.transactionId.isDefined)
                  assert(s.error.isEmpty)
                case Failure(f) => fail(f.getMessage)
              }
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "pay in / refund" in {
      !?(
        PayIn(
          orderUuid,
          computeExternalUuidWithProfile(customerUuid, Some("customer")),
          100,
          currency,
          computeExternalUuidWithProfile(sellerUuid, Some("seller"))
        )
      ) await {
        case result: PaidIn =>
          val payInTransactionId = result.transactionId
          paymentClient.refund(
            orderUuid,
            payInTransactionId,
            101,
            None,
            currency,
            "change my mind",
            initializedByClient = true
          ) complete () match {
            case Success(r) =>
              assert(r.transactionId.isEmpty)
              assert(r.error.getOrElse("") == "IllegalTransactionAmount")
              paymentClient.refund(
                orderUuid,
                payInTransactionId,
                50,
                None,
                currency,
                "change my mind",
                initializedByClient = true
              ) complete () match {
                case Success(s) =>
                  assert(s.transactionId.isDefined)
                  assert(s.error.isEmpty)
                case Failure(f) => fail(f.getMessage)
              }
            case Failure(f) => fail(f.getMessage)
          }
        case other => fail(other.toString)
      }
    }

    "transfer" in {
      !?(
        CreateOrUpdateBankAccount(
          computeExternalUuidWithProfile(vendorUuid, Some("vendor")),
          BankAccount(None, ownerName, ownerAddress, iban, bic),
          Some(User.NaturalUser(naturalUser.withExternalUuid(vendorUuid).withProfile("vendor"))),
          clientId = Some(clientId),
          ipAddress = Some("127.0.0.1"),
          userAgent = Some("UserAgent")
        )
      ) await {
        case r: BankAccountCreatedOrUpdated =>
          assert(r.userCreated)
          !?(LoadPaymentAccount(computeExternalUuidWithProfile(vendorUuid, Some("vendor")))) await {
            case result: PaymentAccountLoaded =>
              val paymentAccount = result.paymentAccount
              assert(paymentAccount.bankAccount.isDefined)
              assert(paymentAccount.documents.size == 1)
              assert(
                paymentAccount.documents.exists(
                  _.`type` == KycDocument.KycDocumentType.KYC_IDENTITY_PROOF
                )
              )
              vendorBankAccountId = paymentAccount.getBankAccount.getId
              !?(
                AddKycDocument(
                  computeExternalUuidWithProfile(vendorUuid, Some("vendor")),
                  Seq.empty,
                  KycDocument.KycDocumentType.KYC_IDENTITY_PROOF
                )
              ) await {
                case result: KycDocumentAdded =>
                  !?(
                    UpdateKycDocumentStatus(
                      result.kycDocumentId,
                      Some(KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATED)
                    )
                  ) await {
                    case _: KycDocumentStatusUpdated =>
                    case other                       => fail(other.toString)
                  }
                case other => fail(other.toString)
              }
              paymentClient.transfer(
                Some(orderUuid),
                computeExternalUuidWithProfile(sellerUuid, Some("seller")),
                computeExternalUuidWithProfile(vendorUuid, Some("vendor")),
                50,
                10,
                currency,
                payOutRequired = true,
                None
              ) complete () match {
                case Success(s) =>
                  val paidOutTransactionId = s.paidOutTransactionId
                  assert(paidOutTransactionId.isDefined)
                  assert(s.error.isEmpty)
                case Failure(f) => fail(f.getMessage)
              }
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "create mandate" in {
      !?(CreateMandate(computeExternalUuidWithProfile(vendorUuid, Some("vendor")))) await {
        case MandateCreated =>
          !?(LoadPaymentAccount(computeExternalUuidWithProfile(vendorUuid, Some("vendor")))) await {
            case result: PaymentAccountLoaded =>
              val bankAccount = result.paymentAccount.getBankAccount
              assert(bankAccount.mandateId.isDefined)
              mandateId = bankAccount.getMandateId
              assert(bankAccount.getMandateStatus == Mandate.MandateStatus.MANDATE_SUBMITTED)
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "direct debit" in {
      paymentClient.directDebit(
        computeExternalUuidWithProfile(vendorUuid, Some("vendor")),
        100,
        0,
        currency,
        "Direct Debit",
        None
      ) complete () match {
        case Success(s) =>
          assert(s.transactionId.isDefined)
          assert(s.error.isEmpty)
          directDebitTransactionId = s.getTransactionId
          !?(LoadPaymentAccount(computeExternalUuidWithProfile(vendorUuid, Some("vendor")))) await {
            case result: PaymentAccountLoaded =>
              result.paymentAccount.transactions.find(_.id == directDebitTransactionId) match {
                case Some(transaction) =>
                  assert(transaction.currency == currency)
                  assert(transaction.paymentType == Transaction.PaymentType.DIRECT_DEBITED)
                  assert(transaction.amount == 100)
                  assert(transaction.fees == 0)
                case _ => fail("transaction not found")
              }
            case other => fail(other.toString)
          }
        case Failure(f) => fail(f.getMessage)
      }
    }

    "load direct debit status" in {
      !?(LoadDirectDebitTransaction(directDebitTransactionId)) await {
        case _: DirectDebited =>
        case other            => fail(other.toString)
      }
    }

    "update mandate status" in {
      !?(UpdateMandateStatus(mandateId, Some(Mandate.MandateStatus.MANDATE_ACTIVATED))) await {
        case r: MandateStatusUpdated =>
          val result = r.result
          assert(result.id == mandateId)
          assert(result.status == Mandate.MandateStatus.MANDATE_ACTIVATED)
          !?(LoadPaymentAccount(computeExternalUuidWithProfile(vendorUuid, Some("vendor")))) await {
            case result: PaymentAccountLoaded =>
              result.paymentAccount.mandate match {
                case Some(mandate) =>
                  assert(mandate.id == mandateId)
                  assert(mandate.mandateStatus == Mandate.MandateStatus.MANDATE_ACTIVATED)
                case _ => fail("mandate not found")
              }
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "register recurring direct debit payment" in {
      !?(
        RegisterRecurringPayment(
          computeExternalUuidWithProfile(vendorUuid, Some("vendor")),
          `type` = RecurringPayment.RecurringPaymentType.DIRECT_DEBIT,
          frequency = Some(RecurringPayment.RecurringPaymentFrequency.DAILY),
          endDate = Some(now()),
          fixedNextAmount = Some(true),
          nextDebitedAmount = Some(1000),
          nextFeesAmount = Some(100)
        )
      ) await {
        case result: RecurringPaymentRegistered =>
          recurringPaymentRegistrationId = result.recurringPaymentRegistrationId
          !?(
            LoadRecurringPayment(
              computeExternalUuidWithProfile(vendorUuid, Some("vendor")),
              recurringPaymentRegistrationId
            )
          ) await {
            case result: RecurringPaymentLoaded =>
              val recurringPayment = result.recurringPayment
              assert(recurringPayment.`type`.isDirectDebit)
              assert(recurringPayment.getFrequency.isDaily)
              assert(recurringPayment.firstDebitedAmount == 0)
              assert(recurringPayment.firstFeesAmount == 0)
              assert(recurringPayment.getFixedNextAmount)
              assert(recurringPayment.getNextDebitedAmount == 1000)
              assert(recurringPayment.getNextFeesAmount == 100)
              assert(recurringPayment.getNumberOfRecurringPayments == 0)
              assert(LocalDate.now().isEqual(recurringPayment.getNextRecurringPaymentDate))
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "execute direct debit automatically for next recurring payment" in {
      val probe = createTestProbe[PaymentResult]()
      subscribeProbe(probe)

      !?(CancelMandate(computeExternalUuidWithProfile(vendorUuid, Some("vendor")))) await {
        case MandateNotCanceled =>
        case other              => fail(other.toString)
      }
      probe.expectMessageType[Schedule4PaymentTriggered]
      !?(
        LoadRecurringPayment(
          computeExternalUuidWithProfile(vendorUuid, Some("vendor")),
          recurringPaymentRegistrationId
        )
      ) await {
        case result: RecurringPaymentLoaded =>
          val recurringPayment = result.recurringPayment
          assert(recurringPayment.getCumulatedDebitedAmount == 1000)
          assert(recurringPayment.getCumulatedFeesAmount == 100)
          assert(LocalDate.now().isEqual(recurringPayment.getLastRecurringPaymentDate))
          assert(recurringPayment.lastRecurringPaymentTransactionId.isDefined)
          assert(recurringPayment.getNumberOfRecurringPayments == 1)
          assert(recurringPayment.nextRecurringPaymentDate.isEmpty)
        case other => fail(other.toString)
      }
    }

    "register recurring card payment" in {
      !?(
        RegisterRecurringPayment(
          computeExternalUuidWithProfile(customerUuid, Some("customer")),
          `type` = RecurringPayment.RecurringPaymentType.CARD,
          frequency = Some(RecurringPayment.RecurringPaymentFrequency.DAILY),
          endDate = Some(LocalDate.now().plusDays(1)),
          fixedNextAmount = Some(true),
          nextDebitedAmount = Some(1000),
          nextFeesAmount = Some(100)
        )
      ) await {
        case result: RecurringPaymentRegistered =>
          recurringPaymentRegistrationId = result.recurringPaymentRegistrationId
          !?(
            LoadRecurringPayment(
              computeExternalUuidWithProfile(customerUuid, Some("customer")),
              recurringPaymentRegistrationId
            )
          ) await {
            case result: RecurringPaymentLoaded =>
              val recurringPayment = result.recurringPayment
              assert(recurringPayment.`type`.isCard)
              assert(recurringPayment.getFrequency.isDaily)
              assert(recurringPayment.firstDebitedAmount == 0)
              assert(recurringPayment.firstFeesAmount == 0)
              assert(recurringPayment.getFixedNextAmount)
              assert(recurringPayment.getNextDebitedAmount == 1000)
              assert(recurringPayment.getNextFeesAmount == 100)
              assert(recurringPayment.getNumberOfRecurringPayments == 0)
              assert(recurringPayment.getCardStatus.isCreated)
              assert(LocalDate.now().isEqual(recurringPayment.getNextRecurringPaymentDate))
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "execute first recurring card payment" in {
      !?(
        ExecuteFirstRecurringPayment(
          recurringPaymentRegistrationId,
          computeExternalUuidWithProfile(customerUuid, Some("customer"))
        )
      ) await {
        case _: FirstRecurringPaidIn =>
          !?(
            LoadRecurringPayment(
              computeExternalUuidWithProfile(customerUuid, Some("customer")),
              recurringPaymentRegistrationId
            )
          ) await {
            case result: RecurringPaymentLoaded =>
              val recurringPayment = result.recurringPayment
              assert(recurringPayment.`type`.isCard)
              assert(recurringPayment.getFrequency.isDaily)
              assert(recurringPayment.firstDebitedAmount == 0)
              assert(recurringPayment.firstFeesAmount == 0)
              assert(recurringPayment.getFixedNextAmount)
              assert(recurringPayment.getNextDebitedAmount == 1000)
              assert(recurringPayment.getNextFeesAmount == 100)
              assert(recurringPayment.getNumberOfRecurringPayments == 1)
              assert(recurringPayment.getCardStatus.isInProgress)
              assert(
                LocalDate.now().plusDays(1).isEqual(recurringPayment.getNextRecurringPaymentDate)
              )
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "cancel mandate" in {
      !?(CancelMandate(computeExternalUuidWithProfile(vendorUuid, Some("vendor")))) await {
        case MandateCanceled =>
          !?(LoadPaymentAccount(computeExternalUuidWithProfile(vendorUuid, Some("vendor")))) await {
            case result: PaymentAccountLoaded =>
              assert(result.paymentAccount.mandate.isEmpty)
              val bankAccount = result.paymentAccount.getBankAccount
              assert(bankAccount.mandateId.isEmpty)
              assert(bankAccount.mandateStatus.isEmpty)
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "create or update payment account" in {
      !?(LoadPaymentAccount(computeExternalUuidWithProfile(sellerUuid, Some("seller")))) await {
        case result: PaymentAccountLoaded =>
          val paymentAccount = result.paymentAccount
          val legalUser = paymentAccount.getLegalUser
          val externalUuid = "other"
          val profile = "other"
          !?(
            CreateOrUpdatePaymentAccount(
              paymentAccount.withLegalUser(
                legalUser.withLegalRepresentative(
                  legalUser.legalRepresentative.withExternalUuid(externalUuid).withProfile(profile)
                )
              )
            )
          ) await {
            case PaymentAccountCreated =>
              !?(
                LoadPaymentAccount(computeExternalUuidWithProfile(externalUuid, Some(profile)))
              ) await {
                case result: PaymentAccountLoaded =>
                  log.info(result.paymentAccount.toProtoString)
                case other => fail(other.toString)
              }
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "delete bank account" in {
      !?(
        DeleteBankAccount(computeExternalUuidWithProfile(sellerUuid, Some("seller")), Some(true))
      ) await {
        case BankAccountDeleted =>
          !?(LoadBankAccount(computeExternalUuidWithProfile(sellerUuid, Some("seller")))) await {
            case BankAccountNotFound =>
            case other               => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }

    "disable card" in {
      !?(
        DisablePaymentMethod(computeExternalUuidWithProfile(customerUuid, Some("customer")), cardId)
      ) await {
        case PaymentMethodNotDisabled => // card associated with recurring payment not ended
        case other                    => fail(other.toString)
      }
      !?(
        UpdateRecurringCardPaymentRegistration(
          computeExternalUuidWithProfile(customerUuid, Some("customer")),
          recurringPaymentRegistrationId,
          status = Some(RecurringPayment.RecurringCardPaymentStatus.ENDED)
        )
      ) await {
        case _: RecurringCardPaymentRegistrationUpdated =>
        case other                                      => fail(other.toString)
      }
      !?(
        DisablePaymentMethod(computeExternalUuidWithProfile(customerUuid, Some("customer")), cardId)
      ) await {
        case PaymentMethodDisabled =>
          !?(
            LoadPaymentMethods(computeExternalUuidWithProfile(customerUuid, Some("customer")))
          ) await {
            case result: PaymentMethodsLoaded =>
              val card = PaymentMethodsView(result.paymentMethods).cards.find(_.id == cardId)
              assert(card.isDefined)
              assert(!card.exists(_.active))
            case other => fail(other.toString)
          }
        case other => fail(other.toString)
      }
    }
  }
}
