package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{BankAccountView, PaymentAccount}
import org.softnetwork.session.model.Session
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.{Method, StatusCode}
import sttp.model.headers.CookieValueWithMeta
import sttp.tapir.server.ServerEndpoint.Full
import sttp.tapir.generic.auto._
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir._
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait BankAccountEndpoints { _: RootPaymentEndpoints with GenericPaymentHandler =>

  import app.softnetwork.serialization._

  val createOrUpdateBankAccount: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    BankAccountCommand,
    PaymentError,
    (Seq[Option[String]], Option[CookieValueWithMeta], PaymentResult),
    Any,
    Future
  ] =
    secureEndpoint.post
      .in(PaymentSettings.BankRoute)
      .in(jsonBody[BankAccountCommand].description("Legal or natural user bank account"))
      .out(
        oneOf[PaymentResult](
          oneOfVariant[BankAccountCreatedOrUpdated](
            statusCode(StatusCode.Ok).and(jsonBody[BankAccountCreatedOrUpdated])
          ),
          oneOfVariant[PaymentErrorMessage](
            statusCode(StatusCode.BadRequest)
              .and(jsonBody[PaymentErrorMessage].description("Bank account creation failure"))
          ),
          oneOfVariant[PaymentAccountNotFound.type](
            statusCode(StatusCode.NotFound)
              .and(jsonBody[PaymentAccountNotFound.type].description("Payment account not found"))
          )
        )
      )
      .serverLogic(principal => { bank =>
        val session = principal._2
        import bank._
        var externalUuid: String = ""
        val updatedUser: Option[PaymentAccount.User] = {
          user match {
            case Left(naturalUser) =>
              var updatedNaturalUser = {
                if (naturalUser.externalUuid.trim.isEmpty) {
                  naturalUser.withExternalUuid(session.id)
                } else {
                  naturalUser
                }
              }
              session.profile match {
                case Some(profile) if updatedNaturalUser.profile.isEmpty =>
                  updatedNaturalUser = updatedNaturalUser.withProfile(profile)
                case _ =>
              }
              externalUuid = updatedNaturalUser.externalUuid
              Some(PaymentAccount.User.NaturalUser(updatedNaturalUser))
            case Right(legalUser) =>
              var updatedLegalRepresentative = legalUser.legalRepresentative
              if (updatedLegalRepresentative.externalUuid.trim.isEmpty) {
                updatedLegalRepresentative = updatedLegalRepresentative.withExternalUuid(session.id)
              }
              session.profile match {
                case Some(profile) if updatedLegalRepresentative.profile.isEmpty =>
                  updatedLegalRepresentative = updatedLegalRepresentative.withProfile(profile)
                case _ =>
              }
              externalUuid = updatedLegalRepresentative.externalUuid
              Some(
                PaymentAccount.User.LegalUser(
                  legalUser.withLegalRepresentative(updatedLegalRepresentative)
                )
              )
          }
        }
        run(
          CreateOrUpdateBankAccount(
            externalUuidWithProfile(session),
            bankAccount.withExternalUuid(externalUuid),
            updatedUser,
            acceptedTermsOfPSP
          )
        ).map {
          case r: BankAccountCreatedOrUpdated => Right((principal._1._1, principal._1._2, r))
          case r: PaymentError =>
            Right((principal._1._1, principal._1._2, PaymentErrorMessage(r.message)))
          case other =>
            Right((principal._1._1, principal._1._2, other))
        }
      })
      .description("Create or update legal or natural user bank account")

  val loadBankAccount: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    Unit,
    PaymentError,
    (Seq[Option[String]], Option[CookieValueWithMeta], Either[PaymentResult, BankAccountView]),
    Any,
    Future
  ] =
    secureEndpoint.get
      .in(PaymentSettings.BankRoute)
      .out(
        oneOf[Either[PaymentResult, BankAccountView]](
          oneOfVariantValueMatcher[Right[PaymentResult, BankAccountView]](
            statusCode(StatusCode.Ok).and(
              jsonBody[Right[PaymentResult, BankAccountView]]
                .description("Authenticated user bank account")
            )
          ) { case Right(_) =>
            true
          },
          oneOfPaymentErrorMessageValueMatcher[BankAccountView]("Bank account loading failure"),
          oneOfVariantValueMatcher[Left[PaymentAccountNotFound.type, BankAccountView]](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[Left[PaymentAccountNotFound.type, BankAccountView]]
                  .description("Payment account not found")
              )
          ) { case Left(PaymentAccountNotFound) =>
            true
          },
          oneOfVariantValueMatcher[Left[BankAccountNotFound.type, BankAccountView]](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[Left[BankAccountNotFound.type, BankAccountView]]
                  .description("Bank account not found")
              )
          ) { case Left(BankAccountNotFound) =>
            true
          }
        )
      )
      .serverLogic(principal =>
        _ => {
          run(
            LoadBankAccount(externalUuidWithProfile(principal._2))
          ).map {
            case r: BankAccountLoaded =>
              Right((principal._1._1, principal._1._2, Right(r.bankAccount.view)))
            case r: PaymentError =>
              Right((principal._1._1, principal._1._2, Left(PaymentErrorMessage(r.message))))
            case other => Right((principal._1._1, principal._1._2, Left(other)))
          }
        }
      )
      .description("Load authenticated user bank account")

  val deleteBankAccount: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    Unit,
    PaymentError,
    (Seq[Option[String]], Option[CookieValueWithMeta], PaymentResult),
    Any,
    Future
  ] =
    secureEndpoint.delete
      .in(PaymentSettings.BankRoute)
      .out(
        oneOf[PaymentResult](
          oneOfVariant[BankAccountDeleted.type](
            statusCode(StatusCode.Ok).and(jsonBody[BankAccountDeleted.type])
          ),
          oneOfVariant[PaymentErrorMessage](
            statusCode(StatusCode.BadRequest)
              .and(jsonBody[PaymentErrorMessage].description("Bank account deletion failure"))
          ),
          oneOfVariant[PaymentAccountNotFound.type](
            statusCode(StatusCode.NotFound)
              .and(jsonBody[PaymentAccountNotFound.type].description("Payment account not found"))
          ),
          oneOfVariant[BankAccountNotFound.type](
            statusCode(StatusCode.NotFound)
              .and(jsonBody[BankAccountNotFound.type].description("Bank account not found"))
          )
        )
      )
      .serverLogic(principal =>
        _ =>
          run(DeleteBankAccount(externalUuidWithProfile(principal._2), Some(false))).map {
            case PaymentAccountNotFound =>
              Right((principal._1._1, principal._1._2, PaymentAccountNotFound))
            case BankAccountNotFound =>
              Right((principal._1._1, principal._1._2, PaymentAccountNotFound))
            case r: PaymentError =>
              Right((principal._1._1, principal._1._2, PaymentErrorMessage(r.message)))
            case other => Right((principal._1._1, principal._1._2, other))
          }
      )
      .description("Delete authenticated user bank account")

  val bankAccountEndpoints: List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      createOrUpdateBankAccount,
      loadBankAccount,
      deleteBankAccount
    )
}
