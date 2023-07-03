package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{BankAccountView, PaymentAccount}
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.StatusCode
import sttp.tapir.generic.auto._
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir._
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait BankAccountEndpoints { _: RootPaymentEndpoints with GenericPaymentHandler =>

  import app.softnetwork.serialization._

  val createOrUpdateBankAccount: ServerEndpoint[Any with AkkaStreams, Future] =
    secureEndpoint.post
      .in(PaymentSettings.BankRoute)
      .in(jsonBody[BankAccountCommand].description("Legal or natural user bank account"))
      .out(
        statusCode(StatusCode.Ok)
          .and(jsonBody[BankAccountCreatedOrUpdated].description("Bank account created or updated"))
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
          case other                          => Left(error(other))
        }
      })
      .description("Create or update legal or natural user bank account")

  val loadBankAccount: ServerEndpoint[Any with AkkaStreams, Future] =
    secureEndpoint.get
      .in(PaymentSettings.BankRoute)
      .out(
        statusCode(StatusCode.Ok).and(
          jsonBody[BankAccountView]
            .description("Authenticated user bank account")
        )
      )
      .serverLogic(principal =>
        _ => {
          run(
            LoadBankAccount(externalUuidWithProfile(principal._2))
          ).map {
            case r: BankAccountLoaded =>
              Right((principal._1._1, principal._1._2, r.bankAccount.view))
            case other => Left(error(other))
          }
        }
      )
      .description("Load authenticated user bank account")

  val deleteBankAccount: ServerEndpoint[Any with AkkaStreams, Future] =
    secureEndpoint.delete
      .in(PaymentSettings.BankRoute)
      .out(
        statusCode(StatusCode.Ok).and(jsonBody[BankAccountDeleted.type])
      )
      .serverLogic(principal =>
        _ =>
          run(DeleteBankAccount(externalUuidWithProfile(principal._2), Some(false))).map {
            case BankAccountDeleted =>
              Right((principal._1._1, principal._1._2, BankAccountDeleted))
            case other => Left(error(other))
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
