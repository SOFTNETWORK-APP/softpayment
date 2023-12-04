package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{BankAccountView, PaymentAccount}
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.StatusCode
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait BankAccountEndpoints[SD <: SessionData with SessionDataDecorator[SD]] {
  _: RootPaymentEndpoints[SD] with GenericPaymentHandler =>

  import app.softnetwork.serialization._

  val createOrUpdateBankAccount: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.post
      .in(PaymentSettings.BankRoute)
      .in(jsonBody[BankAccountCommand].description("Legal or natural user bank account"))
      .out(
        statusCode(StatusCode.Ok)
          .and(jsonBody[BankAccountCreatedOrUpdated].description("Bank account created or updated"))
      )
      .serverLogic(principal => { bank =>
        import bank._
        var externalUuid: String = ""
        val updatedUser: Option[PaymentAccount.User] = {
          user match {
            case Left(naturalUser) =>
              var updatedNaturalUser = {
                if (naturalUser.externalUuid.trim.isEmpty) {
                  naturalUser.withExternalUuid(principal._2.id)
                } else {
                  naturalUser
                }
              }
              principal._2.profile match {
                case Some(profile) if updatedNaturalUser.profile.isEmpty =>
                  updatedNaturalUser = updatedNaturalUser.withProfile(profile)
                case _ =>
              }
              externalUuid = updatedNaturalUser.externalUuid
              Some(PaymentAccount.User.NaturalUser(updatedNaturalUser))
            case Right(legalUser) =>
              var updatedLegalRepresentative = legalUser.legalRepresentative
              if (updatedLegalRepresentative.externalUuid.trim.isEmpty) {
                updatedLegalRepresentative =
                  updatedLegalRepresentative.withExternalUuid(principal._2.id)
              }
              principal._2.profile match {
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
            externalUuidWithProfile(principal._2),
            bankAccount.withExternalUuid(externalUuid),
            updatedUser,
            acceptedTermsOfPSP,
            clientId = principal._1.map(_.clientId)
          )
        ).map {
          case r: BankAccountCreatedOrUpdated => Right(r)
          case other                          => Left(error(other))
        }
      })
      .description("Create or update legal or natural user bank account")

  val loadBankAccount: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.get
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
            case r: BankAccountLoaded => Right(r.bankAccount.view)
            case other                => Left(error(other))
          }
        }
      )
      .description("Load authenticated user bank account")

  val deleteBankAccount: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.delete
      .in(PaymentSettings.BankRoute)
      .out(
        statusCode(StatusCode.Ok).and(jsonBody[BankAccountDeleted.type])
      )
      .serverLogic(principal =>
        _ =>
          run(DeleteBankAccount(externalUuidWithProfile(principal._2), Some(false))).map {
            case BankAccountDeleted => Right(BankAccountDeleted)
            case other              => Left(error(other))
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
