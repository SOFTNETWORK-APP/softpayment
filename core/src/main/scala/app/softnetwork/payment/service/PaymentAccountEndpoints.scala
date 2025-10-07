package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{PaymentAccount, PaymentAccountView}
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.{HeaderNames, StatusCode}
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait PaymentAccountEndpoints[SD <: SessionData with SessionDataDecorator[SD]] {
  _: RootPaymentEndpoints[SD] with PaymentHandler =>

  import app.softnetwork.serialization._

  lazy val createOrUpdatePaymentAccount: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.post
      .in(PaymentSettings.PaymentConfig.accountRoute)
      .in(clientIp)
      .in(header[Option[String]](HeaderNames.UserAgent))
      .in(jsonBody[UserPaymentAccountCommand].description("Legal or natural user payment account"))
      .out(
        statusCode(StatusCode.Ok)
          .and(
            jsonBody[UserPaymentAccountCreatedOrUpdated].description(
              "User payment account created or updated"
            )
          )
      )
      .serverLogic {
        case (client, session) => { case (ipAddress, userAgent, userAccountCommand) =>
          import userAccountCommand._
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
                  updatedLegalRepresentative =
                    updatedLegalRepresentative.withExternalUuid(session.id)
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
            CreateOrUpdateUserPaymentAccount(
              externalUuidWithProfile(session),
              updatedUser,
              acceptedTermsOfPSP,
              clientId = client.map(_.clientId).orElse(session.clientId),
              ipAddress = ipAddress,
              userAgent = userAgent,
              tokenId = tokenId
            )
          ).map {
            case r: UserPaymentAccountCreatedOrUpdated => Right(r)
            case other                                 => Left(error(other))
          }
        }
      }
      .description("Create or update legal or natural user payment account")

  lazy val loadPaymentAccount: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.get
      .in(PaymentSettings.PaymentConfig.accountRoute)
      .out(jsonBody[PaymentAccountView].description("Authenticated user payment account"))
      .serverLogic { case (client, session) =>
        _ => {
          run(
            LoadPaymentAccount(
              externalUuidWithProfile(session),
              clientId = client.map(_.clientId).orElse(session.clientId)
            )
          ).map {
            case r: PaymentAccountLoaded => Right(r.paymentAccount.view)
            case other                   => Left(error(other))
          }
        }
      }
      .description("Load authenticated user payment account")

  lazy val paymentAccountEndpoints
    : List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      createOrUpdatePaymentAccount,
      loadPaymentAccount
    )
}
