package app.softnetwork.payment.service

import app.softnetwork.api.server.HttpCorrelation
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{CardView, PaymentMethodsView, PreRegistration, Transaction}
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.StatusCode
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait PaymentMethodEndpoints[SD <: SessionData with SessionDataDecorator[SD]] {
  _: RootPaymentEndpoints[SD] with PaymentHandler =>

  import app.softnetwork.serialization._

  val loadCards: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.get
      .in(PaymentSettings.PaymentConfig.cardRoute)
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        statusCode(StatusCode.Ok).and(
          jsonBody[Seq[CardView]].description("Authenticated user cards")
        )
      )
      .serverLogic(principal =>
        cid => {
          val cmd =
            LoadPaymentMethods(externalUuidWithProfile(principal._2))
          runCorrelated(cmd, cid).map {
            case r: PaymentMethodsLoaded => Right(PaymentMethodsView(r.paymentMethods).cards)
            case other                   => Left(error(other))
          }
        }
      )
      .description("Load authenticated user cards")

  val loadPaymentMethods: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.get
      .in(PaymentSettings.PaymentConfig.paymentMethodRoute)
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        statusCode(StatusCode.Ok).and(
          jsonBody[PaymentMethodsView].description("Authenticated user payment methods")
        )
      )
      .serverLogic(principal =>
        cid => {
          val cmd =
            LoadPaymentMethods(externalUuidWithProfile(principal._2))
          runCorrelated(cmd, cid).map {
            case r: PaymentMethodsLoaded => Right(PaymentMethodsView(r.paymentMethods))
            case other                   => Left(error(other))
          }
        }
      )
      .description("Load authenticated user payment methods")

  val preRegisterCard: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.post
      .in(PaymentSettings.PaymentConfig.cardRoute)
      .in(jsonBody[PreRegisterPaymentMethod])
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        statusCode(StatusCode.Ok).and(
          jsonBody[PreRegistration]
            .description("Card pre registration data")
        )
      )
      .serverLogic(principal =>
        args => {
          val cmd = args._1
          var updatedUser =
            if (cmd.user.externalUuid.trim.isEmpty) {
              cmd.user.withExternalUuid(principal._2.id)
            } else {
              cmd.user
            }
          principal._2.profile match {
            case Some(profile) if updatedUser.profile.isEmpty =>
              updatedUser = updatedUser.withProfile(profile)
            case _ =>
          }
          runCorrelated(
            cmd.copy(
              user = updatedUser,
              paymentType = Transaction.PaymentType.CARD,
              clientId = principal._1.map(_.clientId).orElse(principal._2.clientId)
            ),
            args._2
          ).map {
            case r: PaymentMethodPreRegistered => Right(r.preRegistration)
            case other                         => Left(error(other))
          }
        }
      )
      .description("Pre register card")

  val preRegisterPaymentMethod: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.post
      .in(PaymentSettings.PaymentConfig.paymentMethodRoute)
      .in(jsonBody[PreRegisterPaymentMethod])
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        statusCode(StatusCode.Ok).and(
          jsonBody[PreRegistration]
            .description("Pre registration data")
        )
      )
      .serverLogic(principal =>
        args => {
          val cmd = args._1
          var updatedUser =
            if (cmd.user.externalUuid.trim.isEmpty) {
              cmd.user.withExternalUuid(principal._2.id)
            } else {
              cmd.user
            }
          principal._2.profile match {
            case Some(profile) if updatedUser.profile.isEmpty =>
              updatedUser = updatedUser.withProfile(profile)
            case _ =>
          }
          runCorrelated(
            cmd.copy(
              user = updatedUser,
              clientId = principal._1.map(_.clientId).orElse(principal._2.clientId)
            ),
            args._2
          ).map {
            case r: PaymentMethodPreRegistered => Right(r.preRegistration)
            case other                         => Left(error(other))
          }
        }
      )
      .description("Pre register payment method")

  val disableCard: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.delete
      .in(PaymentSettings.PaymentConfig.cardRoute)
      .in(query[String]("cardId").description("Card id to disable"))
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        statusCode(StatusCode.Ok).and(jsonBody[PaymentMethodDisabled.type])
      )
      .serverLogic(principal =>
        args => {
          val cmd = DisablePaymentMethod(
            externalUuidWithProfile(principal._2),
            args._1
          )
          runCorrelated(cmd, args._2).map {
            case PaymentMethodDisabled => Right(PaymentMethodDisabled)
            case other                 => Left(error(other))
          }
        }
      )
      .description("Disable registered card")

  val disablePaymentMethod: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.delete
      .in(PaymentSettings.PaymentConfig.paymentMethodRoute)
      .in(query[String]("paymentMethodId").description("Id of Payment method to disable"))
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        statusCode(StatusCode.Ok).and(jsonBody[PaymentMethodDisabled.type])
      )
      .serverLogic(principal =>
        args => {
          val cmd = DisablePaymentMethod(
            externalUuidWithProfile(principal._2),
            args._1
          )
          runCorrelated(cmd, args._2).map {
            case PaymentMethodDisabled => Right(PaymentMethodDisabled)
            case other                 => Left(error(other))
          }
        }
      )
      .description("Disable registered payment method")

  val cardEndpoints: List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      preRegisterCard,
      preRegisterPaymentMethod,
      loadCards,
      loadPaymentMethods,
      disableCard,
      disablePaymentMethod
    )
}
