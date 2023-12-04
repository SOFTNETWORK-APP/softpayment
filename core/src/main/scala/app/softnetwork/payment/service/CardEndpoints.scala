package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{CardPreRegistration, CardView}
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.StatusCode
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait CardEndpoints[SD <: SessionData with SessionDataDecorator[SD]] {
  _: RootPaymentEndpoints[SD] with GenericPaymentHandler =>

  import app.softnetwork.serialization._

  val loadCards: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.get
      .in(PaymentSettings.CardRoute)
      .out(
        statusCode(StatusCode.Ok).and(
          jsonBody[Seq[CardView]].description("Authenticated user cards")
        )
      )
      .serverLogic(principal =>
        _ => {
          run(LoadCards(externalUuidWithProfile(principal._2))).map {
            case r: CardsLoaded => Right(r.cards.map(_.view))
            case other          => Left(error(other))
          }
        }
      )
      .description("Load authenticated user cards")

  val preRegisterCard: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.post
      .in(PaymentSettings.CardRoute)
      .in(jsonBody[PreRegisterCard])
      .out(
        statusCode(StatusCode.Ok).and(
          jsonBody[CardPreRegistration]
            .description("Card pre registration data")
        )
      )
      .serverLogic(principal =>
        cmd => {
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
          run(cmd.copy(user = updatedUser, clientId = principal._1.map(_.clientId))).map {
            case r: CardPreRegistered => Right(r.cardPreRegistration)
            case other                => Left(error(other))
          }
        }
      )
      .description("Pre register card")

  val disableCard: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.delete
      .in(PaymentSettings.CardRoute)
      .in(query[String]("cardId").description("Card id to disable"))
      .out(
        statusCode(StatusCode.Ok).and(jsonBody[CardDisabled.type])
      )
      .serverLogic(principal =>
        cardId => {
          run(DisableCard(externalUuidWithProfile(principal._2), cardId)).map {
            case CardDisabled => Right(CardDisabled)
            case other        => Left(error(other))
          }
        }
      )
      .description("Disable registered card")

  val cardEndpoints: List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      preRegisterCard,
      loadCards,
      disableCard
    )
}
