package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{CardPreRegistration, CardView}
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.StatusCode
import sttp.tapir.generic.auto._
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir._
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait CardEndpoints { _: RootPaymentEndpoints with GenericPaymentHandler =>

  import app.softnetwork.serialization._

  val loadCards: ServerEndpoint[Any with AkkaStreams, Future] =
    secureEndpoint.get
      .in(PaymentSettings.CardRoute)
      .out(
        statusCode(StatusCode.Ok).and(
          jsonBody[Seq[CardView]].description("Authenticated user cards")
        )
      )
      .serverLogic(principal =>
        _ => {
          run(LoadCards(externalUuidWithProfile(principal._2))).map {
            case r: CardsLoaded =>
              Right((principal._1._1, principal._1._2, r.cards.map(_.view)))
            case other => Left(error(other))
          }
        }
      )
      .description("Load authenticated user cards")

  val preRegisterCard: ServerEndpoint[Any with AkkaStreams, Future] =
    secureEndpoint.post
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
          val session = principal._2
          var updatedUser =
            if (cmd.user.externalUuid.trim.isEmpty) {
              cmd.user.withExternalUuid(session.id)
            } else {
              cmd.user
            }
          session.profile match {
            case Some(profile) if updatedUser.profile.isEmpty =>
              updatedUser = updatedUser.withProfile(profile)
            case _ =>
          }
          run(cmd.copy(user = updatedUser)).map {
            case r: CardPreRegistered =>
              Right((principal._1._1, principal._1._2, r.cardPreRegistration))
            case other => Left(error(other))
          }
        }
      )
      .description("Pre register card")

  val disableCard: ServerEndpoint[Any with AkkaStreams, Future] =
    secureEndpoint.delete
      .in(PaymentSettings.CardRoute)
      .in(query[String]("cardId").description("Card id to disable"))
      .out(
        statusCode(StatusCode.Ok).and(jsonBody[CardDisabled.type])
      )
      .serverLogic(principal =>
        cardId => {
          run(DisableCard(externalUuidWithProfile(principal._2), cardId)).map {
            case CardDisabled => Right((principal._1._1, principal._1._2, CardDisabled))
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
