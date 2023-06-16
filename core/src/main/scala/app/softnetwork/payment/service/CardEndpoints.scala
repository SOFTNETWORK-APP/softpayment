package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{CardPreRegistration, CardView}
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

trait CardEndpoints { _: RootPaymentEndpoints with GenericPaymentHandler =>

  import app.softnetwork.serialization._

  val loadCards: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    Unit,
    PaymentError,
    (Seq[Option[String]], Option[CookieValueWithMeta], Either[PaymentResult, Seq[CardView]]),
    Any,
    Future
  ] =
    secureEndpoint.get
      .in(PaymentSettings.CardRoute)
      .out(
        oneOf[Either[PaymentResult, Seq[CardView]]](
          oneOfVariantValueMatcher[Right[PaymentResult, Seq[CardView]]](
            statusCode(StatusCode.Ok).and(
              jsonBody[Right[PaymentResult, Seq[CardView]]].description("Authenticated user cards")
            )
          ) { case Right(_) =>
            true
          },
          oneOfPaymentErrorMessageValueMatcher[Seq[CardView]](
            "Authenticated user cards loading failure"
          )
        )
      )
      .serverLogic(principal =>
        _ => {
          run(LoadCards(externalUuidWithProfile(principal._2))).map {
            case r: CardsLoaded =>
              Right((principal._1._1, principal._1._2, Right(r.cards.map(_.view))))
            case r: PaymentError =>
              Right((principal._1._1, principal._1._2, Left(PaymentErrorMessage(r.message))))
            case other => Right((principal._1._1, principal._1._2, Left(other)))
          }
        }
      )
      .description("Load authenticated user cards")

  val preRegisterCard: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    PreRegisterCard,
    PaymentError,
    (Seq[Option[String]], Option[CookieValueWithMeta], Either[PaymentResult, CardPreRegistration]),
    Any,
    Future
  ] =
    secureEndpoint.post
      .in(PaymentSettings.CardRoute)
      .in(jsonBody[PreRegisterCard])
      .out(
        oneOf[Either[PaymentResult, CardPreRegistration]](
          oneOfVariantValueMatcher[Right[PaymentResult, CardPreRegistration]](
            statusCode(StatusCode.Ok).and(
              jsonBody[Right[PaymentResult, CardPreRegistration]]
                .description("Card pre registration data")
            )
          ) { case Right(_) =>
            true
          },
          oneOfVariantValueMatcher[Left[CardNotPreRegistered.type, CardPreRegistration]](
            statusCode(StatusCode.BadRequest)
              .and(jsonBody[Left[CardNotPreRegistered.type, CardPreRegistration]])
          ) { case Left(CardNotPreRegistered) =>
            true
          }
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
              Right((principal._1._1, principal._1._2, Right(r.cardPreRegistration)))
            case other => Right((principal._1._1, principal._1._2, Left(other)))
          }
        }
      )
      .description("Pre register card")

  val disableCard: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    String,
    PaymentError,
    (
      Seq[Option[String]],
      Option[CookieValueWithMeta],
      Either[PaymentResult, CardDisabled.type]
    ),
    Any,
    Future
  ] =
    secureEndpoint.delete
      .in(PaymentSettings.CardRoute)
      .in(query[String]("cardId").description("Card id to disable"))
      .out(
        oneOf[Either[PaymentResult, CardDisabled.type]](
          oneOfVariantValueMatcher[Right[PaymentResult, CardDisabled.type]](
            statusCode(StatusCode.Ok).and(jsonBody[Right[PaymentResult, CardDisabled.type]])
          ) { case Right(_) =>
            true
          },
          oneOfVariantValueMatcher[Left[CardNotDisabled.type, CardDisabled.type]](
            statusCode(StatusCode.BadRequest)
              .and(
                jsonBody[Left[CardNotDisabled.type, CardDisabled.type]]
                  .description("Card disabling failure")
              )
          ) { case Left(CardNotDisabled) =>
            true
          }
        )
      )
      .serverLogic(principal =>
        cardId => {
          run(DisableCard(externalUuidWithProfile(principal._2), cardId)).map {
            case CardDisabled => Right((principal._1._1, principal._1._2, Right(CardDisabled)))
            case other        => Right((principal._1._1, principal._1._2, Left(other)))
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
