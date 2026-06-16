package app.softnetwork.payment.service

import app.softnetwork.api.server.HttpCorrelation
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{UboDeclaration, UboDeclarationView}
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.{HeaderNames, StatusCode}
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait UboDeclarationEndpoints[SD <: SessionData with SessionDataDecorator[SD]] {
  _: RootPaymentEndpoints[SD] with PaymentHandler =>

  import app.softnetwork.serialization._

  val addUboDeclaration: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.post
      .in(PaymentSettings.PaymentConfig.declarationRoute)
      .in(
        jsonBody[UboDeclaration.UltimateBeneficialOwner]
          .description("The UBO to declare for the authenticated legal payment account")
      )
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        statusCode(StatusCode.Ok)
          .and(
            jsonBody[UboDeclaration.UltimateBeneficialOwner]
              .description("The UBO successfully recorded")
          )
      )
      .serverLogic(principal => { args =>
        val ubo = args._1
        val correlationId = args._2
        val cmd =
          CreateOrUpdateUbo(externalUuidWithProfile(principal._2), ubo)
        runCorrelated(cmd, correlationId).map {
          case r: UboCreatedOrUpdated => Right(r.ubo)
          case other                  => Left(error(other))
        }
      })
      .description("Record an UBO for the authenticated legal payment account")

  val loadUboDeclaration: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.get
      .in(PaymentSettings.PaymentConfig.declarationRoute)
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        statusCode(StatusCode.Ok)
          .and(
            jsonBody[UboDeclarationView]
              .description("Ubo declaration of the authenticated legal payment account")
          )
      )
      .serverLogic(principal =>
        cid => {
          val cmd = GetUboDeclaration(externalUuidWithProfile(principal._2))
          runCorrelated(cmd, cid).map {
            case r: UboDeclarationLoaded => Right(r.declaration.view)
            case other                   => Left(error(other))
          }
        }
      )
      .description("Load the Ubo declaration of the authenticated legal payment account")

  val validateUboDeclaration: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.put
      .in(clientIp)
      .in(header[Option[String]](HeaderNames.UserAgent))
      .in(
        query[Option[String]]("tokenId").description(
          "The token id of the Ubo declaration to validate"
        )
      )
      .in(PaymentSettings.PaymentConfig.declarationRoute)
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        statusCode(StatusCode.Ok).and(
          jsonBody[UboDeclarationAskedForValidation.type].description(
            "Ubo declaration for the authenticated legal payment account asked for validation"
          )
        )
      )
      .serverLogic(principal => { case (ipAddress, userAgent, tokenId, cid) =>
        val session: SD = principal._2
        val cmd = ValidateUboDeclaration(
          externalUuidWithProfile(session),
          ipAddress,
          userAgent,
          tokenId
        )
        runCorrelated(cmd, cid).map {
          case UboDeclarationAskedForValidation => Right(UboDeclarationAskedForValidation)
          case other                            => Left(error(other))
        }
      })
      .description("Validate the Ubo declaration of the authenticated legal payment account")

  val uboDeclarationEndpoints
    : List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      addUboDeclaration,
      loadUboDeclaration,
      validateUboDeclaration
    )
}
