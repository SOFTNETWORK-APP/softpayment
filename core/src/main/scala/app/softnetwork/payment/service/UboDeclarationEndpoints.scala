package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{UboDeclaration, UboDeclarationView}
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.StatusCode
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait UboDeclarationEndpoints { _: RootPaymentEndpoints with GenericPaymentHandler =>

  import app.softnetwork.serialization._

  val addUboDeclaration: ServerEndpoint[Any with AkkaStreams, Future] =
    secureEndpoint.post
      .in(PaymentSettings.DeclarationRoute)
      .in(
        jsonBody[UboDeclaration.UltimateBeneficialOwner]
          .description("The UBO to declare for the authenticated legal payment account")
      )
      .out(
        statusCode(StatusCode.Ok)
          .and(
            jsonBody[UboDeclaration.UltimateBeneficialOwner]
              .description("The UBO successfully recorded")
          )
      )
      .serverLogic(session => { ubo =>
        run(CreateOrUpdateUbo(externalUuidWithProfile(session), ubo)).map {
          case r: UboCreatedOrUpdated => Right(r.ubo)
          case other                  => Left(error(other))
        }
      })
      .description("Record an UBO for the authenticated legal payment account")

  val loadUboDeclaration: ServerEndpoint[Any with AkkaStreams, Future] =
    secureEndpoint.get
      .in(PaymentSettings.DeclarationRoute)
      .out(
        statusCode(StatusCode.Ok)
          .and(
            jsonBody[UboDeclarationView]
              .description("Ubo declaration of the authenticated legal payment account")
          )
      )
      .serverLogic(session =>
        _ =>
          run(GetUboDeclaration(externalUuidWithProfile(session))).map {
            case r: UboDeclarationLoaded => Right(r.declaration.view)
            case other                   => Left(error(other))
          }
      )
      .description("Load the Ubo declaration of the authenticated legal payment account")

  val validateUboDeclaration: ServerEndpoint[Any with AkkaStreams, Future] =
    secureEndpoint.put
      .in(PaymentSettings.DeclarationRoute)
      .out(
        statusCode(StatusCode.Ok).and(
          jsonBody[UboDeclarationAskedForValidation.type].description(
            "Ubo declaration for the authenticated legal payment account asked for validation"
          )
        )
      )
      .serverLogic(session =>
        _ =>
          run(ValidateUboDeclaration(externalUuidWithProfile(session))).map {
            case UboDeclarationAskedForValidation => Right(UboDeclarationAskedForValidation)
            case other                            => Left(error(other))
          }
      )
      .description("Validate the Ubo declaration of the authenticated legal payment account")

  val uboDeclarationEndpoints
    : List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      addUboDeclaration,
      loadUboDeclaration,
      validateUboDeclaration
    )
}
