package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{UboDeclaration, UboDeclarationView}
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.StatusCode
import sttp.tapir.generic.auto._
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir._
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
      .serverLogic(principal => { ubo =>
        run(CreateOrUpdateUbo(externalUuidWithProfile(principal._2), ubo)).map {
          case r: UboCreatedOrUpdated => Right((principal._1._1, principal._1._2, r.ubo))
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
      .serverLogic(principal =>
        _ =>
          run(GetUboDeclaration(externalUuidWithProfile(principal._2))).map {
            case r: UboDeclarationLoaded =>
              Right((principal._1._1, principal._1._2, r.declaration.view))
            case other => Left(error(other))
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
      .serverLogic(principal =>
        _ =>
          run(ValidateUboDeclaration(externalUuidWithProfile(principal._2))).map {
            case UboDeclarationAskedForValidation =>
              Right((principal._1._1, principal._1._2, UboDeclarationAskedForValidation))
            case other => Left(error(other))
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
