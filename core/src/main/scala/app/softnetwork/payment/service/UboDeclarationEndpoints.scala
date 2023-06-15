package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{UboDeclaration, UboDeclarationView}
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

trait UboDeclarationEndpoints extends BasicPaymentService {
  _: GenericPaymentEndpoints with GenericPaymentHandler =>

  import app.softnetwork.serialization._

  val addUboDeclaration: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    UboDeclaration.UltimateBeneficialOwner,
    PaymentError,
    (
      Seq[Option[String]],
      Option[CookieValueWithMeta],
      Either[PaymentResult, UboDeclaration.UltimateBeneficialOwner]
    ),
    Any,
    Future
  ] =
    secureEndpoint.post
      .in(PaymentSettings.DeclarationRoute)
      .in(
        jsonBody[UboDeclaration.UltimateBeneficialOwner]
          .description("The UBO to declare for the authenticated legal payment account")
      )
      .out(
        oneOf[Either[PaymentResult, UboDeclaration.UltimateBeneficialOwner]](
          oneOfVariantValueMatcher[Right[PaymentResult, UboDeclaration.UltimateBeneficialOwner]](
            statusCode(StatusCode.Ok)
              .and(
                jsonBody[Right[PaymentResult, UboDeclaration.UltimateBeneficialOwner]]
                  .description("The UBO successfully recorded")
              )
          ) { case Right(_) =>
            true
          },
          oneOfPaymentErrorMessageValueMatcher[UboDeclaration.UltimateBeneficialOwner](
            "UBO recording failure"
          ),
          oneOfVariantValueMatcher[
            Left[PaymentAccountNotFound.type, UboDeclaration.UltimateBeneficialOwner]
          ](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[Left[PaymentAccountNotFound.type, UboDeclaration.UltimateBeneficialOwner]]
                  .description("Payment account not found")
              )
          ) { case Left(PaymentAccountNotFound) =>
            true
          },
          oneOfVariantValueMatcher[
            Left[UboDeclarationNotFound.type, UboDeclaration.UltimateBeneficialOwner]
          ](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[Left[UboDeclarationNotFound.type, UboDeclaration.UltimateBeneficialOwner]]
                  .description("UBO declaration not found")
              )
          ) { case Left(UboDeclarationNotFound) =>
            true
          }
        )
      )
      .serverLogic(principal => { ubo =>
        run(CreateOrUpdateUbo(externalUuidWithProfile(principal._2), ubo)).map {
          case PaymentAccountNotFound =>
            Right((principal._1._1, principal._1._2, Left(PaymentAccountNotFound)))
          case UboDeclarationNotFound =>
            Right((principal._1._1, principal._1._2, Left(UboDeclarationNotFound)))
          case r: UboCreatedOrUpdated =>
            Right((principal._1._1, principal._1._2, Right(r.ubo)))
          case r: PaymentError =>
            Right((principal._1._1, principal._1._2, Left(PaymentErrorMessage(r.message))))
          case other => Right((principal._1._1, principal._1._2, Left(other)))
        }
      })
      .description("Record an UBO for the authenticated legal payment account")

  val loadUboDeclaration: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    Unit,
    PaymentError,
    (Seq[Option[String]], Option[CookieValueWithMeta], Either[PaymentResult, UboDeclarationView]),
    Any,
    Future
  ] =
    secureEndpoint.get
      .in(PaymentSettings.DeclarationRoute)
      .out(
        oneOf[Either[PaymentResult, UboDeclarationView]](
          oneOfVariantValueMatcher[Right[PaymentResult, UboDeclarationView]](
            statusCode(StatusCode.Ok)
              .and(
                jsonBody[Right[PaymentResult, UboDeclarationView]]
                  .description("Ubo declaration of the authenticated legal payment account")
              )
          ) { case Right(_) =>
            true
          },
          oneOfPaymentErrorMessageValueMatcher[UboDeclarationView](
            "Ubo declaration loading failure"
          ),
          oneOfVariantValueMatcher[
            Left[PaymentAccountNotFound.type, UboDeclarationView]
          ](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[Left[PaymentAccountNotFound.type, UboDeclarationView]]
                  .description("Legal payment account not found")
              )
          ) { case Left(PaymentAccountNotFound) =>
            true
          },
          oneOfVariantValueMatcher[
            Left[UboDeclarationNotFound.type, UboDeclarationView]
          ](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[Left[UboDeclarationNotFound.type, UboDeclarationView]].description(
                  "Ubo declaration not found for the authenticated legal payment account"
                )
              )
          ) { case Left(UboDeclarationNotFound) =>
            true
          }
        )
      )
      .serverLogic(principal =>
        _ =>
          run(GetUboDeclaration(externalUuidWithProfile(principal._2))).map {
            case PaymentAccountNotFound =>
              Right((principal._1._1, principal._1._2, Left(PaymentAccountNotFound)))
            case UboDeclarationNotFound =>
              Right((principal._1._1, principal._1._2, Left(UboDeclarationNotFound)))
            case r: UboDeclarationLoaded =>
              Right((principal._1._1, principal._1._2, Right(r.declaration.view)))
            case r: PaymentError =>
              Right((principal._1._1, principal._1._2, Left(PaymentErrorMessage(r.message))))
            case other => Right((principal._1._1, principal._1._2, Left(other)))
          }
      )
      .description("Load the Ubo declaration of the authenticated legal payment account")

  val validateUboDeclaration: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    Unit,
    PaymentError,
    (Seq[Option[String]], Option[CookieValueWithMeta], PaymentResult),
    Any,
    Future
  ] =
    secureEndpoint.put
      .in(PaymentSettings.DeclarationRoute)
      .out(
        oneOf[PaymentResult](
          oneOfVariant[UboDeclarationAskedForValidation.type](
            statusCode(StatusCode.Ok).and(
              jsonBody[UboDeclarationAskedForValidation.type].description(
                "Ubo declaration for the authenticated legal payment account asked for validation"
              )
            )
          ),
          oneOfVariant[PaymentErrorMessage](
            statusCode(StatusCode.BadRequest)
              .and(jsonBody[PaymentErrorMessage].description("Ubo declaration validation failure"))
          ),
          oneOfVariant[PaymentAccountNotFound.type](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[PaymentAccountNotFound.type].description("Legal payment account not found")
              )
          ),
          oneOfVariant[UboDeclarationNotFound.type](
            statusCode(StatusCode.NotFound)
              .and(jsonBody[UboDeclarationNotFound.type].description("Ubo declaration not found"))
          )
        )
      )
      .serverLogic(principal =>
        _ =>
          run(ValidateUboDeclaration(externalUuidWithProfile(principal._2))).map {
            case PaymentAccountNotFound =>
              Right((principal._1._1, principal._1._2, PaymentAccountNotFound))
            case UboDeclarationNotFound =>
              Right((principal._1._1, principal._1._2, UboDeclarationNotFound))
            case r: PaymentError =>
              Right((principal._1._1, principal._1._2, PaymentErrorMessage(r.message)))
            case other => Right((principal._1._1, principal._1._2, other))
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
