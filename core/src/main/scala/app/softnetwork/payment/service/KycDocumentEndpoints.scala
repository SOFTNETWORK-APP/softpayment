package app.softnetwork.payment.service

import app.softnetwork.api.server.ApiErrors
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{KycDocument, KycDocumentValidationReport}
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.StatusCode
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait KycDocumentEndpoints[SD <: SessionData with SessionDataDecorator[SD]] {
  _: RootPaymentEndpoints[SD] with GenericPaymentHandler =>

  import app.softnetwork.serialization._

  val loadKycDocument: ServerEndpoint[Any with AkkaStreams, Future] =
    secureEndpoint.get
      .in(PaymentSettings.KycRoute)
      .in(
        query[String]("documentType")
          .description(
            "Type of Kyc document validation report to load (whether identity proof or one of legal document)"
          ) //KYC_REGISTRATION_PROOF, KYC_ARTICLES_OF_ASSOCIATION, KYC_SHAREHOLDER_DECLARATION or KYC_ADDRESS_PROOF
          .example("KYC_IDENTITY_PROOF")
      )
      .out(
        statusCode(StatusCode.Ok)
          .and(
            jsonBody[KycDocumentValidationReport]
              .description("Kyc document validation report")
          )
      )
      .serverLogic(session => { documentType =>
        val maybeKycDocumentType: Option[KycDocument.KycDocumentType] =
          KycDocument.KycDocumentType.enumCompanion.fromName(documentType)
        maybeKycDocumentType match {
          case None =>
            Future.successful(Left(ApiErrors.BadRequest("wrong kyc document type")))
          case Some(kycDocumentType) =>
            run(LoadKycDocumentStatus(externalUuidWithProfile(session), kycDocumentType)).map {
              case r: KycDocumentStatusLoaded => Right(r.report)
              case other                      => Left(error(other))
            }
        }
      })
      .description("Load Kyc document validation report")

  val addKycDocument: ServerEndpoint[Any with AkkaStreams, Future] =
    secureEndpoint.post
      .in(PaymentSettings.KycRoute)
      .in(
        query[String]("documentType")
          .description(
            "Type of Kyc document to validate (whether identity proof or one of legal document)"
          ) //KYC_REGISTRATION_PROOF, KYC_ARTICLES_OF_ASSOCIATION, KYC_SHAREHOLDER_DECLARATION or KYC_ADDRESS_PROOF
          .example("KYC_IDENTITY_PROOF")
      )
      .in(multipartBody[UploadDocument].description("Kyc document to record for validation"))
      .out(
        statusCode(StatusCode.Ok).and(
          jsonBody[KycDocumentAdded].description(
            "Id of the Kyc document successfully recorded for validation"
          )
        )
      )
      .serverLogic(session => { case (documentType, pages) =>
        val maybeKycDocumentType: Option[KycDocument.KycDocumentType] =
          KycDocument.KycDocumentType.enumCompanion.fromName(documentType)
        maybeKycDocumentType match {
          case None =>
            Future.successful(Left(ApiErrors.BadRequest("wrong kyc document type")))
          case Some(kycDocumentType) =>
            run(AddKycDocument(externalUuidWithProfile(session), pages.bytes, kycDocumentType))
              .map {
                case r: KycDocumentAdded => Right(r)
                case other               => Left(error(other))
              }
        }
      })
      .description("Record Kyc document for validation")

  val kycDocumentEndpoints: List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      loadKycDocument,
      addKycDocument
    )
}
