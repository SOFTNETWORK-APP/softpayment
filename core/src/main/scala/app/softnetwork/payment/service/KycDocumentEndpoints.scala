package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{KycDocument, KycDocumentValidationReport}
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

trait KycDocumentEndpoints extends BasicPaymentService {
  _: GenericPaymentEndpoints with GenericPaymentHandler =>

  import app.softnetwork.serialization._

  val loadKycDocument: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    String,
    PaymentError,
    (
      Seq[Option[String]],
      Option[CookieValueWithMeta],
      Either[PaymentResult, KycDocumentValidationReport]
    ),
    Any,
    Future
  ] =
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
        oneOf[Either[PaymentResult, KycDocumentValidationReport]](
          oneOfVariantValueMatcher[Right[PaymentResult, KycDocumentValidationReport]](
            statusCode(StatusCode.Ok)
              .and(
                jsonBody[Right[PaymentResult, KycDocumentValidationReport]]
                  .description("Kyc document validation report")
              )
          ) { case Right(_) =>
            true
          },
          oneOfPaymentErrorMessageValueMatcher[KycDocumentValidationReport](
            "Kyc document validation report loading failure"
          ),
          oneOfVariantValueMatcher[Left[PaymentAccountNotFound.type, KycDocumentValidationReport]](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[Left[PaymentAccountNotFound.type, KycDocumentValidationReport]]
                  .description("Payment account not found")
              )
          ) { case Left(PaymentAccountNotFound) =>
            true
          }
        )
      )
      .serverLogic(principal => { documentType =>
        val maybeKycDocumentType: Option[KycDocument.KycDocumentType] =
          KycDocument.KycDocumentType.enumCompanion.fromName(documentType)
        maybeKycDocumentType match {
          case None =>
            Future.successful(
              Right(
                (
                  principal._1._1,
                  principal._1._2,
                  Left(PaymentErrorMessage("wrong kyc document type"))
                )
              )
            )
          case Some(kycDocumentType) =>
            run(LoadKycDocumentStatus(externalUuidWithProfile(principal._2), kycDocumentType)).map {
              case r: KycDocumentStatusLoaded =>
                Right((principal._1._1, principal._1._2, Right(r.report)))
              case PaymentAccountNotFound =>
                Right((principal._1._1, principal._1._2, Left(PaymentAccountNotFound)))
              case r: PaymentError =>
                Right((principal._1._1, principal._1._2, Left(PaymentErrorMessage(r.message))))
              case other => Right((principal._1._1, principal._1._2, Left(other)))
            }
        }
      })
      .description("Load Kyc document validation report")

  val addKycDocument: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    (String, UploadDocument),
    PaymentError,
    (Seq[Option[String]], Option[CookieValueWithMeta], PaymentResult),
    Any,
    Future
  ] =
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
        oneOf[PaymentResult](
          oneOfVariant[KycDocumentAdded](
            statusCode(StatusCode.Ok).and(
              jsonBody[KycDocumentAdded].description(
                "Id of the Kyc document successfully recorded for validation"
              )
            )
          ),
          oneOfVariant[PaymentErrorMessage](
            statusCode(StatusCode.BadRequest)
              .and(jsonBody[PaymentErrorMessage].description("Kyc document recording failure"))
          ),
          oneOfVariant[PaymentAccountNotFound.type](
            statusCode(StatusCode.NotFound)
              .and(jsonBody[PaymentAccountNotFound.type].description("Payment account not found"))
          )
        )
      )
      .serverLogic(principal => { case (documentType, pages) =>
        val maybeKycDocumentType: Option[KycDocument.KycDocumentType] =
          KycDocument.KycDocumentType.enumCompanion.fromName(documentType)
        maybeKycDocumentType match {
          case None =>
            Future.successful(
              Right(
                (
                  principal._1._1,
                  principal._1._2,
                  PaymentErrorMessage("wrong kyc document type")
                )
              )
            )
          case Some(kycDocumentType) =>
            run(AddKycDocument(externalUuidWithProfile(principal._2), pages.bytes, kycDocumentType))
              .map {
                case r: KycDocumentAdded =>
                  Right((principal._1._1, principal._1._2, r))
                case PaymentAccountNotFound =>
                  Right((principal._1._1, principal._1._2, PaymentAccountNotFound))
                case r: PaymentError =>
                  Right((principal._1._1, principal._1._2, PaymentErrorMessage(r.message)))
                case other => Right((principal._1._1, principal._1._2, other))
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
