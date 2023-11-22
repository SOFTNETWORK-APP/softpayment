package app.softnetwork.payment.scalatest

import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ContentTypes, Multipart, StatusCodes}
import app.softnetwork.api.server.ApiRoutes
import app.softnetwork.api.server.config.ServerSettings.RootPath
import app.softnetwork.payment.api.PaymentGrpcServices
import app.softnetwork.payment.config.PaymentSettings._
import app.softnetwork.payment.model._
import app.softnetwork.session.scalatest.SessionTestKit
import app.softnetwork.session.service.SessionMaterials
import org.scalatest.Suite

import java.nio.file.Paths

trait PaymentRouteTestKit extends SessionTestKit with PaymentTestKit with PaymentGrpcServices {
  _: Suite with ApiRoutes with SessionMaterials =>

  import app.softnetwork.serialization._

  override lazy val additionalConfig: String = paymentGrpcConfig

  override def beforeAll(): Unit = {
    initAndJoinCluster()
    // pre load routes
    mainRoutes(typedSystem())
  }

  def loadPaymentAccount(): PaymentAccountView = {
    withHeaders(
      Get(s"/$RootPath/$PaymentPath")
    ) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[PaymentAccountView]
    }
  }

  def loadCards(): Seq[Card] = {
    withHeaders(
      Get(s"/$RootPath/$PaymentPath/$CardRoute")
    ) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[Seq[Card]]
    }
  }

  def loadBankAccount(): BankAccountView = {
    withHeaders(
      Get(s"/$RootPath/$PaymentPath/$BankRoute")
    ) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[BankAccountView]
    }
  }

  def addKycDocuments(): Unit = {
    val path =
      Paths.get(Thread.currentThread().getContextClassLoader.getResource("avatar.png").getPath)
    loadPaymentAccount().documents
      .filter(_.documentStatus == KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED)
      .map(_.documentType)
      .foreach { documentType =>
        withHeaders(
          Post(
            s"/$RootPath/$PaymentPath/$KycRoute?documentType=$documentType",
            entity = Multipart.FormData
              .fromPath(
                "pages",
                ContentTypes.`application/octet-stream`,
                path,
                100000
              )
              .toEntity
          ).withHeaders(
            RawHeader("Content-Type", "application/x-www-form-urlencoded"),
            RawHeader("Content-Type", "multipart/form-data")
          )
        ) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          assert(
            loadKycDocumentStatus(
              documentType
            ).status == KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATION_ASKED
          )
        }
      }
  }

  def loadKycDocumentStatus(
    documentType: KycDocument.KycDocumentType
  ): KycDocumentValidationReport = {
    withHeaders(
      Get(s"/$RootPath/$PaymentPath/$KycRoute?documentType=$documentType")
    ) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[KycDocumentValidationReport]
    }
  }

  def validateKycDocuments(): Unit = {
    loadPaymentAccount().documents
      .filter(_.documentStatus == KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATION_ASKED)
      .map(_.documentType)
      .foreach { documentType =>
        withHeaders(
          Get(s"/$RootPath/$PaymentPath/$KycRoute?documentType=$documentType")
        ) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val report = responseAs[KycDocumentValidationReport]
          assert(report.status == KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATION_ASKED)
          Get(
            s"/$RootPath/$PaymentPath/$HooksRoute?EventType=KYC_SUCCEEDED&RessourceId=${report.id}"
          ) ~> routes ~> check {
            status shouldEqual StatusCodes.OK
            assert(
              loadKycDocumentStatus(
                documentType
              ).status == KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATED
            )
          }
        }
      }
  }

  def loadDeclaration(): UboDeclarationView = {
    withHeaders(
      Get(s"/$RootPath/$PaymentPath/$DeclarationRoute")
    ) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[UboDeclarationView]
    }
  }
}
