package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ContentTypes, Multipart, StatusCodes}
import app.softnetwork.api.server.ApiRoutes
import app.softnetwork.api.server.config.ServerSettings.RootPath
import app.softnetwork.payment.api.{PaymentClient, PaymentGrpcServicesTestKit}
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.config.PaymentSettings.PaymentConfig._
import app.softnetwork.payment.data.{customerUuid, sellerUuid}
import app.softnetwork.payment.model.SoftPayAccount.Client.Provider
import app.softnetwork.payment.model._
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.scalatest.SessionTestKit
import app.softnetwork.session.service.SessionMaterials
import org.scalatest.Suite

import java.nio.file.Paths

trait PaymentRouteTestKit[SD <: SessionData with SessionDataDecorator[SD]]
    extends SessionTestKit[SD]
    with PostgresPaymentTestKit
    with PaymentGrpcServicesTestKit {
  _: Suite with ApiRoutes with SessionMaterials[SD] =>

  lazy val paymentClient: PaymentClient = PaymentClient(ts)

  lazy val customerSession: SD with SessionDataDecorator[SD] =
    companion.newSession.withId(customerUuid).withProfile("customer").withClientId(clientId)

  var externalUserId: String = "individual"

  def sellerSession(id: String = sellerUuid): SD with SessionDataDecorator[SD] =
    companion.newSession.withId(id).withProfile("seller").withClientId(clientId)

  import app.softnetwork.serialization._

  override lazy val additionalConfig: String = paymentGrpcConfig

  override implicit lazy val ts: ActorSystem[_] = typedSystem()

  /*
  override def beforeAll(): Unit = {
    initAndJoinCluster()
    // pre load routes
    mainRoutes(typedSystem())
  }
   */

  def loadPaymentAccount(): PaymentAccountView = {
    withHeaders(
      Get(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$accountRoute")
    ) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[PaymentAccountView]
    }
  }

  def loadCards(): Seq[Card] = {
    withHeaders(
      Get(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$cardRoute")
    ) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[Seq[Card]]
    }
  }

  def loadPaymentMethods(): PaymentMethodsView = {
    withHeaders(
      Get(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$paymentMethodRoute")
    ) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[PaymentMethodsView]
    }
  }

  def loadBankAccount(): BankAccountView = {
    withHeaders(
      Get(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$bankRoute")
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
            s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$kycRoute?documentType=$documentType",
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
      Get(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$kycRoute?documentType=$documentType")
    ) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[KycDocumentValidationReport]
    }
  }

  def validateKycDocuments(): Unit = {
    loadPaymentAccount().documents
      .filterNot(_.documentStatus == KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATED)
      .map(_.documentType)
      .foreach { documentType =>
        withHeaders(
          Get(
            s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$kycRoute?documentType=$documentType"
          )
        ) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val report = responseAs[KycDocumentValidationReport]
          assert(report.status != KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATED)
          Get(
            s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$hooksRoute/${Provider.ProviderType.MOCK.name.toLowerCase}?EventType=KYC_SUCCEEDED&RessourceId=${report.id}"
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
      Get(s"/$RootPath/${PaymentSettings.PaymentConfig.path}/$declarationRoute")
    ) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[UboDeclarationView]
    }
  }
}
