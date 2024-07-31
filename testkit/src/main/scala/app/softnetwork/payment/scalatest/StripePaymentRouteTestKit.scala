package app.softnetwork.payment.scalatest

import app.softnetwork.api.server.ApiRoutes
import app.softnetwork.security.sha256
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import org.scalatest.Suite

import sys.process._

trait StripePaymentRouteTestKit[SD <: SessionData with SessionDataDecorator[SD]]
    extends PaymentRouteTestKit[SD]
    with StripePaymentTestKit { _: Suite with ApiRoutes with SessionMaterials[SD] =>

  private[this] var stripeCLi: Process = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    val hash = sha256(clientId)
    stripeCLi = Process(
      s"stripe listen --forward-to ${providerConfig.hooksBaseUrl.replace("9000", s"$port").replace("localhost", interface)}?hash=$hash"
    ).run()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    if (stripeCLi.isAlive())
      stripeCLi.destroy()
  }

}
