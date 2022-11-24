package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import app.softnetwork.api.server.GrpcServices

import scala.concurrent.Future

trait PaymentGrpcServices extends GrpcServices {

  def interface: String

  def port: Int

  override def grpcServices: ActorSystem[_] => Seq[PartialFunction[HttpRequest, Future[HttpResponse]]] =
    system => Seq(
      PaymentServiceApiHandler.partial(MockPaymentServer(system))(system)
    )

  def grpcConfig: String = s"""
                              |# Important: enable HTTP/2 in ActorSystem's config
                              |akka.http.server.preview.enable-http2 = on
                              |akka.grpc.client."${PaymentClient.name}"{
                              |    host = $interface
                              |    port = $port
                              |    use-tls = false
                              |}
                              |""".stripMargin
}
