package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.GrpcService
import app.softnetwork.api.server.scalatest.ServerTestKit
import app.softnetwork.payment.launch.PaymentGuardian
import app.softnetwork.scheduler.api.SchedulerGrpcServicesTestKit
import app.softnetwork.scheduler.launch.SchedulerGuardian

trait PaymentGrpcServicesTestKit extends SchedulerGrpcServicesTestKit with PaymentClientTestKit {
  _: PaymentGuardian with SchedulerGuardian with ServerTestKit =>

  override def grpcServices: ActorSystem[_] => Seq[GrpcService] = system =>
    paymentGrpcServices(system) ++ schedulerGrpcServices(system)

  def paymentGrpcConfig: String = schedulerGrpcConfig + s"""
                              |# Important: enable HTTP/2 in ActorSystem's config
                              |akka.http.server.preview.enable-http2 = on
                              |akka.grpc.client."${PaymentClient.name}"{
                              |    host = $interface
                              |    port = $port
                              |    use-tls = false
                              |}
                              |
                              |payment.client-id = "${settings.clientId}"
                              |
                              |payment.api-key = "${settings.apiKey}"
                              |
                              |""".stripMargin
}
