package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.GrpcService
import app.softnetwork.api.server.scalatest.ServerTestKit
import app.softnetwork.payment.launch.PaymentGuardian
import app.softnetwork.persistence.scalatest.PersistenceTestKit
import app.softnetwork.scheduler.api.SchedulerGrpcServicesTestKit
import app.softnetwork.scheduler.launch.SchedulerGuardian

trait PaymentGrpcServicesTestKit extends SchedulerGrpcServicesTestKit with PaymentProviderTestKit {
  _: PaymentGuardian with SchedulerGuardian with ServerTestKit with PersistenceTestKit =>

  override def grpcServices: ActorSystem[_] => Seq[GrpcService] = system =>
    paymentGrpcServices(system) ++ schedulerGrpcServices(system)

  def paymentGrpcConfig: String = schedulerGrpcConfig + s"""
                              |# Important: enable HTTP/2 in ActorSystem's config
                              |akka.http.server.preview.enable-http2 = on
                              |
                              |akka.grpc.client."${Client.name}"{
                              |    host = $interface
                              |    port = $port
                              |    use-tls = false
                              |}
                              |
                              |akka.grpc.client."${PaymentClient.name}"{
                              |    host = $interface
                              |    port = $port
                              |    use-tls = false
                              |}
                              |payment.baseUrl = "http://$interface:$port/api"
                              |""".stripMargin + providerSettings
}
