package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.{GrpcService, GrpcServices}
import app.softnetwork.payment.launch.PaymentGuardian

trait PaymentGrpcServices extends GrpcServices { _: PaymentGuardian =>
  override def grpcServices: ActorSystem[_] => Seq[GrpcService] = system =>
    paymentGrpcServices(system)
}
