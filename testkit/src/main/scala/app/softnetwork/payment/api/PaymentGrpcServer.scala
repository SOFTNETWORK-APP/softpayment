package app.softnetwork.payment.api

import akka.http.scaladsl.testkit.PersistenceScalatestGrpcTest
import app.softnetwork.payment.launch.PaymentGuardian
import app.softnetwork.persistence.scalatest.InMemoryPersistenceTestKit
import app.softnetwork.scheduler.launch.SchedulerGuardian
import org.scalatest.Suite

trait PaymentGrpcServer
    extends PersistenceScalatestGrpcTest
    with PaymentGrpcServices
    with InMemoryPersistenceTestKit { _: Suite with PaymentGuardian with SchedulerGuardian =>
  override lazy val additionalConfig: String = paymentGrpcConfig
}
