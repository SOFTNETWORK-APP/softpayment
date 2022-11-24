package app.softnetwork.payment.api

import akka.http.scaladsl.testkit.PersistenceScalatestGrpcTest
import app.softnetwork.persistence.scalatest.InMemoryPersistenceTestKit
import org.scalatest.Suite

trait PaymentGrpcServer extends PersistenceScalatestGrpcTest with PaymentGrpcServices with InMemoryPersistenceTestKit { _: Suite =>
  override lazy val additionalConfig: String = grpcConfig
}
