package app.softnetwork.payment.api

import app.softnetwork.api.server.GrpcServer
import app.softnetwork.persistence.scalatest.InMemoryPersistenceTestKit
import org.scalatest.Suite

trait PaymentGrpcServer extends GrpcServer with PaymentGrpcServices with InMemoryPersistenceTestKit { _: Suite =>
  override lazy val additionalConfig: String = grpcConfig
}
