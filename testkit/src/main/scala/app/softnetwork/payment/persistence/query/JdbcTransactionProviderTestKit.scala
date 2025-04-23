package app.softnetwork.payment.persistence.query

import app.softnetwork.persistence.jdbc.scalatest.JdbcPersistenceTestKit
import slick.jdbc.JdbcProfile

import scala.concurrent.ExecutionContext

trait JdbcTransactionProviderTestKit extends JdbcTransactionProvider {
  _: JdbcPersistenceTestKit with JdbcProfile =>
  override implicit def executionContext: ExecutionContext = system.executionContext
}
