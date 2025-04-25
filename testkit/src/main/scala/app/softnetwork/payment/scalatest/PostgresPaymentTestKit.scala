package app.softnetwork.payment.scalatest

import akka.actor
import akka.actor.typed.ActorSystem
import app.softnetwork.payment.persistence.query.{
  JdbcPaymentAccountProvider,
  JdbcTransactionProvider,
  PaymentAccountToJdbcProcessStream,
  TransactionToJdbcProcessorStream
}
import app.softnetwork.persistence.jdbc.scalatest.PostgresTestKit
import app.softnetwork.persistence.query.{InMemoryJournalProvider, InMemoryOffsetProvider}
import app.softnetwork.persistence.typed._
import com.typesafe.config.Config
import org.scalatest.Suite
import slick.jdbc.PostgresProfile

import scala.concurrent.ExecutionContext

trait PostgresPaymentTestKit extends JdbcPaymentTestKit with PostgresTestKit {
  _: Suite =>

  override lazy val jdbcPaymentAccountProvider: JdbcPaymentAccountProvider =
    new JdbcPaymentAccountProvider with PostgresProfile {
      override implicit def executionContext: ExecutionContext = classicSystem.dispatcher

      override implicit def classicSystem: actor.ActorSystem = typedSystem()

      override lazy val config: Config = PostgresPaymentTestKit.this.config
    }

  override lazy val jdbcTransactionProvider: JdbcTransactionProvider = new JdbcTransactionProvider
    with PostgresProfile {
    override implicit def executionContext: ExecutionContext = classicSystem.dispatcher

    override implicit def classicSystem: actor.ActorSystem = typedSystem()

    override lazy val config: Config = PostgresPaymentTestKit.this.config
  }

  override def paymentAccountToJdbcProcessorStream
    : ActorSystem[_] => PaymentAccountToJdbcProcessStream = sys =>
    new PaymentAccountToJdbcProcessStream
      with InMemoryJournalProvider
      with InMemoryOffsetProvider
      with PostgresProfile {
      override implicit def system: ActorSystem[_] = sys

      override val forTests: Boolean = true

      override lazy val config: Config = PostgresPaymentTestKit.this.config
    }

  override def transactionToJdbcProcessorStream
    : ActorSystem[_] => TransactionToJdbcProcessorStream = sys =>
    new TransactionToJdbcProcessorStream
      with InMemoryJournalProvider
      with InMemoryOffsetProvider
      with PostgresProfile {
      override implicit def system: ActorSystem[_] = sys

      override val forTests: Boolean = true

      override lazy val config: Config = PostgresPaymentTestKit.this.config
    }
}
