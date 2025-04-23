package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.persistence.query.{
  PaymentAccountToJdbcProcessStream,
  TransactionToJdbcProcessorStream
}
import app.softnetwork.persistence.jdbc.scalatest.PostgresTestKit
import app.softnetwork.persistence.query.{InMemoryJournalProvider, InMemoryOffsetProvider}
import com.typesafe.config.Config
import org.scalatest.Suite
import slick.jdbc.PostgresProfile

trait PostgresPaymentTestKit extends JdbcPaymentTestKit with PostgresTestKit with PostgresProfile {
  _: Suite =>

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
