package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.message.TransactionEvents.TransactionUpdatedEvent
import app.softnetwork.payment.persistence.query.{
  JdbcTransactionProvider,
  JdbcTransactionProviderTestKit,
  PaymentAccountToJdbcProcessStream,
  TransactionToJdbcProcessorStream
}
import app.softnetwork.persistence.jdbc.scalatest.JdbcPersistenceTestKit
import app.softnetwork.persistence.query.EventProcessorStream
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.Suite
import slick.jdbc.JdbcProfile

trait JdbcPaymentTestKit
    extends PaymentTestKit
    with JdbcPersistenceTestKit
    with JdbcTransactionProviderTestKit { _: Suite with JdbcProfile =>

  override lazy val config: Config = akkaConfig
    .withFallback(ConfigFactory.load("softnetwork-in-memory-persistence.conf"))
    .withFallback(
      ConfigFactory.parseString(providerSettings)
    )
    .withFallback(
      ConfigFactory.parseString(slick)
    )
    .withFallback(ConfigFactory.load())

  lazy val jdbcTransactionProvider: JdbcTransactionProvider = this

  def paymentAccountToJdbcProcessorStream: ActorSystem[_] => PaymentAccountToJdbcProcessStream

  def transactionToJdbcProcessorStream: ActorSystem[_] => TransactionToJdbcProcessorStream

  override def paymentEventProcessorStreams: ActorSystem[_] => Seq[EventProcessorStream[_]] = sys =>
    super.paymentEventProcessorStreams(sys) :+ paymentAccountToJdbcProcessorStream(
      sys
    ) :+ transactionToJdbcProcessorStream(sys)

  lazy val transactionProbe = createTestProbe[TransactionUpdatedEvent]()

  def subscribeJdbPaymentProbes(): Unit = {
    subscribeProbe(transactionProbe)
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    subscribeJdbPaymentProbes()
  }

}
