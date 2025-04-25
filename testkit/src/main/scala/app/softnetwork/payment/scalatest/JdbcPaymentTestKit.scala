package app.softnetwork.payment.scalatest

import akka.actor.testkit.typed.scaladsl.TestProbe
import akka.actor.typed.ActorSystem
import app.softnetwork.payment.message.TransactionEvents.TransactionUpdatedEvent
import app.softnetwork.payment.persistence.query.{
  JdbcPaymentAccountProvider,
  JdbcTransactionProvider,
  PaymentAccountToJdbcProcessStream,
  TransactionToJdbcProcessorStream
}
import app.softnetwork.persistence.jdbc.scalatest.JdbcPersistenceTestKit
import app.softnetwork.persistence.query.EventProcessorStream
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.Suite

trait JdbcPaymentTestKit extends PaymentTestKit with JdbcPersistenceTestKit { _: Suite =>

  override lazy val config: Config =
    ConfigFactory
      .parseString(slick)
      .withFallback(akkaConfig)
      .withFallback(ConfigFactory.load("softnetwork-in-memory-persistence.conf"))
      .withFallback(
        ConfigFactory.parseString(providerSettings)
      )
      .withFallback(ConfigFactory.load())

  def jdbcPaymentAccountProvider: JdbcPaymentAccountProvider

  def jdbcTransactionProvider: JdbcTransactionProvider

  def paymentAccountToJdbcProcessorStream: ActorSystem[_] => PaymentAccountToJdbcProcessStream

  def transactionToJdbcProcessorStream: ActorSystem[_] => TransactionToJdbcProcessorStream

  override def paymentEventProcessorStreams: ActorSystem[_] => Seq[EventProcessorStream[_]] = sys =>
    super.paymentEventProcessorStreams(sys) :+ paymentAccountToJdbcProcessorStream(
      sys
    ) :+ transactionToJdbcProcessorStream(sys)

  lazy val transactionProbe: TestProbe[TransactionUpdatedEvent] =
    createTestProbe[TransactionUpdatedEvent]()

  def subscribeJdbPaymentProbes(): Unit = {
    subscribeProbe(transactionProbe)
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    subscribeJdbPaymentProbes()
  }

}
