package app.softnetwork.payment.persistence.query

import app.softnetwork.payment.message.TransactionEvents.TransactionUpdatedEvent
import app.softnetwork.payment.model.Transaction
import app.softnetwork.persistence.ManifestWrapper
import app.softnetwork.persistence.query.{
  JournalProvider,
  OffsetProvider,
  State2ExternalProcessorStream
}

import scala.concurrent.ExecutionContext
import slick.jdbc.JdbcProfile

trait TransactionToJdbcProcessorStream
    extends State2ExternalProcessorStream[Transaction, TransactionUpdatedEvent]
    with ManifestWrapper[Transaction]
    with JdbcTransactionProvider {
  _: JournalProvider with OffsetProvider with JdbcProfile =>

  override val externalProcessor: String = "jdbc"
  override implicit lazy val executionContext: ExecutionContext = system.executionContext
  override protected def init(): Unit = initTable()
}
