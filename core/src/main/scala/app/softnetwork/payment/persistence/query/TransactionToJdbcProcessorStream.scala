package app.softnetwork.payment.persistence.query

import app.softnetwork.payment.message.TransactionEvents.TransactionUpdatedEvent
import app.softnetwork.payment.model.Transaction
import app.softnetwork.persistence.jdbc.query.State2JdbcProcessorStream
import app.softnetwork.persistence.query.{JournalProvider, OffsetProvider}
import slick.jdbc.JdbcProfile

trait TransactionToJdbcProcessorStream
    extends State2JdbcProcessorStream[Transaction, TransactionUpdatedEvent]
    with JdbcTransactionProvider {
  _: JournalProvider with OffsetProvider with JdbcProfile =>

}
