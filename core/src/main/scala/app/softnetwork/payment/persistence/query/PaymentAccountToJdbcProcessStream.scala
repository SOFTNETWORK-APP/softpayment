package app.softnetwork.payment.persistence.query

import app.softnetwork.payment.message.PaymentEvents.PaymentAccountUpsertedEvent
import app.softnetwork.payment.model.PaymentAccount
import app.softnetwork.persistence.jdbc.query.State2JdbcProcessorStream
import app.softnetwork.persistence.query.{JournalProvider, OffsetProvider}
import slick.jdbc.JdbcProfile

trait PaymentAccountToJdbcProcessStream
    extends State2JdbcProcessorStream[PaymentAccount, PaymentAccountUpsertedEvent]
    with JdbcPaymentAccountProvider {
  _: JournalProvider with OffsetProvider with JdbcProfile =>
}
