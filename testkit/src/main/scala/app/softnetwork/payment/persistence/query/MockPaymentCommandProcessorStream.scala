package app.softnetwork.payment.persistence.query

import app.softnetwork.payment.handlers.MockPaymentHandler
import app.softnetwork.persistence.query.{JournalProvider, OffsetProvider}

trait MockPaymentCommandProcessorStream
    extends GenericPaymentCommandProcessorStream
    with MockPaymentHandler {
  _: JournalProvider with OffsetProvider =>
}
