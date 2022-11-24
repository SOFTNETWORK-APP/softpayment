package app.softnetwork.payment.persistence.query

import app.softnetwork.payment.handlers.MangoPayPaymentHandler
import app.softnetwork.persistence.query.JournalProvider

trait MangoPayPaymentCommandProcessorStream extends GenericPaymentCommandProcessorStream with MangoPayPaymentHandler {
  _: JournalProvider =>
}
