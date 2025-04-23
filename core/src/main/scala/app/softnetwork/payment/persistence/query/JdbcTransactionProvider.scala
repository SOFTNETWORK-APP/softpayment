package app.softnetwork.payment.persistence.query

import app.softnetwork.payment.model.Transaction
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.persistence.jdbc.query.JdbcStateProvider
import app.softnetwork.persistence.model.StateWrappertReader
import app.softnetwork.persistence.ManifestWrapper
import org.json4s.Formats
import slick.jdbc.JdbcProfile

trait JdbcTransactionProvider
    extends JdbcStateProvider[Transaction]
    with ManifestWrapper[Transaction] {
  _: JdbcProfile =>

  override implicit def formats: Formats = paymentFormats

  override protected val manifestWrapper: ManifestW = ManifestW()

  override def reader: StateWrappertReader[Transaction] =
    new StateWrappertReader[Transaction] {
      override protected val manifestWrapper: ManifestW = ManifestW()
    }

}
