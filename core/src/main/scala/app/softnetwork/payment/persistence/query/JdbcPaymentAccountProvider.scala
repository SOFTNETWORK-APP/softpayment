package app.softnetwork.payment.persistence.query

import app.softnetwork.payment.model.{LegalUser, NaturalUser, PaymentAccount}
import app.softnetwork.payment.model.PaymentAccount.User
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.persistence.{model, ManifestWrapper}
import app.softnetwork.persistence.jdbc.query.JdbcStateProvider
import app.softnetwork.persistence.model.StateWrappertReader
import app.softnetwork.serialization.serialization
import org.json4s.Formats
import slick.jdbc.JdbcProfile

trait JdbcPaymentAccountProvider
    extends JdbcStateProvider[PaymentAccount]
    with ManifestWrapper[PaymentAccount] {
  _: JdbcProfile =>

  override implicit def formats: Formats = paymentFormats

  override protected val manifestWrapper: ManifestW = ManifestW()

  override def reader: StateWrappertReader[PaymentAccount] =
    new StateWrappertReader[PaymentAccount] {
      override protected val manifestWrapper: ManifestW = ManifestW()
    }

  override protected def writeToDb(
    document: model.StateWrapper[PaymentAccount],
    to_update: Boolean,
    data: Option[String]
  ): Boolean = {
    document.state match {
      case Some(state) =>
        super.writeToDb(document.copy(state = Some(state.clearTransactions)), to_update, data)
      case _ =>
        log.warn(s"Cannot write empty payment account to db")
        false
    }
  }

  override protected def readState(
    state: String
  )(implicit manifest: Manifest[PaymentAccount]): PaymentAccount = {
    var map = serialization.read[Map[String, Any]](state)
    var user: Option[User] = None
    val value: Map[String, Any] = map
      .getOrElse("user", Map.empty)
      .asInstanceOf[Map[String, Any]]
      .getOrElse("value", Map.empty)
      .asInstanceOf[Map[String, Any]]
    map = map - "user"
    if (value.contains("naturalUserType")) { // a specific case for natural user
      user = Option(
        User.NaturalUser(
          serialization.read[NaturalUser](
            serialization.write(value)
          )
        )
      )
    } else if (value.contains("legalUserType")) { // a specific case for legal user
      user = Option(
        User.LegalUser(
          serialization.read[LegalUser](
            serialization.write(value)
          )
        )
      )
    }
    var paymentAccount =
      serialization.read[PaymentAccount](
        serialization.write(map)
      )
    user match {
      case Some(u) => paymentAccount = paymentAccount.withUser(u)
      case _       =>
    }
    paymentAccount
  }
}
