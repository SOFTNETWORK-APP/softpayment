package app.softnetwork.payment.model

import app.softnetwork.payment.model

import java.text.SimpleDateFormat
import java.util.Date
import scala.util.{Failure, Success, Try}

trait CardDecorator { self: Card =>
  lazy val expired: Boolean = {
    val sdf = new SimpleDateFormat("MMyy")
    Try(sdf.parse(expirationDate)) match {
      case Success(date) =>
        sdf.parse(sdf.format(new Date())).after(date)
      case Failure(_) =>
        false
    }
  }

  lazy val owner: CardOwner =
    CardOwner.defaultInstance
      .withFirstName(firstName)
      .withLastName(lastName)
      .withBirthday(birthday)

  lazy val view: CardView = model.CardView(self)
}

case class CardView(
  id: String,
  firstName: String,
  lastName: String,
  birthday: String,
  alias: String,
  expirationDate: String,
  active: Boolean,
  expired: Boolean
)

object CardView {
  def apply(card: Card): CardView = {
    import card._
    CardView(
      id,
      firstName,
      lastName,
      birthday,
      alias,
      expirationDate,
      active.getOrElse(true),
      expired
    )
  }
}
