package app.softnetwork.payment.model

import app.softnetwork.payment.model
import com.typesafe.scalalogging.StrictLogging

import java.text.SimpleDateFormat
import java.util.Date
import scala.util.{Failure, Success, Try}

trait CardDecorator extends StrictLogging { self: Card =>
  lazy val expired: Boolean = {
    val pattern =
      expirationDate.split("/").toSeq match {
        case Seq(_, y) =>
          if (y.length == 2) {
            "MM/yy"
          } else {
            "MM/yyyy"
          }
        case _ =>
          "MMyy"
      }
    val sdf = new SimpleDateFormat(pattern)
    Try(sdf.parse(expirationDate)) match {
      case Success(date) =>
        sdf.parse(sdf.format(new Date())).after(date)
      case Failure(f) =>
        logger.error(s"Error parsing expiration date $expirationDate: ${f.getMessage}")
        true
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
