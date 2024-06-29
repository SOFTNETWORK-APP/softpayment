package app.softnetwork.payment.service

import akka.http.scaladsl.server.{Directives, Route}
import app.softnetwork.api.server.DefaultComplete
import app.softnetwork.payment.message.PaymentMessages.{PaymentCommand, PaymentResult}
import app.softnetwork.persistence.typed.scaladsl.EntityPattern
import de.heikoseeberger.akkahttpjson4s.Json4sSupport

trait HooksDirectives
    extends Directives
    with DefaultComplete
    with Json4sSupport
    with BasicPaymentService { _: EntityPattern[PaymentCommand, PaymentResult] =>
  def hooks: Route

}
