package app.softnetwork.payment.service

import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages.{
  Payment,
  PaymentCommand,
  PaymentCommandWithKey,
  PaymentResult
}
import app.softnetwork.payment.model.{computeExternalUuidWithProfile, BrowserInfo}
import app.softnetwork.persistence.service.Service
import org.softnetwork.session.model.Session

import java.util.TimeZone
import scala.concurrent.Future
import scala.reflect.ClassTag

trait BasicPaymentService extends Service[PaymentCommand, PaymentResult] {
  _: GenericPaymentHandler =>

  def run(command: PaymentCommandWithKey)(implicit
    tTag: ClassTag[PaymentCommand]
  ): Future[PaymentResult] =
    super.run(command.key, command)

  protected[payment] def externalUuidWithProfile(session: Session): String =
    computeExternalUuidWithProfile(session.id, session.profile)

  protected[payment] def extractBrowserInfo(
    language: Option[String],
    accept: Option[String],
    userAgent: Option[String],
    payment: Payment
  ): Option[BrowserInfo] = {
    import payment._
    if (
      language.isDefined &&
      accept.isDefined &&
      userAgent.isDefined &&
      colorDepth.isDefined &&
      screenWidth.isDefined &&
      screenHeight.isDefined
    ) {
      Some(
        BrowserInfo.defaultInstance.copy(
          colorDepth = colorDepth.get,
          screenWidth = screenWidth.get,
          screenHeight = screenHeight.get,
          acceptHeader = accept.get,
          javaEnabled = javaEnabled,
          javascriptEnabled = javascriptEnabled,
          language = "fr-FR" /*language.get.map(_.replace('_', '-'))*/,
          timeZoneOffset = "+" + (TimeZone
            .getTimeZone("Europe/Paris")
            .getRawOffset / (60 * 1000)),
          userAgent = userAgent.get
        )
      )
    } else {
      var missingParameters: Set[String] = Set.empty
      if (colorDepth.isEmpty)
        missingParameters += "colorDepth"
      if (screenWidth.isEmpty)
        missingParameters += "screenWidth"
      if (screenHeight.isEmpty)
        missingParameters += "screenHeight"
      if (missingParameters.nonEmpty)
        log.warn(
          s"Missing parameters ${missingParameters.mkString(", ")} will be mandatory"
        )

      var missingHeaders: Set[String] = Set.empty
      if (language.isEmpty)
        missingHeaders += "Accept-Language"
      if (accept.isEmpty)
        missingHeaders += "Accept"
      if (userAgent.isEmpty)
        missingHeaders += "User-Agent"
      if (missingHeaders.nonEmpty)
        log.warn(
          s"Missing Http headers ${missingHeaders.mkString(", ")} will be mandatory"
        )
      None
    }
  }

}
