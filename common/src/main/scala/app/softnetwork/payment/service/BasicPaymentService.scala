package app.softnetwork.payment.service

import app.softnetwork.api.server.{ApiErrors, HttpCorrelation}
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.BrowserInfo
import app.softnetwork.persistence.service.Service
import app.softnetwork.persistence.typed.scaladsl.EntityPattern
import org.slf4j.MDC

import java.util.TimeZone
import scala.concurrent.Future
import scala.reflect.ClassTag

trait BasicPaymentService extends Service[PaymentCommand, PaymentResult] {
  _: EntityPattern[PaymentCommand, PaymentResult] =>

  def run(command: PaymentCommandWithKey)(implicit
    tTag: ClassTag[PaymentCommand]
  ): Future[PaymentResult] = {
    // Story 13.7 — synchronous akka-http routes (PaymentService) build the command on the request
    // thread, where HttpCorrelation.withCorrelation has put the correlation id on MDC; stamp it onto
    // the command here, the single dispatch point. Guarded by isEmpty so a tapir endpoint that already
    // set it explicitly (its serverLogic runs in a Future where MDC does not survive — C14) is not
    // clobbered.
    if (command.correlationId.isEmpty)
      Option(MDC.get(HttpCorrelation.MdcKey)).filter(_.nonEmpty).foreach(command.withCorrelationId)
    super.run(command.key, command)
  }

  def error(result: PaymentResult): ApiErrors.ErrorInfo =
    result match {
      case PaymentAccountNotFound   => ApiErrors.NotFound(PaymentAccountNotFound.message)
      case MandateNotFound          => ApiErrors.NotFound(MandateNotFound)
      case UboDeclarationNotFound   => ApiErrors.NotFound(UboDeclarationNotFound)
      case BankAccountNotFound      => ApiErrors.NotFound(BankAccountNotFound)
      case TransactionNotFound      => ApiErrors.NotFound(TransactionNotFound.message)
      case UserNotFound             => ApiErrors.NotFound(UserNotFound)
      case WalletNotFound           => ApiErrors.NotFound(WalletNotFound)
      case CardNotFound             => ApiErrors.NotFound(CardNotFound)
      case RecurringPaymentNotFound => ApiErrors.NotFound(RecurringPaymentNotFound)
      case PaymentMethodNotPreRegistered =>
        ApiErrors.InternalServerError(PaymentMethodNotPreRegistered)
      case r: PreAuthorizationFailed => ApiErrors.InternalServerError(r)
      case r: PayInFailed            => ApiErrors.InternalServerError(r)
      case BillingPortalSessionNotCreated =>
        ApiErrors.InternalServerError(BillingPortalSessionNotCreated)
      case r: PaymentError => ApiErrors.BadRequest(r.message)
      case _               => ApiErrors.BadRequest("Unknown")
    }

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
