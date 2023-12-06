package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.handlers.{PaymentHandler, SoftPaymentAccountDao}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContextExecutor, Future}

trait ClientServer extends ClientServiceApi with PaymentHandler {

  implicit def system: ActorSystem[_]

  implicit lazy val ec: ExecutionContextExecutor = system.executionContext

  def softPaymentAccountDao: SoftPaymentAccountDao = SoftPaymentAccountDao

  override def generateClientTokens(
    in: GenerateClientTokensRequest
  ): Future[ClientTokensResponse] = {
    import in._
    softPaymentAccountDao.generateClientToken(clientId, clientSecret, scope) map {
      case Some(tokens) =>
        import tokens._
        ClientTokensResponse.defaultInstance.withTokens(
          Tokens(
            access_token,
            token_type,
            expires_in,
            refresh_token
          )
        )
      case _ => ClientTokensResponse.defaultInstance.withError("unknown")
    }
  }

  override def refreshClientTokens(in: RefreshClientTokensRequest): Future[ClientTokensResponse] = {
    import in._
    softPaymentAccountDao.refreshClientToken(refreshToken) map {
      case Some(tokens) =>
        import tokens._
        ClientTokensResponse.defaultInstance.withTokens(
          Tokens(
            access_token,
            token_type,
            expires_in,
            refresh_token
          )
        )
      case _ => ClientTokensResponse.defaultInstance.withError("unknown")
    }
  }
}

object ClientServer {

  def apply(sys: ActorSystem[_]): ClientServer = {
    new ClientServer {
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override implicit val system: ActorSystem[_] = sys
    }
  }
}
