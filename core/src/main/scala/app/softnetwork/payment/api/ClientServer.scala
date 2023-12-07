package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.handlers.SoftPaymentAccountDao
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContextExecutor, Future}

trait ClientServer extends ClientServiceApi with SoftPaymentAccountDao {

  implicit def system: ActorSystem[_]

  implicit lazy val ec: ExecutionContextExecutor = system.executionContext

  override def generateClientTokens(
    in: GenerateClientTokensRequest
  ): Future[ClientTokensResponse] = {
    import in._
    generateClientToken(clientId, clientSecret, scope) map {
      case Some(tokens) =>
        import tokens._
        ClientTokensResponse.defaultInstance.withTokens(
          Tokens(
            access_token,
            token_type,
            expires_in,
            refresh_token,
            refresh_token_expires_in.map(_.toLong)
          )
        )
      case _ => ClientTokensResponse.defaultInstance.withError("unknown")
    }
  }

  override def refreshClientTokens(in: RefreshClientTokensRequest): Future[ClientTokensResponse] = {
    import in._
    refreshClientToken(refreshToken) map {
      case Some(tokens) =>
        import tokens._
        ClientTokensResponse.defaultInstance.withTokens(
          Tokens(
            access_token,
            token_type,
            expires_in,
            refresh_token,
            refresh_token_expires_in.map(_.toLong)
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
