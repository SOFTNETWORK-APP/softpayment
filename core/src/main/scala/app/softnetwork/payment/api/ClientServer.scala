package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.account.message.Activate
import app.softnetwork.payment.handlers.SoftPayAccountDao
import app.softnetwork.payment.message.AccountMessages
import app.softnetwork.payment.model.SoftPayAccount
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContextExecutor, Future}

trait ClientServer extends ClientServiceApi with SoftPayAccountDao {

  implicit def system: ActorSystem[_]

  implicit lazy val ec: ExecutionContextExecutor = system.executionContext

  override def generateClientTokens(
    in: GenerateClientTokensRequest
  ): Future[ClientTokensResponse] = {
    import in._
    generateClientTokens(clientId, clientSecret, scope) map {
      case Right(tokens) =>
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
      case Left(error) => ClientTokensResponse.defaultInstance.withError(error)
    }
  }

  override def refreshClientTokens(in: RefreshClientTokensRequest): Future[ClientTokensResponse] = {
    import in._
    refreshClientTokens(refreshToken) map {
      case Right(tokens) =>
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
      case Left(error) => ClientTokensResponse.defaultInstance.withError(error)
    }
  }

  override def signUpClient(in: SignUpClientRequest): Future[SignUpClientResponse] = {
    import in._
    signUpClient(
      AccountMessages.SoftPaySignUp(
        principal,
        credentials,
        SoftPayAccount.Client.Provider(
          providerId,
          providerApiKey,
          SoftPayAccount.Client.Provider.ProviderType
            .fromName(providerType.name)
            .getOrElse(SoftPayAccount.Client.Provider.ProviderType.MANGOPAY)
        )
      )
    ) map {
      case Right(client) =>
        import client._
        SignUpClientResponse.defaultInstance.withClient(
          ClientCreated(
            clientId,
            clientApiKey.getOrElse("")
          )
        )
      case Left(error) => SignUpClientResponse.defaultInstance.withError(error)
    }
  }

  override def activateClient(in: ActivateClientRequest): Future[ActivateClientResponse] = {
    import in._
    activateClient(Activate(token)) map {
      case Right(activated) => ActivateClientResponse.defaultInstance.withActivated(activated)
      case Left(error) =>
        ActivateClientResponse.defaultInstance.withError(error)
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
