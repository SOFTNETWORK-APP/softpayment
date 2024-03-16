package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.grpc.GrpcClientSettings
import app.softnetwork.api.server.client.{GrpcClient, GrpcClientFactory}
import app.softnetwork.payment.api.config.SoftPayClientSettings

import scala.concurrent.Future

trait Client extends GrpcClient {

  implicit lazy val grpcClient: ClientServiceApiClient =
    ClientServiceApiClient(
      GrpcClientSettings.fromConfig(name)
    )

  lazy val settings: SoftPayClientSettings = SoftPayClientSettings(system)

  def generateClientTokens(
    scope: Option[String] = None
  ): Future[Either[String, Option[Tokens]]] = {
    import settings._
    grpcClient
      .generateClientTokens()
      .invoke(
        GenerateClientTokensRequest(clientId, apiKey, scope)
      ) map (response =>
      response.clientTokens match {
        case r: ClientTokensResponse.ClientTokens.Tokens =>
          Right(r.tokens)
        case error: ClientTokensResponse.ClientTokens.Error => Left(error.value)
      }
    )
  }

  def refreshClientTokens(refreshToken: String): Future[Either[String, Option[Tokens]]] = {
    grpcClient
      .refreshClientTokens()
      .invoke(
        RefreshClientTokensRequest(refreshToken)
      ) map (response =>
      response.clientTokens match {
        case r: ClientTokensResponse.ClientTokens.Tokens =>
          Right(r.tokens)
        case error: ClientTokensResponse.ClientTokens.Error => Left(error.value)
      }
    )
  }

  def signUpClient(
    principal: String,
    credentials: Array[Char],
    providerId: String,
    providerApiKey: Array[Char],
    providerType: Option[ProviderType]
  ): Future[Either[String, ClientCreated]] = {
    grpcClient
      .signUpClient()
      .invoke(
        SignUpClientRequest(
          principal,
          credentials.mkString,
          providerId,
          providerApiKey.mkString,
          providerType.getOrElse(ProviderType.MANGOPAY)
        )
      ) map (response =>
      response.signup match {
        case r: SignUpClientResponse.Signup.Client =>
          Right(r.value)
        case error: SignUpClientResponse.Signup.Error => Left(error.value)
      }
    )
  }

  def activateClient(token: String): Future[Either[String, Boolean]] = {
    grpcClient
      .activateClient()
      .invoke(
        ActivateClientRequest(token)
      ) map (response =>
      response.activation match {
        case r: ActivateClientResponse.Activation.Activated => Right(r.value)
        case error: ActivateClientResponse.Activation.Error => Left(error.value)
      }
    )
  }
}

object Client extends GrpcClientFactory[Client] {
  override val name: String = "ClientService"
  override def init(sys: ActorSystem[_]): Client = {
    new Client {
      override implicit lazy val system: ActorSystem[_] = sys
      val name: String = Client.name
    }
  }
}
