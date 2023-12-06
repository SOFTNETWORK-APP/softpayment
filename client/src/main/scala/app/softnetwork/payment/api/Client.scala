package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.grpc.GrpcClientSettings
import app.softnetwork.api.server.client.{GrpcClient, GrpcClientFactory}
import app.softnetwork.payment.api.config.PaymentClientSettings

import scala.concurrent.Future

trait Client extends GrpcClient {

  implicit lazy val grpcClient: ClientServiceApiClient =
    ClientServiceApiClient(
      GrpcClientSettings.fromConfig(name)
    )

  lazy val settings: PaymentClientSettings = PaymentClientSettings(system)

  def generateClientTokens(
    scope: Option[String] = None
  ): Future[Option[Tokens]] = {
    import settings._
    grpcClient
      .generateClientTokens()
      .invoke(
        GenerateClientTokensRequest(clientId, apiKey, scope)
      ) map (response =>
      response.clientTokens match {
        case r: ClientTokensResponse.ClientTokens.Tokens =>
          r.tokens
        case _ => None
      }
    )
  }

  def refreshClientTokens(refreshToken: String): Future[Option[Tokens]] = {
    grpcClient
      .refreshClientTokens()
      .invoke(
        RefreshClientTokensRequest(refreshToken)
      ) map (response =>
      response.clientTokens match {
        case r: ClientTokensResponse.ClientTokens.Tokens =>
          r.tokens
        case _ => None
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
