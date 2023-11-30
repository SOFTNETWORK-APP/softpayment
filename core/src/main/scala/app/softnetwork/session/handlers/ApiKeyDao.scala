package app.softnetwork.session.handlers

import akka.actor.typed.ActorSystem
import app.softnetwork.session.model.ApiKey

import scala.concurrent.Future

trait ApiKeyDao {

  def loadApiKey(clientId: String)(implicit system: ActorSystem[_]): Future[Option[ApiKey]]

}
