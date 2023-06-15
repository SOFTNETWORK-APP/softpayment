package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.scheduler.launch.SchedulerEndpoints
import com.softwaremill.session.CsrfCheck
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait MangoPayWithSchedulerEndpoints extends SchedulerEndpoints with MangoPayEndpoints {
  _: SchemaProvider with CsrfCheck =>

  override def endpoints
    : ActorSystem[_] => List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    system => super.endpoints(system) ++ schedulerEndpoints(system).endpoints
}
