package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.ApiEndpoint
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.scheduler.launch.SchedulerEndpoints
import com.softwaremill.session.CsrfCheck

trait MangoPayWithSchedulerEndpoints extends SchedulerEndpoints with MangoPayEndpoints {
  _: SchemaProvider with CsrfCheck =>

  override def endpoints: ActorSystem[_] => List[ApiEndpoint] =
    system => super.endpoints(system) :+ schedulerEndpoints(system)
}
