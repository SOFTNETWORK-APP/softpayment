package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.Endpoint
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.scheduler.launch.SchedulerEndpoints
import app.softnetwork.session.CsrfCheck

trait MangoPayWithSchedulerEndpoints extends SchedulerEndpoints with MangoPayEndpoints {
  _: MangoPayWithSchedulerApi with SchemaProvider with CsrfCheck =>

  override def endpoints: ActorSystem[_] => List[Endpoint] = system =>
    super.endpoints(system) :+ schedulerEndpoints(system) :+ schedulerSwagger(system)
}
