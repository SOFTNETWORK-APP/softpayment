package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.ApiRoute
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.scheduler.launch.SchedulerRoutes

trait MangoPayWithSchedulerRoutes extends SchedulerRoutes with MangoPayRoutes {
  _: MangoPayWithSchedulerApi with SchemaProvider =>
  override def apiRoutes: ActorSystem[_] => List[ApiRoute] =
    system => super.apiRoutes(system) :+ schedulerService(system) :+ schedulerSwagger(system)
}
