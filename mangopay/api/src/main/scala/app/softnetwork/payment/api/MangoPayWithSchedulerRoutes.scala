package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.server.Route
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.scheduler.launch.SchedulerRoutes

trait MangoPayWithSchedulerRoutes extends SchedulerRoutes with MangoPayRoutes { _: SchemaProvider =>
  override def apiRoutes(system: ActorSystem[_]): Route =
    super.apiRoutes(system) ~ schedulerService(system).route
}
