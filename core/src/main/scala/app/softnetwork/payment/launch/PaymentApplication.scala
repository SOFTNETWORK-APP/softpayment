package app.softnetwork.payment.launch

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.launch.Application
import app.softnetwork.persistence.query.SchemaProvider

trait PaymentApplication extends Application with PaymentRoutes { _: SchemaProvider =>
  override def initSystem: ActorSystem[_] => Unit = system => {
    initSchedulerSystem(system)
  }
}
