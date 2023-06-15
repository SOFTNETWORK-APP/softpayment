package app.softnetwork.payment.launch

import app.softnetwork.api.server.ApiRoutes
import app.softnetwork.api.server.launch.Application
import app.softnetwork.persistence.schema.SchemaProvider

trait PaymentApplication extends Application with ApiRoutes with PaymentGuardian {
  _: SchemaProvider =>
}
