package app.softnetwork.payment.launch

import app.softnetwork.api.server.ApiRoutes
import app.softnetwork.api.server.launch.Application
import app.softnetwork.payment.api.PaymentGrpcServices
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck

trait PaymentApplication
    extends Application
    with ApiRoutes
    with PaymentGuardian
    with PaymentGrpcServices {
  _: SchemaProvider with CsrfCheck =>
  override val applicationVersion = systemVersion()
}
