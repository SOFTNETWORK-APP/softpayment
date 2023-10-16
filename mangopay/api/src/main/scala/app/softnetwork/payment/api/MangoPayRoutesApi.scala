package app.softnetwork.payment.api

import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck

trait MangoPayRoutesApi extends MangoPayApi with MangoPayRoutes {
  _: SchemaProvider with CsrfCheck =>
}
