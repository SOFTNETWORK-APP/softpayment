package app.softnetwork.payment.api

import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck

trait MangoPayEndpointsApi extends MangoPayApi with MangoPayEndpoints {
  _: SchemaProvider with CsrfCheck =>

}
