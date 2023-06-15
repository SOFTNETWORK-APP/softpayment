package app.softnetwork.payment.api

import app.softnetwork.persistence.schema.SchemaProvider
import com.softwaremill.session.CsrfCheck

trait MangoPayEndpointsApi extends MangoPayApi with MangoPayEndpoints {
  _: SchemaProvider with CsrfCheck =>

}
