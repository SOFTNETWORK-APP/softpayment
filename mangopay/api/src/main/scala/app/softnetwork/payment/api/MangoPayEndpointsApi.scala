package app.softnetwork.payment.api

import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}

trait MangoPayEndpointsApi[SD <: SessionData with SessionDataDecorator[SD]]
    extends MangoPayApi[SD]
    with MangoPayEndpoints[SD] {
  _: SchemaProvider with CsrfCheck =>

}
