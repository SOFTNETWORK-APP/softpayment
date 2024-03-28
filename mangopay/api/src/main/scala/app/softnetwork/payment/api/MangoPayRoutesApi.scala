package app.softnetwork.payment.api

import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}

trait MangoPayRoutesApi[SD <: SessionData with SessionDataDecorator[SD]]
    extends MangoPayApi[SD]
    with MangoPayRoutes[SD] {
  _: SchemaProvider with CsrfCheck =>
}
