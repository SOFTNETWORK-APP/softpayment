package app.softnetwork.payment.api

import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}

trait MangoPayWithSchedulerEndpointsApi[SD <: SessionData with SessionDataDecorator[SD]]
    extends MangoPayWithSchedulerApi[SD]
    with MangoPayWithSchedulerEndpoints[SD] { _: SchemaProvider with CsrfCheck => }