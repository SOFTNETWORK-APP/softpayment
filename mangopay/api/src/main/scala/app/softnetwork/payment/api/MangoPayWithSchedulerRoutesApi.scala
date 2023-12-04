package app.softnetwork.payment.api

import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}

trait MangoPayWithSchedulerRoutesApi[SD <: SessionData with SessionDataDecorator[SD]]
    extends MangoPayWithSchedulerApi[SD]
    with MangoPayWithSchedulerRoutes[SD] { _: SchemaProvider => }
