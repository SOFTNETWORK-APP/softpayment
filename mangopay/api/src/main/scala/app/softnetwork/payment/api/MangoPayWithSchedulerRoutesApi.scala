package app.softnetwork.payment.api

import app.softnetwork.persistence.schema.SchemaProvider

trait MangoPayWithSchedulerRoutesApi
    extends MangoPayWithSchedulerApi
    with MangoPayWithSchedulerRoutes { _: SchemaProvider => }
