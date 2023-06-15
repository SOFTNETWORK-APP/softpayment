package app.softnetwork.payment.api

import app.softnetwork.persistence.schema.SchemaProvider

trait MangoPayRoutesApi extends MangoPayApi with MangoPayRoutes { _: SchemaProvider => }
