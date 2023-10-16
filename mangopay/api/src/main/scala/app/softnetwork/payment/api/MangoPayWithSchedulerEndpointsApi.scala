package app.softnetwork.payment.api

import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck

trait MangoPayWithSchedulerEndpointsApi
    extends MangoPayWithSchedulerApi
    with MangoPayWithSchedulerEndpoints { _: SchemaProvider with CsrfCheck => }
