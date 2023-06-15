package app.softnetwork.payment.api

import app.softnetwork.persistence.schema.SchemaProvider
import com.softwaremill.session.CsrfCheck

trait MangoPayWithSchedulerEndpointsApi
    extends MangoPayWithSchedulerApi
    with MangoPayWithSchedulerEndpoints { _: SchemaProvider with CsrfCheck => }
