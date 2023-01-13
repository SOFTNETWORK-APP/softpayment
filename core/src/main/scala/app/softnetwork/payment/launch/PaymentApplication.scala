package app.softnetwork.payment.launch

import app.softnetwork.api.server.launch.Application
import app.softnetwork.persistence.query.SchemaProvider

trait PaymentApplication extends Application with PaymentRoutes { _: SchemaProvider => }
