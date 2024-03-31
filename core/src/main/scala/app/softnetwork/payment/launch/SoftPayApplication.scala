package app.softnetwork.payment.launch

import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck

trait SoftPayApplication extends PaymentApplication with SoftPayGuardian {
  _: SchemaProvider with CsrfCheck =>
}
