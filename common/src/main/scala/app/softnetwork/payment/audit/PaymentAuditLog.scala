package app.softnetwork.payment.audit

import app.softnetwork.persistence.audit.AuditLog

object PaymentAuditLog {

  /** Story 13.7 — structured audit trail for the payment pod. service = "payment"; the
    * `correlationId` is threaded as data (proto field on the transaction events / `AuditableCommand`),
    * never via MDC — the emission points are `thenRun` continuations of the persistence `Effect`,
    * where a `ThreadLocal` MDC value would not survive.
    */
  private[payment] lazy val audit: AuditLog = AuditLog("payment")

}
