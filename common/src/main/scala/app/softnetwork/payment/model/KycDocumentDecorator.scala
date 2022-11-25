package app.softnetwork.payment.model

trait KycDocumentDecorator { self: KycDocument =>
  lazy val view: KycDocumentView = KycDocumentView(self)
}

case class KycDocumentView(
  documentType: KycDocument.KycDocumentType,
  documentStatus: KycDocument.KycDocumentStatus,
  documentId: Option[String] = None,
  refusedReasonType: Option[String] = None,
  refusedReasonMessage: Option[String] = None,
  createdDate: Option[java.util.Date] = None,
  lastUpdated: Option[java.util.Date] = None
)

object KycDocumentView {
  def apply(kycDocument: KycDocument): KycDocumentView = {
    import kycDocument._
    KycDocumentView(
      `type`,
      status,
      id,
      refusedReasonType,
      refusedReasonMessage,
      createdDate,
      lastUpdated
    )
  }
}
