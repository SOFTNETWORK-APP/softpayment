package app.softnetwork.payment.model

import app.softnetwork.payment.model.UboDeclaration.UltimateBeneficialOwner

trait UltimateBeneficialOwnerDecorator {self: UltimateBeneficialOwner =>
  lazy val view: UltimateBeneficialOwnerView = UltimateBeneficialOwnerView(self)
}

case class UltimateBeneficialOwnerView()

object UltimateBeneficialOwnerView{
  def apply(ubo: UltimateBeneficialOwner): UltimateBeneficialOwnerView = {
    import ubo._
    UltimateBeneficialOwnerView()
  }
}