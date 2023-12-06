package app.softnetwork.payment.model

trait BankAccountCompanion {
  def apply(
    bankAccountId: Option[String],
    ownerName: String,
    ownerAddress: Address,
    iban: String,
    bic: String
  ): BankAccount = {
    BankAccount.defaultInstance
      .withBic(bic)
      .withIban(iban)
      .withOwnerName(ownerName)
      .withOwnerAddress(ownerAddress)
      .copy(id = bankAccountId)
  }
}
