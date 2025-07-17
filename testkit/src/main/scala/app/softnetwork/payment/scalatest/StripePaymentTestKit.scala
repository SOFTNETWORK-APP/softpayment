package app.softnetwork.payment.scalatest

import app.softnetwork.payment.config.{StripeApi, StripeSettings}
import app.softnetwork.payment.model.{BankAccount, PaymentAccount}
import com.google.gson.Gson
import com.stripe.model.Token
import com.stripe.param.TokenCreateParams
import org.scalatest.Suite

trait StripePaymentTestKit extends PaymentTestKit { _: Suite =>

  override implicit lazy val providerConfig: StripeApi.Config = StripeSettings.StripeApiConfig

  def createAccountToken(paymentAccount: PaymentAccount): Token = {
    if (paymentAccount.user.isNaturalUser) {
      paymentAccount.user.naturalUser match {
        case Some(naturalUser) =>
          val birthday = naturalUser.birthday.split("/")
          val individual =
            TokenCreateParams.Account.Individual
              .builder()
              .setFirstName(naturalUser.firstName)
              .setLastName(naturalUser.lastName)
              .setDob(
                TokenCreateParams.Account.Individual.Dob
                  .builder()
                  .setDay(birthday(0).toInt)
                  .setMonth(birthday(1).toInt)
                  .setYear(birthday(2).toInt)
                  .build()
              )
              .setEmail(naturalUser.email)
          naturalUser.phone match {
            case Some(phone) =>
              individual.setPhone(phone)
            case _ =>
          }
          naturalUser.address match {
            case Some(address) =>
              individual.setAddress(
                TokenCreateParams.Account.Individual.Address
                  .builder()
                  .setCity(address.city)
                  .setCountry(address.country)
                  .setLine1(address.addressLine)
                  .setPostalCode(address.postalCode)
                  .build()
              )
            case _ =>
          }
          val params = TokenCreateParams.Account
            .builder()
            .setBusinessType(TokenCreateParams.Account.BusinessType.INDIVIDUAL)
            .setIndividual(individual.build())
            .setTosShownAndAccepted(true)
          log.info(s"individual account token params -> ${new Gson().toJson(params.build())}")
          Token.create(
            TokenCreateParams
              .builder()
              .setAccount(params.build())
              .build(),
            StripeApi().requestOptions()
          )
        case _ => fail("Natural user is not defined for the payment account")
      }
    } else {
      paymentAccount.user.legalUser match {
        case Some(legalUser) =>
          val soleTrader = legalUser.legalUserType.isSoletrader
          val company =
            TokenCreateParams.Account.Company
              .builder()
              .setName(legalUser.legalName)
              .setAddress(
                TokenCreateParams.Account.Company.Address
                  .builder()
                  .setCity(legalUser.headQuartersAddress.city)
                  .setCountry(legalUser.headQuartersAddress.country)
                  .setLine1(legalUser.headQuartersAddress.addressLine)
                  .setPostalCode(legalUser.headQuartersAddress.postalCode)
                  .build()
              )
              .setTaxId(legalUser.siren)
              .setDirectorsProvided(true)
              .setExecutivesProvided(true)

          if (soleTrader) {
            company
              .setOwnersProvided(true)
          }

          legalUser.vatNumber match {
            case Some(vatNumber) =>
              company.setVatId(vatNumber)
            case _ =>
          }

          legalUser.phone.orElse(legalUser.legalRepresentative.phone) match {
            case Some(phone) =>
              company.setPhone(phone)
            case _ =>
          }

          val params =
            TokenCreateParams.Account
              .builder()
              .setBusinessType(TokenCreateParams.Account.BusinessType.COMPANY)
              .setCompany(company.build())
              .setTosShownAndAccepted(true)
          log.info(s"business account token params -> ${new Gson().toJson(params.build())}")
          Token.create(
            TokenCreateParams
              .builder()
              .setAccount(params.build())
              .build(),
            StripeApi().requestOptions()
          )
        case _ => fail("Legal user is not defined for the payment account")
      }
    }
  }

  def createBankToken(bankAccount: BankAccount, individual: Boolean): Token = {
    val params = TokenCreateParams.BankAccount
      .builder()
      .setAccountNumber(bankAccount.iban)
      .setRoutingNumber(bankAccount.bic)
      .setCountry(
        bankAccount.countryCode.getOrElse(bankAccount.ownerAddress.country)
      )
      .setCurrency(bankAccount.currency.getOrElse("EUR"))
      .setAccountHolderName(bankAccount.ownerName)
      .setAccountHolderType(if (individual) {
        TokenCreateParams.BankAccount.AccountHolderType.INDIVIDUAL
      } else {
        TokenCreateParams.BankAccount.AccountHolderType.COMPANY
      })
    log.info(s"bank account token params -> ${new Gson().toJson(params.build())}")
    Token.create(
      TokenCreateParams
        .builder()
        .setBankAccount(params.build())
        .build(),
      StripeApi().requestOptions()
    )
  }

}
