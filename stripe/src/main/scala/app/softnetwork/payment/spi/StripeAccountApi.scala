package app.softnetwork.payment.spi

import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.model
import app.softnetwork.payment.model.UboDeclaration.UltimateBeneficialOwner
import app.softnetwork.payment.model.{
  BankAccount,
  KycDocument,
  KycDocumentValidationReport,
  LegalUser,
  NaturalUser,
  UboDeclaration
}
import app.softnetwork.persistence
import app.softnetwork.time
import app.softnetwork.time.dateToInstant
import com.google.gson.Gson
import com.stripe.Stripe
import com.stripe.model.{Customer, Token}
import com.stripe.param.{
  AccountListParams,
  CustomerCreateParams,
  CustomerListParams,
  CustomerUpdateParams,
  ExternalAccountCollectionCreateParams,
  ExternalAccountCollectionListParams,
  TokenCreateParams
}

//import com.stripe.model.identity.VerificationReport
import com.stripe.model.{Account, File, Person}
//import com.stripe.param.identity.VerificationReportListParams
import com.stripe.param.{
  AccountCreateParams,
  AccountUpdateParams,
  FileCreateParams,
  PersonCollectionCreateParams,
  PersonCollectionListParams,
  PersonUpdateParams
}

import java.io.ByteArrayInputStream
import java.text.SimpleDateFormat
import java.util.{Calendar, TimeZone}
import scala.util.{Failure, Success, Try}
import collection.JavaConverters._
import scala.language.implicitConversions

trait StripeAccountApi extends PaymentAccountApi { _: StripeContext =>

  implicit def personToUbo(person: Person): UboDeclaration.UltimateBeneficialOwner = {
    UboDeclaration.UltimateBeneficialOwner.defaultInstance
      .withId(person.getId)
      .withFirstName(person.getFirstName)
      .withLastName(person.getLastName)
      .withAddress(person.getAddress.getLine1)
      .withCity(person.getAddress.getCity)
      .withCountry(person.getAddress.getCountry)
      .withPostalCode(person.getAddress.getPostalCode)
      .withBirthday(
        s"${person.getDob.getMonth}/${person.getDob.getDay}/${person.getDob.getYear}"
      )
      .copy(
        percentOwnership = Option(person.getRelationship.getPercentOwnership).map(_.doubleValue())
      )
  }

  private[this] def pagesToFiles(
    pages: Seq[Array[Byte]],
    documentType: KycDocument.KycDocumentType
  ): Try[Seq[File]] = {
    val filesCreateParams = pages.map(page => {
      FileCreateParams
        .builder()
        .setPurpose(
          documentType match {
            case KycDocument.KycDocumentType.KYC_IDENTITY_PROOF =>
              FileCreateParams.Purpose.IDENTITY_DOCUMENT
            case KycDocument.KycDocumentType.KYC_ADDRESS_PROOF =>
              FileCreateParams.Purpose.ADDITIONAL_VERIFICATION
            /*case KycDocument.KycDocumentType.KYC_SHAREHOLDER_DECLARATION => //ubo declaration
              FileCreateParams.Purpose.ACCOUNT_REQUIREMENT
            case KycDocument.KycDocumentType.KYC_ARTICLES_OF_ASSOCIATION => //legal representative document
              FileCreateParams.Purpose.ACCOUNT_REQUIREMENT*/
            case KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF => //company registration document
              FileCreateParams.Purpose.ACCOUNT_REQUIREMENT
            case _ => FileCreateParams.Purpose.ADDITIONAL_VERIFICATION
          }
        )
        .setFile(new ByteArrayInputStream(page))
        .build()
    })
    Try(
      filesCreateParams.map(fileCreateParams =>
        File.create(fileCreateParams, StripeApi().requestOptions)
      )
    )
  }

  /** @param maybeNaturalUser
    *   - natural user to create
    * @return
    *   provider user id
    */
  @InternalApi
  private[spi] override def createOrUpdateNaturalUser(
    maybeNaturalUser: Option[NaturalUser],
    acceptedTermsOfPSP: Boolean,
    ipAddress: Option[String],
    userAgent: Option[String]
  ): Option[String] = {
    maybeNaturalUser match {
      case Some(naturalUser) =>
        // create or update natural user
        val birthday = naturalUser.birthday
        val sdf = new SimpleDateFormat("dd/MM/yyyy")
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"))
        Try(sdf.parse(birthday)) match {
          case Success(date) =>
            val customer =
              naturalUser.naturalUserType.contains(model.NaturalUser.NaturalUserType.PAYER)
            if (customer) {
              createOrUpdateCustomer(naturalUser)
            } else {
              val tos_shown_and_accepted =
                acceptedTermsOfPSP && ipAddress.isDefined && userAgent.isDefined
              val c = Calendar.getInstance()
              c.setTime(date)
              Try(
                (naturalUser.userId match {
                  case Some(userId) if userId.startsWith("acct_") =>
                    Option(Account.retrieve(userId, StripeApi().requestOptions))
                  case _ =>
                    Account
                      .list(
                        AccountListParams
                          .builder()
                          .setLimit(100)
                          .build(),
                        StripeApi().requestOptions
                      )
                      .getData
                      .asScala
                      .find(acc => acc.getMetadata.get("external_uuid") == naturalUser.externalUuid)
                }) match {
                  case Some(account) =>
                    mlog.info(s"account -> ${new Gson().toJson(account)}")

                    // update account
                    val individual =
                      TokenCreateParams.Account.Individual
                        .builder()
                        .setFirstName(naturalUser.firstName)
                        .setLastName(naturalUser.lastName)
                        .setDob(
                          TokenCreateParams.Account.Individual.Dob
                            .builder()
                            .setDay(c.get(Calendar.DAY_OF_MONTH))
                            .setMonth(c.get(Calendar.MONTH) + 1)
                            .setYear(c.get(Calendar.YEAR))
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
                    val token = Token.create(
                      TokenCreateParams
                        .builder()
                        .setAccount(
                          TokenCreateParams.Account
                            .builder()
                            .setBusinessType(TokenCreateParams.Account.BusinessType.INDIVIDUAL)
                            .setIndividual(individual.build())
                            .setTosShownAndAccepted(tos_shown_and_accepted)
                            .build
                        )
                        .build(),
                      StripeApi().requestOptions
                    )
                    val params =
                      AccountUpdateParams
                        .builder()
                        .setAccountToken(token.getId)
                        .setCapabilities(
                          AccountUpdateParams.Capabilities
                            .builder()
                            .setBankTransferPayments(
                              AccountUpdateParams.Capabilities.BankTransferPayments
                                .builder()
                                .setRequested(true)
                                .build()
                            )
                            .setCardPayments(
                              AccountUpdateParams.Capabilities.CardPayments
                                .builder()
                                .setRequested(true)
                                .build()
                            )
                            .setCartesBancairesPayments(
                              AccountUpdateParams.Capabilities.CartesBancairesPayments
                                .builder()
                                .setRequested(true)
                                .build()
                            )
                            .setTransfers(
                              AccountUpdateParams.Capabilities.Transfers
                                .builder()
                                .setRequested(true)
                                .build()
                            )
                            .setSepaBankTransferPayments(
                              AccountUpdateParams.Capabilities.SepaBankTransferPayments
                                .builder()
                                .setRequested(true)
                                .build()
                            )
                            .setSepaDebitPayments(
                              AccountUpdateParams.Capabilities.SepaDebitPayments
                                .builder()
                                .setRequested(true)
                                .build()
                            )
                            .build()
                        )
                        .setSettings(
                          AccountUpdateParams.Settings
                            .builder()
                            .setPayouts(
                              AccountUpdateParams.Settings.Payouts
                                .builder()
                                .setSchedule(
                                  AccountUpdateParams.Settings.Payouts.Schedule
                                    .builder()
                                    .setInterval(
                                      AccountUpdateParams.Settings.Payouts.Schedule.Interval.MANUAL
                                    )
                                    .build()
                                )
                                .build()
                            )
                            .build()
                        )
                        .putMetadata("external_uuid", naturalUser.externalUuid)
                    naturalUser.business match {
                      case Some(business) =>
                        val businessProfile =
                          AccountUpdateParams.BusinessProfile
                            .builder()
                            .setMcc(business.merchantCategoryCode)
                            .setUrl(business.website)
                        business.support match {
                          case Some(support) =>
                            businessProfile.setSupportEmail(support.email)
                            support.phone match {
                              case Some(phone) =>
                                businessProfile.setSupportPhone(phone)
                              case _ =>
                            }
                            support.url match {
                              case Some(url) =>
                                businessProfile.setSupportUrl(url)
                              case _ =>
                            }
                          case _ =>
                        }
                        params.setBusinessProfile(businessProfile.build())
                      case _ =>
                    }
                    account.update(
                      params.build(),
                      StripeApi().requestOptions
                    )

                  case _ =>
                    // create account
                    val individual =
                      TokenCreateParams.Account.Individual
                        .builder()
                        .setFirstName(naturalUser.firstName)
                        .setLastName(naturalUser.lastName)
                        .setDob(
                          TokenCreateParams.Account.Individual.Dob
                            .builder()
                            .setDay(c.get(Calendar.DAY_OF_MONTH))
                            .setMonth(c.get(Calendar.MONTH) + 1)
                            .setYear(c.get(Calendar.YEAR))
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
                    val token = Token.create(
                      TokenCreateParams
                        .builder()
                        .setAccount(
                          TokenCreateParams.Account
                            .builder()
                            .setBusinessType(TokenCreateParams.Account.BusinessType.INDIVIDUAL)
                            .setIndividual(individual.build())
                            .setTosShownAndAccepted(tos_shown_and_accepted)
                            .build()
                        )
                        .build(),
                      StripeApi().requestOptions
                    )
                    val params =
                      AccountCreateParams
                        .builder()
                        .setAccountToken(token.getId)
                        .setCapabilities(
                          AccountCreateParams.Capabilities
                            .builder()
                            .setBankTransferPayments(
                              AccountCreateParams.Capabilities.BankTransferPayments
                                .builder()
                                .setRequested(true)
                                .build()
                            )
                            .setCardPayments(
                              AccountCreateParams.Capabilities.CardPayments
                                .builder()
                                .setRequested(true)
                                .build()
                            )
                            .setCartesBancairesPayments(
                              AccountCreateParams.Capabilities.CartesBancairesPayments
                                .builder()
                                .setRequested(true)
                                .build()
                            )
                            .setTransfers(
                              AccountCreateParams.Capabilities.Transfers
                                .builder()
                                .setRequested(true)
                                .build()
                            )
                            .setSepaBankTransferPayments(
                              AccountCreateParams.Capabilities.SepaBankTransferPayments
                                .builder()
                                .setRequested(true)
                                .build()
                            )
                            .setSepaDebitPayments(
                              AccountCreateParams.Capabilities.SepaDebitPayments
                                .builder()
                                .setRequested(true)
                                .build()
                            )
                            .build()
                        )
                        .setController(
                          AccountCreateParams.Controller
                            .builder()
                            .setFees(
                              AccountCreateParams.Controller.Fees
                                .builder()
                                .setPayer(AccountCreateParams.Controller.Fees.Payer.APPLICATION)
                                .build()
                            )
                            .setLosses(
                              AccountCreateParams.Controller.Losses
                                .builder()
                                .setPayments(
                                  AccountCreateParams.Controller.Losses.Payments.APPLICATION
                                )
                                .build()
                            )
                            .setRequirementCollection(
                              AccountCreateParams.Controller.RequirementCollection.APPLICATION
                            )
                            .setStripeDashboard(
                              AccountCreateParams.Controller.StripeDashboard
                                .builder()
                                .setType(AccountCreateParams.Controller.StripeDashboard.Type.NONE)
                                .build()
                            )
                            .build()
                        )
                        .setCountry(naturalUser.countryOfResidence)
                        .setSettings(
                          AccountCreateParams.Settings
                            .builder()
                            .setPayouts(
                              AccountCreateParams.Settings.Payouts
                                .builder()
                                .setSchedule(
                                  AccountCreateParams.Settings.Payouts.Schedule
                                    .builder()
                                    .setInterval(
                                      AccountCreateParams.Settings.Payouts.Schedule.Interval.MANUAL
                                    )
                                    .build()
                                )
                                .build()
                            )
                            .build()
                        )
                        .setType(AccountCreateParams.Type.CUSTOM)
                        .setMetadata(Map("external_uuid" -> naturalUser.externalUuid).asJava)

                    naturalUser.business match {
                      case Some(business) =>
                        val businessProfile =
                          AccountCreateParams.BusinessProfile
                            .builder()
                            .setMcc(business.merchantCategoryCode)
                            .setUrl(business.website)
                        business.support match {
                          case Some(support) =>
                            businessProfile.setSupportEmail(support.email)
                            support.phone match {
                              case Some(phone) =>
                                businessProfile.setSupportPhone(phone)
                              case _ =>
                            }
                            support.url match {
                              case Some(url) =>
                                businessProfile.setSupportUrl(url)
                              case _ =>
                            }
                          case _ =>
                        }
                        params.setBusinessProfile(businessProfile.build())
                      case _ =>
                    }

                    Account.create(
                      params.build(),
                      StripeApi().requestOptions
                    )
                }
              ) match {
                case Success(account) =>
                  if (tos_shown_and_accepted) {
                    mlog.info(s"****** tos_shown_and_accepted -> $tos_shown_and_accepted")
                    val params =
                      AccountUpdateParams
                        .builder()
                        .setTosAcceptance(
                          AccountUpdateParams.TosAcceptance
                            .builder()
                            .setIp(ipAddress.get)
                            .setUserAgent(userAgent.get)
                            .setDate(persistence.now().getEpochSecond)
                            .build()
                        )
                        .build()
                    Try(
                      account.update(
                        params,
                        StripeApi().requestOptions
                      )
                    )
                  }
                  Some(account.getId)
                case Failure(f) =>
                  mlog.error(f.getMessage, f)
                  None
              }
            }
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case _ => None
    }
  }

  /** @param maybeLegalUser
    *   - legal user to create or update
    * @return
    *   legal user created or updated
    */
  @InternalApi
  private[spi] override def createOrUpdateLegalUser(
    maybeLegalUser: Option[LegalUser],
    acceptedTermsOfPSP: Boolean,
    ipAddress: Option[String],
    userAgent: Option[String]
  ): Option[String] = {
    maybeLegalUser match {
      case Some(legalUser) =>
        // create or update legal user
        val birthday = legalUser.legalRepresentative.birthday
        val sdf = new SimpleDateFormat("dd/MM/yyyy")
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"))
        Try(sdf.parse(birthday)) match {
          case Success(date) =>
            val tos_shown_and_accepted =
              ipAddress.isDefined && userAgent.isDefined && acceptedTermsOfPSP
            val soleTrader = legalUser.legalUserType.isSoletrader
            val c = Calendar.getInstance()
            c.setTime(date)
            Try(
              (legalUser.legalRepresentative.userId match {
                case Some(userId) if userId.startsWith("acct_") =>
                  Option(Account.retrieve(userId, StripeApi().requestOptions))
                case _ =>
                  Account
                    .list(
                      AccountListParams
                        .builder()
                        .setLimit(100)
                        .build(),
                      StripeApi().requestOptions
                    )
                    .getData
                    .asScala
                    .find(acc =>
                      acc.getMetadata
                        .get("external_uuid") == legalUser.legalRepresentative.externalUuid
                    )
              }) match {
                case Some(account) =>
                  // update account legal representative

                  val requestOptions = StripeApi().requestOptionsBuilder
                    .setStripeAccount(account.getId)
                    .build()

                  mlog.info(s"options -> ${new Gson().toJson(requestOptions)}")

                  // FIXME we shouldn't have to do this but the stripe api seems to not take into account the request options
                  Stripe.apiKey = provider.providerApiKey

                  account
                    .persons()
                    .list(
                      PersonCollectionListParams
                        .builder()
                        .setRelationship(
                          PersonCollectionListParams.Relationship
                            .builder()
                            .setRepresentative(true)
                            .build()
                        )
                        .build(),
                      requestOptions
                    )
                    .getData
                    .asScala
                    .headOption
                    .map(person => {
                      val params =
                        PersonUpdateParams
                          .builder()
                          .setAddress(
                            PersonUpdateParams.Address
                              .builder()
                              .setCity(legalUser.legalRepresentativeAddress.city)
                              .setCountry(legalUser.legalRepresentativeAddress.country)
                              .setLine1(legalUser.legalRepresentativeAddress.addressLine)
                              .setPostalCode(legalUser.legalRepresentativeAddress.postalCode)
                              .build()
                          )
                          .setFirstName(legalUser.legalRepresentative.firstName)
                          .setLastName(legalUser.legalRepresentative.lastName)
                          .setDob(
                            PersonUpdateParams.Dob
                              .builder()
                              .setDay(c.get(Calendar.DAY_OF_MONTH))
                              .setMonth(c.get(Calendar.MONTH) + 1)
                              .setYear(c.get(Calendar.YEAR))
                              .build()
                          )
                          .setEmail(legalUser.legalRepresentative.email)
                          .setNationality(legalUser.legalRepresentative.nationality)

                      val relationship =
                        PersonUpdateParams.Relationship
                          .builder()
                          .setRepresentative(true)
                          .setDirector(true)
                          .setExecutive(true)

                      if (soleTrader) {
                        relationship
                          .setTitle(legalUser.legalRepresentative.title.getOrElse("Owner"))
                          .setOwner(true)
                          .setPercentOwnership(new java.math.BigDecimal(100.0))
                      } else {
                        relationship
                          .setOwner(false)
                          .setTitle(
                            legalUser.legalRepresentative.title.getOrElse("Representative")
                          )
                      }

                      mlog.info(s"relationship -> ${new Gson().toJson(relationship.build())}")

                      params.setRelationship(relationship.build())

                      legalUser.legalRepresentative.phone match {
                        case Some(phone) =>
                          params.setPhone(phone)
                        case _ =>
                      }

                      mlog.info(s"person -> ${new Gson().toJson(params.build())}")

                      person.update(
                        params.build(),
                        requestOptions
                      )
                    })
                    .getOrElse {
                      val params =
                        PersonCollectionCreateParams
                          .builder()
                          .setAddress(
                            PersonCollectionCreateParams.Address
                              .builder()
                              .setCity(legalUser.legalRepresentativeAddress.city)
                              .setCountry(legalUser.legalRepresentativeAddress.country)
                              .setLine1(legalUser.legalRepresentativeAddress.addressLine)
                              .setPostalCode(legalUser.legalRepresentativeAddress.postalCode)
                              .build()
                          )
                          .setFirstName(legalUser.legalRepresentative.firstName)
                          .setLastName(legalUser.legalRepresentative.lastName)
                          .setDob(
                            PersonCollectionCreateParams.Dob
                              .builder()
                              .setDay(c.get(Calendar.DAY_OF_MONTH))
                              .setMonth(c.get(Calendar.MONTH) + 1)
                              .setYear(c.get(Calendar.YEAR))
                              .build()
                          )
                          .setEmail(legalUser.legalRepresentative.email)
                          .setNationality(legalUser.legalRepresentative.nationality)
                      val relationship =
                        PersonCollectionCreateParams.Relationship
                          .builder()
                          .setRepresentative(true)
                          .setDirector(true)
                          .setExecutive(true)
                      if (soleTrader) {
                        relationship
                          .setTitle(legalUser.legalRepresentative.title.getOrElse("Owner"))
                          .setOwner(true)
                          .setPercentOwnership(new java.math.BigDecimal(100.0))
                      } else {
                        relationship
                          .setOwner(false)
                          .setTitle(
                            legalUser.legalRepresentative.title.getOrElse("Representative")
                          )
                      }

                      mlog.info(s"relationship -> ${new Gson().toJson(relationship.build())}")

                      params.setRelationship(relationship.build())

                      legalUser.legalRepresentative.phone match {
                        case Some(phone) =>
                          params.setPhone(phone)
                        case _ =>
                      }

                      mlog.info(s"person -> ${new Gson().toJson(params.build())}")

                      account
                        .persons()
                        .create(
                          params.build(),
                          requestOptions
                        )
                    }

                  // update account company
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

                  mlog.info(s"company -> ${new Gson().toJson(company.build())}")

                  val token =
                    Token.create(
                      TokenCreateParams
                        .builder()
                        .setAccount(
                          TokenCreateParams.Account
                            .builder()
                            .setBusinessType(TokenCreateParams.Account.BusinessType.COMPANY)
                            .setCompany(company.build())
                            .setTosShownAndAccepted(tos_shown_and_accepted)
                            .build
                        )
                        .build(),
                      requestOptions
                    )

                  val params =
                    AccountUpdateParams
                      .builder()
                      .setAccountToken(token.getId)
                      .setCapabilities(
                        AccountUpdateParams.Capabilities
                          .builder()
                          .setBankTransferPayments(
                            AccountUpdateParams.Capabilities.BankTransferPayments
                              .builder()
                              .setRequested(true)
                              .build()
                          )
                          .setCardPayments(
                            AccountUpdateParams.Capabilities.CardPayments
                              .builder()
                              .setRequested(true)
                              .build()
                          )
                          .setCartesBancairesPayments(
                            AccountUpdateParams.Capabilities.CartesBancairesPayments
                              .builder()
                              .setRequested(true)
                              .build()
                          )
                          .setTransfers(
                            AccountUpdateParams.Capabilities.Transfers
                              .builder()
                              .setRequested(true)
                              .build()
                          )
                          .setSepaBankTransferPayments(
                            AccountUpdateParams.Capabilities.SepaBankTransferPayments
                              .builder()
                              .setRequested(true)
                              .build()
                          )
                          .setSepaDebitPayments(
                            AccountUpdateParams.Capabilities.SepaDebitPayments
                              .builder()
                              .setRequested(true)
                              .build()
                          )
                          .build()
                      )
                      .setEmail(legalUser.legalRepresentative.email)
                      .setSettings(
                        AccountUpdateParams.Settings
                          .builder()
                          .setPayouts(
                            AccountUpdateParams.Settings.Payouts
                              .builder()
                              .setSchedule(
                                AccountUpdateParams.Settings.Payouts.Schedule
                                  .builder()
                                  .setInterval(
                                    AccountUpdateParams.Settings.Payouts.Schedule.Interval.MANUAL
                                  )
                                  .build()
                              )
                              .build()
                          )
                          .build()
                      )
                      .putMetadata("external_uuid", legalUser.legalRepresentative.externalUuid)

                  /*if (tos_shown_and_accepted) {
                  //FIXME Parameter 'tos_acceptance' cannot be used in conjunction with an account token.
                    params
                      .setTosAcceptance(
                        AccountUpdateParams.TosAcceptance
                          .builder()
                          .setIp(ipAddress.get)
                          .setUserAgent(userAgent.get)
                          .setDate(persistence.now().getEpochSecond)
                          .build()
                      )
                  }*/

                  legalUser.business.orElse(legalUser.legalRepresentative.business) match {
                    case Some(business) =>
                      val businessProfile =
                        AccountUpdateParams.BusinessProfile
                          .builder()
                          .setMcc(business.merchantCategoryCode)
                          .setUrl(business.website)
                      business.support match {
                        case Some(support) =>
                          businessProfile.setSupportEmail(support.email)
                          support.phone match {
                            case Some(phone) =>
                              businessProfile.setSupportPhone(phone)
                            case _ =>
                          }
                          support.url match {
                            case Some(url) =>
                              businessProfile.setSupportUrl(url)
                            case _ =>
                          }
                        case _ =>
                      }
                      params.setBusinessProfile(businessProfile.build())
                    case _ =>
                  }

                  account.update(
                    params.build(),
                    requestOptions
                  )

                  /*val person =
                    TokenCreateParams.Person
                      .builder()
                      .setFirstName(legalUser.legalRepresentative.firstName)
                      .setLastName(legalUser.legalRepresentative.lastName)
                      .setDob(
                        TokenCreateParams.Person.Dob
                          .builder()
                          .setDay(c.get(Calendar.DAY_OF_MONTH))
                          .setMonth(c.get(Calendar.MONTH) + 1)
                          .setYear(c.get(Calendar.YEAR))
                          .build()
                      )
                      .setEmail(legalUser.legalRepresentative.email)
                      .setNationality(legalUser.legalRepresentative.nationality)
                      .setRelationship(
                        TokenCreateParams.Person.Relationship
                          .builder()
                          .setRepresentative(true)
                          .build()
                      )
                  if(tos_shown_and_accepted)
                    person.setAdditionalTosAcceptances(
                      TokenCreateParams.Person.AdditionalTosAcceptances.builder()
                        .setAccount(TokenCreateParams.Person.AdditionalTosAcceptances.Account
                          .builder()
                          .setIp(ipAddress.get)
                          .setUserAgent(userAgent.get)
                          .setDate(persistence.now().getEpochSecond)
                          .build()
                        )
                        .build()
                    )
                  legalUser.legalRepresentative.phone match {
                    case Some(phone) =>
                      person.setPhone(phone)
                    case _ =>
                  }

                  mlog.info(s"person -> ${new Gson().toJson(person.build())}")

                  val token2 =
                    Token.create(
                        TokenCreateParams.builder.setPerson(person.build()).build(),
                        requestOptions
                    )

                  val params2 =
                    PersonCollectionCreateParams
                      .builder()
                      .setPersonToken(token2.getId)

                  account
                    .persons()
                    .create(
                      params2.build(),
                      requestOptions
                    )*/

                  account
                case _ =>
                  // create company account

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

                  mlog.info(s"company -> ${new Gson().toJson(company.build())}")

                  val accountParams =
                    TokenCreateParams.Account
                      .builder()
                      .setBusinessType(TokenCreateParams.Account.BusinessType.COMPANY)
                      .setCompany(company.build())
                      .setTosShownAndAccepted(tos_shown_and_accepted)

                  mlog.info(s"token -> ${new Gson().toJson(accountParams.build())}")

                  val token = Token.create(
                    TokenCreateParams.builder.setAccount(accountParams.build()).build(),
                    StripeApi().requestOptions
                  )

                  val params =
                    AccountCreateParams
                      .builder()
                      .setAccountToken(token.getId)
                      .setCapabilities(
                        AccountCreateParams.Capabilities
                          .builder()
                          .setBankTransferPayments(
                            AccountCreateParams.Capabilities.BankTransferPayments
                              .builder()
                              .setRequested(true)
                              .build()
                          )
                          .setCardPayments(
                            AccountCreateParams.Capabilities.CardPayments
                              .builder()
                              .setRequested(true)
                              .build()
                          )
                          .setCartesBancairesPayments(
                            AccountCreateParams.Capabilities.CartesBancairesPayments
                              .builder()
                              .setRequested(true)
                              .build()
                          )
                          .setTransfers(
                            AccountCreateParams.Capabilities.Transfers
                              .builder()
                              .setRequested(true)
                              .build()
                          )
                          .setSepaBankTransferPayments(
                            AccountCreateParams.Capabilities.SepaBankTransferPayments
                              .builder()
                              .setRequested(true)
                              .build()
                          )
                          .setSepaDebitPayments(
                            AccountCreateParams.Capabilities.SepaDebitPayments
                              .builder()
                              .setRequested(true)
                              .build()
                          )
                          .build()
                      )
                      .setController(
                        AccountCreateParams.Controller
                          .builder()
                          .setFees(
                            AccountCreateParams.Controller.Fees
                              .builder()
                              .setPayer(AccountCreateParams.Controller.Fees.Payer.APPLICATION)
                              .build()
                          )
                          .setLosses(
                            AccountCreateParams.Controller.Losses
                              .builder()
                              .setPayments(
                                AccountCreateParams.Controller.Losses.Payments.APPLICATION
                              )
                              .build()
                          )
                          .setRequirementCollection(
                            AccountCreateParams.Controller.RequirementCollection.APPLICATION
                          )
                          .setStripeDashboard(
                            AccountCreateParams.Controller.StripeDashboard
                              .builder()
                              .setType(AccountCreateParams.Controller.StripeDashboard.Type.NONE)
                              .build()
                          )
                          .build()
                      )
                      .setCountry(legalUser.legalRepresentative.countryOfResidence)
                      .setEmail(legalUser.legalRepresentative.email)
                      .setSettings(
                        AccountCreateParams.Settings
                          .builder()
                          .setPayouts(
                            AccountCreateParams.Settings.Payouts
                              .builder()
                              .setSchedule(
                                AccountCreateParams.Settings.Payouts.Schedule
                                  .builder()
                                  .setInterval(
                                    AccountCreateParams.Settings.Payouts.Schedule.Interval.MANUAL
                                  )
                                  .build()
                              )
                              .build()
                          )
                          .build()
                      )
                      .setType(AccountCreateParams.Type.CUSTOM)
                      .putMetadata("external_uuid", legalUser.legalRepresentative.externalUuid)

                  /*if (tos_shown_and_accepted) {
                  //FIXME Parameter 'tos_acceptance' cannot be used in conjunction with an account token.
                    params
                      .setTosAcceptance(
                        AccountCreateParams.TosAcceptance
                          .builder()
                          .setIp(ipAddress.get)
                          .setUserAgent(userAgent.get)
                          .setDate(persistence.now().getEpochSecond)
                          .build()
                      )
                  }*/

                  legalUser.business.orElse(legalUser.legalRepresentative.business) match {
                    case Some(business) =>
                      val businessProfile =
                        AccountCreateParams.BusinessProfile
                          .builder()
                          .setMcc(business.merchantCategoryCode)
                          .setUrl(business.website)
                      business.support match {
                        case Some(support) =>
                          businessProfile.setSupportEmail(support.email)
                          support.phone match {
                            case Some(phone) =>
                              businessProfile.setSupportPhone(phone)
                            case _ =>
                          }
                          support.url match {
                            case Some(url) =>
                              businessProfile.setSupportUrl(url)
                            case _ =>
                          }
                        case _ =>
                      }
                      params.setBusinessProfile(businessProfile.build())
                    case _ =>
                  }

                  mlog.info(s"account -> ${new Gson().toJson(params.build())}")

                  val account = Account.create(
                    params.build(),
                    StripeApi().requestOptions
                  )

                  val requestOptions = StripeApi().requestOptionsBuilder
                    .setStripeAccount(account.getId)
                    .build()

                  mlog.info(s"options -> ${new Gson().toJson(requestOptions)}")

                  // FIXME we shouldn't have to do this
                  //  but the stripe api does not seem to take into account the request options
                  Stripe.apiKey = provider.providerApiKey

                  // create legal representative
                  account
                    .persons()
                    .create(
                      {
                        val params =
                          PersonCollectionCreateParams
                            .builder()
                            .setAddress(
                              PersonCollectionCreateParams.Address
                                .builder()
                                .setCity(legalUser.legalRepresentativeAddress.city)
                                .setCountry(legalUser.legalRepresentativeAddress.country)
                                .setLine1(legalUser.legalRepresentativeAddress.addressLine)
                                .setPostalCode(legalUser.legalRepresentativeAddress.postalCode)
                                .build()
                            )
                            .setFirstName(legalUser.legalRepresentative.firstName)
                            .setLastName(legalUser.legalRepresentative.lastName)
                            .setDob(
                              PersonCollectionCreateParams.Dob
                                .builder()
                                .setDay(c.get(Calendar.DAY_OF_MONTH))
                                .setMonth(c.get(Calendar.MONTH) + 1)
                                .setYear(c.get(Calendar.YEAR))
                                .build()
                            )
                            .setEmail(legalUser.legalRepresentative.email)
                            .setNationality(legalUser.legalRepresentative.nationality)

                        val relationship =
                          PersonCollectionCreateParams.Relationship
                            .builder()
                            .setRepresentative(true)
                            .setDirector(true)
                            .setExecutive(true)

                        if (soleTrader) {
                          relationship
                            .setTitle(legalUser.legalRepresentative.title.getOrElse("Owner"))
                            .setOwner(true)
                            .setPercentOwnership(new java.math.BigDecimal(100.0))
                        } else {
                          relationship
                            .setOwner(false)
                            .setTitle(
                              legalUser.legalRepresentative.title.getOrElse("Representative")
                            )
                        }

                        mlog.info(s"relationship -> ${new Gson().toJson(relationship.build())}")

                        params.setRelationship(relationship.build())

                        legalUser.legalRepresentative.phone match {
                          case Some(phone) =>
                            params.setPhone(phone)
                          case _ =>
                        }

                        mlog.info(s"person -> ${new Gson().toJson(params.build())}")

                        params.build()
                      },
                      requestOptions
                    )
                  account
              }
            ) match {
              case Success(account) =>
                if (tos_shown_and_accepted) {
                  mlog.info(s"****** tos_shown_and_accepted -> $tos_shown_and_accepted")
                  val params =
                    AccountUpdateParams
                      .builder()
                      .setTosAcceptance(
                        AccountUpdateParams.TosAcceptance
                          .builder()
                          .setIp(ipAddress.get)
                          .setUserAgent(userAgent.get)
                          .setDate(persistence.now().getEpochSecond)
                          .build()
                      )
                      .build()
                  Try(
                    account.update(
                      params,
                      StripeApi().requestOptions
                    )
                  )
                }
                Some(account.getId)
              case Failure(f) =>
                mlog.error(f.getMessage, f)
                None
            }
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case _ => None
    }
  }

  /** @param userId
    *   - Provider user id
    * @return
    *   Ultimate Beneficial Owner Declaration
    */
  override def createDeclaration(userId: String): Option[UboDeclaration] = {
    Some(
      model.UboDeclaration.defaultInstance
        .withId(userId)
        .withCreatedDate(persistence.now())
        .withStatus(UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_CREATED)
    )
  }

  /** @param userId
    *   - Provider user id
    * @param uboDeclarationId
    *   - Provider declaration id
    * @param ultimateBeneficialOwner
    *   - Ultimate Beneficial Owner
    * @return
    *   Ultimate Beneficial Owner created or updated
    */
  override def createOrUpdateUBO(
    userId: String,
    uboDeclarationId: String,
    ultimateBeneficialOwner: UboDeclaration.UltimateBeneficialOwner
  ): Option[UboDeclaration.UltimateBeneficialOwner] = {
    Try(Account.retrieve(userId, StripeApi().requestOptions)) match {
      case Success(account) =>
        createOrUpdateOwner(account, ultimateBeneficialOwner) match {
          case Some(ownerId) =>
            Some(ultimateBeneficialOwner.withId(ownerId))
          case _ =>
            None
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  /** @param userId
    *   - Provider user id
    * @param uboDeclarationId
    *   - Provider declaration id
    * @return
    *   declaration with Ultimate Beneficial Owner(s)
    */
  override def getDeclaration(userId: String, uboDeclarationId: String): Option[UboDeclaration] = {
    Try(Account.retrieve(userId, StripeApi().requestOptions)) match {
      case Success(account) =>
        account
          .persons()
          .list(
            PersonCollectionListParams
              .builder()
              .setRelationship(
                PersonCollectionListParams.Relationship
                  .builder()
                  .setOwner(true)
                  .build()
              )
              .build(),
            StripeApi().requestOptions
          )
          .getData
          .asScala match {
          case persons =>
            Some(
              model.UboDeclaration.defaultInstance
                .withId(uboDeclarationId)
                .withCreatedDate(
                  Option(account.getCompany.getOwnershipDeclaration)
                    .map(o => time.epochSecondToDate(o.getDate))
                    .getOrElse(persistence.now())
                )
                .withStatus(
                  if (Option(account.getCompany.getOwnershipDeclaration).isDefined)
                    UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_VALIDATED
                  else UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_CREATED
                )
                .withUbos(persons.map(personToUbo))
            )
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  /** @param userId
    *   - Provider user id
    * @param uboDeclarationId
    *   - Provider declaration id
    * @return
    *   Ultimate Beneficial Owner declaration
    */
  override def validateDeclaration(
    userId: String,
    uboDeclarationId: String,
    ipAddress: String,
    userAgent: String
  ): Option[UboDeclaration] = {
    Try {
      val account = Account.retrieve(userId, StripeApi().requestOptions)
      val persons =
        account
          .persons()
          .list(
            PersonCollectionListParams
              .builder()
              .setRelationship(
                PersonCollectionListParams.Relationship
                  .builder()
                  .setOwner(true)
                  .build()
              )
              .build(),
            StripeApi().requestOptions
          )
          .getData
          .asScala

      val ownersProvided =
        persons.map(_.getRelationship.getPercentOwnership.doubleValue()).sum == 100.0

      val company =
        TokenCreateParams.Account.Company
          .builder()
          .setOwnershipDeclaration(
            TokenCreateParams.Account.Company.OwnershipDeclaration
              .builder()
              .setDate(persistence.now().getEpochSecond)
              .setIp(ipAddress)
              .setUserAgent(userAgent)
              .build()
          )
          .setOwnershipDeclarationShownAndSigned(ownersProvided)
          .setOwnersProvided(ownersProvided)

      mlog.info(s"company -> ${new Gson().toJson(company.build())}")

      val token = Token.create(
        TokenCreateParams.builder
          .setAccount(
            TokenCreateParams.Account
              .builder()
              .setBusinessType(TokenCreateParams.Account.BusinessType.COMPANY)
              .setCompany(company.build())
              .setTosShownAndAccepted(true)
              .build()
          )
          .build(),
        StripeApi().requestOptions
      )

      account.update(
        AccountUpdateParams
          .builder()
          .setAccountToken(token.getId)
          .build(),
        StripeApi().requestOptions
      )

      Some(
        model.UboDeclaration.defaultInstance
          .withId(uboDeclarationId)
          .withCreatedDate(persistence.now())
          .withStatus(
            if (ownersProvided) UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_VALIDATED
            else UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_INCOMPLETE
          )
          .withUbos(persons.map(personToUbo))
      )
    } match {
      case Success(declaration) =>
        declaration
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  /** @param maybeUserId
    *   - owner of the wallet
    * @param currency
    *   - currency
    * @param externalUuid
    *   - external unique id
    * @param maybeWalletId
    *   - wallet id to update
    * @return
    *   wallet id
    */
  override def createOrUpdateWallet(
    maybeUserId: Option[String],
    currency: String,
    externalUuid: String,
    maybeWalletId: Option[String]
  ): Option[String] = {
    maybeUserId match {
      case Some(userId) if userId.startsWith("acct") => // account
        Try {
          val account = Account.retrieve(userId, StripeApi().requestOptions)
          // the wallet id is the connected account id if not provided
          val walletId = maybeWalletId.getOrElse(userId)
          account
            .update(
              AccountUpdateParams
                .builder()
                .setDefaultCurrency(currency)
                .putMetadata("external_uuid", externalUuid)
                .putMetadata("wallet_id", walletId)
                .build(),
              StripeApi().requestOptions
            )
          walletId
        } match {
          case Success(walletId) => Some(walletId)
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case Some(userId) if userId.startsWith("cus") => // customer
        Try {
          val customer = Customer.retrieve(userId, StripeApi().requestOptions)
          val walletId = maybeWalletId.getOrElse(java.util.UUID.randomUUID().toString)
          customer
            .update(
              CustomerUpdateParams
                .builder()
                .putMetadata("external_uuid", externalUuid)
                .putMetadata("wallet_id", walletId)
                .build(),
              StripeApi().requestOptions
            )
          walletId
        } match {
          case Success(walletId) => Some(walletId)
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case _ =>
        None
    }
  }

  /** @param userId
    *   - Provider user id
    * @param externalUuid
    *   - external unique id
    * @param pages
    *   - document pages
    * @param documentType
    *   - document type
    * @return
    *   Provider document id
    */
  override def addDocument(
    userId: String,
    externalUuid: String,
    pages: Seq[Array[Byte]],
    documentType: KycDocument.KycDocumentType
  ): Option[String] = {
    Try(Account.retrieve(userId, StripeApi().requestOptions)) match {
      case Success(account) =>
        pagesToFiles(pages, documentType) match {
          case Success(files) =>
            Try {
              val documentId = files.map(_.getId).mkString("#")
              account.getBusinessType match {
                case "individual" =>
                  val verification =
                    documentType match {
                      case KycDocument.KycDocumentType.KYC_IDENTITY_PROOF =>
                        val documentBuilder = AccountUpdateParams.Individual.Verification.Document
                          .builder()
                          .setFront(files.head.getId)
                        if (files.size > 1) { //TODO add additional document if more than 2 files ?
                          documentBuilder.setBack(files(1).getId)
                        }
                        val document = documentBuilder.build()
                        AccountUpdateParams.Individual.Verification
                          .builder()
                          .setDocument(document)
                          .build()
                      case KycDocument.KycDocumentType.KYC_ADDRESS_PROOF =>
                        val documentBuilder =
                          AccountUpdateParams.Individual.Verification.AdditionalDocument
                            .builder()
                            .setFront(files.head.getId)
                        if (files.size > 1) { //TODO add additional document if more than 2 files ?
                          documentBuilder.setBack(files(1).getId)
                        }
                        val document = documentBuilder.build()
                        AccountUpdateParams.Individual.Verification
                          .builder()
                          .setAdditionalDocument(document)
                          .build()
                      case other =>
                        throw new Exception(s"Invalid document type $other")
                    }
                  account
                    .update(
                      AccountUpdateParams
                        .builder()
                        .setIndividual(
                          AccountUpdateParams.Individual
                            .builder()
                            .setVerification(verification)
                            .build()
                        )
                        .build(),
                      StripeApi().requestOptions
                    )
                  Some(documentId)
                case "company" =>
                  def verify(verification: PersonUpdateParams.Verification) =
                    account
                      .persons()
                      .list(
                        PersonCollectionListParams
                          .builder()
                          .setRelationship(
                            PersonCollectionListParams.Relationship
                              .builder()
                              .setRepresentative(true)
                              .build()
                          )
                          .build(),
                        StripeApi().requestOptions
                      )
                      .getData
                      .asScala
                      .headOption match {
                      case Some(person) =>
                        person.update(
                          PersonUpdateParams
                            .builder()
                            .setVerification(verification)
                            .build(),
                          StripeApi().requestOptions
                        )
                      case _ =>
                        throw new Exception("Representative not found")
                    }
                  documentType match {
                    case KycDocument.KycDocumentType.KYC_IDENTITY_PROOF =>
                      val documentBuilder = PersonUpdateParams.Verification.Document
                        .builder()
                        .setFront(files.head.getId)
                      if (files.size > 1) { //TODO add additional document if more than 2 files ?
                        documentBuilder.setBack(files(1).getId)
                      }
                      val document = documentBuilder.build()
                      verify(
                        PersonUpdateParams.Verification
                          .builder()
                          .setDocument(document)
                          .build()
                      )
                      Some(documentId)
                    case KycDocument.KycDocumentType.KYC_ADDRESS_PROOF =>
                      val documentBuilder = PersonUpdateParams.Verification.AdditionalDocument
                        .builder()
                        .setFront(files.head.getId)
                      if (files.size > 1) { //TODO add additional document if more than 2 files ?
                        documentBuilder.setBack(files(1).getId)
                      }
                      val document = documentBuilder.build()
                      verify(
                        PersonUpdateParams.Verification
                          .builder()
                          .setAdditionalDocument(document)
                          .build()
                      )
                      Some(documentId)
                    case KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF => // TODO verify this document
                      val documentBuilder = AccountUpdateParams.Documents
                        .builder()
                        .setProofOfRegistration(
                          AccountUpdateParams.Documents.ProofOfRegistration
                            .builder()
                            .addAllFile(files.map(_.getId).asJava)
                            .build()
                        )
                      val document = documentBuilder.build()
                      account.update(
                        AccountUpdateParams.builder().setDocuments(document).build(),
                        StripeApi().requestOptions
                      )
                      Some(documentId)
                    case _ =>
                      val documentBuilder = AccountUpdateParams.Company.Verification.Document
                        .builder()
                        .setFront(files.head.getId)
                      if (files.size > 1) { //TODO add additional document if more than 2 files ?
                        documentBuilder.setBack(files(1).getId)
                      }
                      val document = documentBuilder.build()
                      account
                        .update(
                          AccountUpdateParams
                            .builder()
                            .setCompany(
                              AccountUpdateParams.Company
                                .builder()
                                .setVerification(
                                  AccountUpdateParams.Company.Verification
                                    .builder()
                                    .setDocument(document)
                                    .build()
                                )
                                .build()
                            )
                            .build(),
                          StripeApi().requestOptions
                        )
                      Some(documentId)
                  }
                case _ => None
              }
            } match {
              case Success(s) => s
              case Failure(f) =>
                mlog.error(f.getMessage, f)
                None
            }
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  /** @param userId
    *   - Provider user id
    * @param documentId
    *   - Provider document id
    * @param documentType
    *   - document type
    * @return
    *   document validation report
    */
  override def loadDocumentStatus(
    userId: String,
    documentId: String,
    documentType: KycDocument.KycDocumentType
  ): KycDocumentValidationReport = {
    /*documentType match {
      case KycDocument.KycDocumentType.KYC_IDENTITY_PROOF =>
        Try(
          VerificationReport.list(
            VerificationReportListParams
              .builder()
              .setType(VerificationReportListParams.Type.DOCUMENT)
              .build(),
            StripeApi().requestOptions
          )
        ) match {
          case Success(reports) =>
            KycDocumentValidationReport.defaultInstance
              .withId(documentId)
              .withType(documentType)
              .withStatus(
                reports.getData.asScala.headOption match {
                  case Some(report) =>
                    Option(report.getDocument.getStatus).getOrElse("pending") match {
                      case "pending"  => KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATION_ASKED
                      case "verified" => KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATED
                      case "unverified" =>
                        KycDocument.KycDocumentStatus.KYC_DOCUMENT_REFUSED // not necessarily refused, but it does mean that Stripe might request more information soon.
                      case _ => KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED
                    }
                  case _ =>
                    KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED
                }
              )
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            KycDocumentValidationReport.defaultInstance
              .withId(documentId)
              .withType(documentType)
              .withStatus(KycDocument.KycDocumentStatus.Unrecognized(-1))
        }
      case _ =>
        KycDocumentValidationReport.defaultInstance
          .withId(documentId)
          .withType(documentType)
          .withStatus(KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED)
    }*/
    Try(Account.retrieve(userId, StripeApi().requestOptions)) match {
      case Success(account) =>
        /*account.getRequirements.getErrors.asScala.find(_.getRequirement == documentId /*FIXME*/) match {
          case Some(error) =>
            KycDocumentValidationReport.defaultInstance
              .withId(documentId)
              .withType(documentType)
              .withStatus(KycDocument.KycDocumentStatus.KYC_DOCUMENT_REFUSED)
              .withRefusedReasonType(error.getCode)
              .withRefusedReasonMessage(error.getReason)
          case _ =>
            KycDocumentValidationReport.defaultInstance
              .withId(documentId)
              .withType(documentType)
              .withStatus(KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED)
        }*/
        account.getBusinessType match {
          case "individual" =>
            verifyDocumentValidationReport(
              documentId,
              documentType,
              account.getIndividual.getVerification
            )
          case "company" =>
            documentType match {
              case KycDocument.KycDocumentType.KYC_IDENTITY_PROOF |
                  KycDocument.KycDocumentType.KYC_ADDRESS_PROOF =>
                account
                  .persons()
                  .list(
                    PersonCollectionListParams
                      .builder()
                      .setRelationship(
                        PersonCollectionListParams.Relationship
                          .builder()
                          .setRepresentative(true)
                          .build()
                      )
                      .build(),
                    StripeApi().requestOptions
                  )
                  .getData
                  .asScala
                  .headOption match {
                  case Some(person) =>
                    verifyDocumentValidationReport(
                      documentId,
                      documentType,
                      person.getVerification
                    )
                  case _ =>
                    KycDocumentValidationReport.defaultInstance
                      .withId(documentId)
                      .withType(documentType)
                      .withStatus(KycDocument.KycDocumentStatus.Unrecognized(-1))
                }
              case _ =>
                Option(account.getCompany.getVerification) match {
                  case Some(verification) =>
                    Option(verification.getDocument.getDetailsCode) match {
                      case Some(detailsCode) =>
                        KycDocumentValidationReport.defaultInstance
                          .withId(documentId)
                          .withType(documentType)
                          .withStatus(KycDocument.KycDocumentStatus.KYC_DOCUMENT_REFUSED)
                          .withRefusedReasonType(detailsCode)
                          .copy(
                            refusedReasonMessage = Option(verification.getDocument.getDetails)
                          )
                      case _ =>
                        KycDocumentValidationReport.defaultInstance
                          .withId(documentId)
                          .withType(documentType)
                          .withStatus(KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATED)
                    }
                  case _ =>
                    KycDocumentValidationReport.defaultInstance
                      .withId(documentId)
                      .withType(documentType)
                      .withStatus(KycDocument.KycDocumentStatus.Unrecognized(-1))
                }

            }

          case _ =>
            KycDocumentValidationReport.defaultInstance
              .withId(documentId)
              .withType(documentType)
              .withStatus(KycDocument.KycDocumentStatus.Unrecognized(-1))
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        KycDocumentValidationReport.defaultInstance
          .withId(documentId)
          .withType(documentType)
          .withStatus(KycDocument.KycDocumentStatus.Unrecognized(-1))
    }
  }

  /** @param maybeBankAccount
    *   - bank account to create
    * @return
    *   bank account id
    */
  override def createOrUpdateBankAccount(maybeBankAccount: Option[BankAccount]): Option[String] = {
    maybeBankAccount match {
      case Some(bankAccount) =>
        Try(Account.retrieve(bankAccount.userId, StripeApi().requestOptions)) match {
          case Success(account) =>
            Try {
              val requestOptions = StripeApi().requestOptions
              val bank_account =
                TokenCreateParams.BankAccount
                  .builder()
                  .setAccountNumber(bankAccount.iban)
                  .setRoutingNumber(bankAccount.bic)
                  .setCountry(bankAccount.countryCode.getOrElse(bankAccount.ownerAddress.country))
                  .setCurrency(bankAccount.currency.getOrElse("EUR"))
                  .setAccountHolderName(bankAccount.ownerName)
                  .setAccountHolderType(account.getBusinessType match {
                    case "individual" => TokenCreateParams.BankAccount.AccountHolderType.INDIVIDUAL
                    case _            => TokenCreateParams.BankAccount.AccountHolderType.COMPANY
                  })
                  .build()
              val token = Token.create(
                TokenCreateParams.builder().setBankAccount(bank_account).build(),
                requestOptions
              )
              val params =
                ExternalAccountCollectionCreateParams
                  .builder()
                  .setExternalAccount(token.getId)
                  .setDefaultForCurrency(true)
                  .putMetadata("external_uuid", bankAccount.externalUuid)
                  .putMetadata("active", "true")
                  .putMetadata("default_for_currency", bank_account.getCurrency)
                  .build()
              account.getExternalAccounts.create(
                params,
                requestOptions
              )
            } match {
              case Success(externalAccount) =>
                // FIXME we shouldn't have to do this
                //  but the stripe api does not seem to take into account the request options
                Stripe.apiKey = provider.providerApiKey
                Try(
                  account.getExternalAccounts
                    .list(
                      ExternalAccountCollectionListParams
                        .builder()
                        .setObject("bank_account")
                        .build()
                    )
                    .getData
                    .asScala
                    .filter(_.getId != externalAccount.getId)
                    .map(
                      _.delete(
                        StripeApi().requestOptions
                      )
                    )
                ) match {
                  case Failure(f) =>
                    mlog.error(f.getMessage, f)
                  case _ =>
                }
                /*bankAccount.id match {
                  case Some(id) =>
                    Try(
                      account.getExternalAccounts
                        .retrieve(id, StripeApi().requestOptions)
                        .delete(StripeApi().requestOptions)
                    ) match {
                      case Failure(f) =>
                        mlog.error(f.getMessage, f)
                      case _ =>
                    }
                  case _ =>
                }*/
                Some(externalAccount.getId)
              case Failure(f) =>
                mlog.error(f.getMessage, f)
                None
            }
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case _ => None
    }
  }

  /** @param userId
    *   - provider user id
    * @return
    *   the first active bank account
    */
  override def getActiveBankAccount(userId: String, currency: String): Option[String] = {
    Try(Account.retrieve(userId, StripeApi().requestOptions)) match {
      case Success(account) =>
        account.getExternalAccounts
          .list(
            ExternalAccountCollectionListParams
              .builder()
              .setObject("bank_account")
              .build()
          )
          .getData
          .asScala
          .find(externalAccount =>
            externalAccount
              .asInstanceOf[com.stripe.model.BankAccount]
              .getMetadata
              .get("default_for_currency")
              .contains(currency)
            &&
            externalAccount
              .asInstanceOf[com.stripe.model.BankAccount]
              .getMetadata
              .get("active")
              .contains("true")
          ) match {
          case Some(externalAccount) =>
            Some(externalAccount.getId)
          case _ =>
            None
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  private[this] def verifyDocumentValidationReport(
    documentId: String,
    documentType: KycDocument.KycDocumentType,
    verification: Person.Verification
  ) = {
    val status =
      Option(verification.getStatus).getOrElse("pending") match {
        case "pending"  => KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATION_ASKED
        case "verified" => KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATED
        case "unverified" =>
          KycDocument.KycDocumentStatus.KYC_DOCUMENT_REFUSED // not necessarily refused, but it does mean that Stripe might request more information soon.
        case _ => KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED
      }
    status match {
      case KycDocument.KycDocumentStatus.KYC_DOCUMENT_REFUSED =>
        KycDocumentValidationReport.defaultInstance
          .withId(documentId)
          .withType(documentType)
          .withStatus(status)
          .copy(
            refusedReasonType = documentType match {
              case KycDocument.KycDocumentType.KYC_IDENTITY_PROOF =>
                Option(verification.getDocument)
                  .map(_.getDetailsCode)
                  .orElse(Option(verification.getDetailsCode))
              case KycDocument.KycDocumentType.KYC_ADDRESS_PROOF =>
                Option(verification.getAdditionalDocument)
                  .map(_.getDetailsCode)
                  .orElse(Option(verification.getDetailsCode))
              case _ => None
            },
            refusedReasonMessage = documentType match {
              case KycDocument.KycDocumentType.KYC_IDENTITY_PROOF =>
                Option(verification.getDocument)
                  .map(_.getDetails)
                  .orElse(Option(verification.getDetails))
              case KycDocument.KycDocumentType.KYC_ADDRESS_PROOF =>
                Option(verification.getAdditionalDocument)
                  .map(_.getDetails)
                  .orElse(Option(verification.getDetails))
              case _ => None
            }
          )
          .withRefusedReasonType(verification.getDetailsCode)
          .withRefusedReasonMessage(verification.getDetails)
      case _ =>
        KycDocumentValidationReport.defaultInstance
          .withId(documentId)
          .withType(documentType)
          .withStatus(status)
    }
  }

  private[this] def createOrUpdateOwner(
    account: Account,
    owner: UltimateBeneficialOwner
  ): Option[String] = {
    val birthday = owner.birthday
    val sdf = new SimpleDateFormat("dd/MM/yyyy")
    sdf.setTimeZone(TimeZone.getTimeZone("UTC"))
    Try(sdf.parse(birthday)) match {
      case Success(date) =>
        val c = Calendar.getInstance()
        c.setTime(date)
        Try(
          (owner.id match {
            case Some(id) =>
              Option(account.persons().retrieve(id, StripeApi().requestOptions))
            case _ =>
              account
                .persons()
                .list(
                  PersonCollectionListParams
                    .builder()
                    .setRelationship(
                      PersonCollectionListParams.Relationship
                        .builder()
//                        .setOwner(true)
                        .build()
                    )
                    .build(),
                  StripeApi().requestOptions
                )
                .getData
                .asScala
                .find { person =>
                  person.getFirstName.toLowerCase == owner.firstName.toLowerCase &&
                  person.getLastName.toLowerCase == owner.lastName.toLowerCase
                }
          }) match {
            case Some(person) =>
              // update owner
              val params =
                PersonUpdateParams
                  .builder()
                  .setAddress(
                    PersonUpdateParams.Address
                      .builder()
                      .setCity(owner.city)
                      .setCountry(owner.country)
                      .setLine1(owner.address)
                      .setPostalCode(owner.postalCode)
                      .build()
                  )
                  .setFirstName(owner.firstName)
                  .setLastName(owner.lastName)
                  .setDob(
                    PersonUpdateParams.Dob
                      .builder()
                      .setDay(c.get(Calendar.DAY_OF_MONTH))
                      .setMonth(c.get(Calendar.MONTH) + 1)
                      .setYear(c.get(Calendar.YEAR))
                      .build()
                  )
                  .setRelationship(
                    owner.percentOwnership match {
                      case Some(ownership) =>
                        PersonUpdateParams.Relationship
                          .builder()
                          .setPercentOwnership(new java.math.BigDecimal(ownership))
                          .setOwner(true)
                          .build()
                      case _ =>
                        PersonUpdateParams.Relationship
                          .builder()
                          .setOwner(true)
                          .build()
                    }
                  )

              owner.email match {
                case Some(email) =>
                  params.setEmail(email)
                case _ =>
              }

              owner.phone match {
                case Some(phone) =>
                  params.setPhone(phone)
                case _ =>
              }

              mlog.info(s"owner -> ${new Gson().toJson(params.build())}")

              person.update(params.build(), StripeApi().requestOptions)
            case _ =>
              // create owner
              val params =
                PersonCollectionCreateParams
                  .builder()
                  .setAddress(
                    PersonCollectionCreateParams.Address
                      .builder()
                      .setCity(owner.city)
                      .setCountry(owner.country)
                      .setLine1(owner.address)
                      .setPostalCode(owner.postalCode)
                      .build()
                  )
                  .setFirstName(owner.firstName)
                  .setLastName(owner.lastName)
                  .setDob(
                    PersonCollectionCreateParams.Dob
                      .builder()
                      .setDay(c.get(Calendar.DAY_OF_MONTH))
                      .setMonth(c.get(Calendar.MONTH) + 1)
                      .setYear(c.get(Calendar.YEAR))
                      .build()
                  )
                  .setRelationship(
                    owner.percentOwnership match {
                      case Some(ownership) =>
                        PersonCollectionCreateParams.Relationship
                          .builder()
                          .setPercentOwnership(new java.math.BigDecimal(ownership))
                          .setOwner(true)
                          .build()
                      case _ =>
                        PersonCollectionCreateParams.Relationship
                          .builder()
                          .setOwner(true)
                          .build()
                    }
                  )

              owner.email match {
                case Some(email) =>
                  params.setEmail(email)
                case _ =>
              }

              owner.phone match {
                case Some(phone) =>
                  params.setPhone(phone)
                case _ =>
              }

              mlog.info(s"owner -> ${new Gson().toJson(params.build())}")

              account
                .persons()
                .create(params.build(), StripeApi().requestOptions)
          }
        ) match {
          case Success(person) =>
            Some(person.getId)
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  private[this] def createOrUpdateCustomer(naturalUser: NaturalUser): Option[String] = {
    Try {
      (naturalUser.userId match {
        case Some(userId) if userId.startsWith("cus_") =>
          Option(Customer.retrieve(userId, StripeApi().requestOptions))
        case _ =>
          Customer
            .list(
              CustomerListParams.builder().setEmail(naturalUser.email).build(),
              StripeApi().requestOptions
            )
            .getData
            .asScala
            .headOption
      }) match {
        // we should not allow a customer to update his email address
        case Some(customer) if customer.getEmail == naturalUser.email =>
          val params = CustomerUpdateParams
            .builder()
            //.setEmail(naturalUser.email)
            .setName(s"${naturalUser.firstName} ${naturalUser.lastName}")
            .setMetadata(
              Map(
                "external_uuid" -> naturalUser.externalUuid
              ).asJava
            )

          naturalUser.phone match {
            case Some(phone) =>
              params.setPhone(phone)
            case _ =>
          }

          naturalUser.address match {
            case Some(address) =>
              params.setAddress(
                CustomerUpdateParams.Address
                  .builder()
                  .setCity(address.city)
                  .setCountry(address.country)
                  .setLine1(address.addressLine)
                  .setPostalCode(address.postalCode)
                  .build()
              )
            case _ =>
          }

          mlog.info(s"customer -> ${new Gson().toJson(params.build())}")

          customer.update(params.build(), StripeApi().requestOptions)

        case _ =>
          val params = CustomerCreateParams
            .builder()
            .setEmail(naturalUser.email)
            .setName(s"${naturalUser.firstName} ${naturalUser.lastName}")
            .setMetadata(
              Map(
                "external_uuid" -> naturalUser.externalUuid
              ).asJava
            )

          naturalUser.phone match {
            case Some(phone) =>
              params.setPhone(phone)
            case _ =>
          }

          naturalUser.address match {
            case Some(address) =>
              params.setAddress(
                CustomerCreateParams.Address
                  .builder()
                  .setCity(address.city)
                  .setCountry(address.country)
                  .setLine1(address.addressLine)
                  .setPostalCode(address.postalCode)
                  .build()
              )
            case _ =>
          }

          mlog.info(s"customer -> ${new Gson().toJson(params.build())}")

          Customer.create(params.build(), StripeApi().requestOptions)
      }
    } match {
      case Success(customer) => Some(customer.getId)
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }
}
