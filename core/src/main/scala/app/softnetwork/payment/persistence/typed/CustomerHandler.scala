package app.softnetwork.payment.persistence.typed

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.model.{NaturalUser, PaymentAccount}
import app.softnetwork.payment.model.NaturalUser.NaturalUserType
import app.softnetwork.persistence.now
import app.softnetwork.time._
import org.slf4j.Logger

trait CustomerHandler { _: PaymentCommandHandler =>

  @InternalApi
  private[payment] def createOrUpdateCustomer(
    entityId: String,
    state: Option[PaymentAccount],
    user: NaturalUser,
    currency: String,
    cid: Option[String]
  )(implicit
    system: ActorSystem[_],
    log: Logger,
    softPayClientSettings: SoftPayClientSettings
  ): (Option[PaymentAccount], Boolean) = {
    var registerWallet = false
    loadPaymentAccount(
      entityId,
      state,
      PaymentAccount.User.NaturalUser(user),
      cid
    ) match {
      case Some(paymentAccount) =>
        val clientId = paymentAccount.clientId
          .orElse(cid)
          .orElse(
            Option(softPayClientSettings.clientId)
          )
        val paymentProvider = loadPaymentProvider(clientId)
        import paymentProvider._
        val lastUpdated = now()
        (paymentAccount.userId match {
          case None =>
            createOrUpdatePaymentAccount(
              Some(
                paymentAccount.withNaturalUser(
                  user.withNaturalUserType(NaturalUserType.PAYER)
                )
              ),
              acceptedTermsOfPSP = false,
              None,
              None
            )
          case some => some
        }) match {
          case Some(userId) =>
            keyValueDao.addKeyValue(userId, entityId)
            (paymentAccount.walletId match {
              case None =>
                registerWallet = true
                createOrUpdateWallet(Some(userId), currency, user.externalUuid, None)
              case some => some
            }) match {
              case Some(walletId) =>
                keyValueDao.addKeyValue(walletId, entityId)
                (
                  Some(
                    paymentAccount
                      .withPaymentAccountStatus(
                        PaymentAccount.PaymentAccountStatus.COMPTE_OK
                      )
                      .copy(user =
                        PaymentAccount.User.NaturalUser(
                          user
                            .withUserId(userId)
                            .withWalletId(walletId)
                            .withNaturalUserType(NaturalUserType.PAYER)
                        )
                      )
                      .withLastUpdated(lastUpdated)
                  ),
                  registerWallet
                )
              case _ =>
                (
                  Some(
                    paymentAccount
                      .withPaymentAccountStatus(
                        PaymentAccount.PaymentAccountStatus.COMPTE_OK
                      )
                      .copy(user =
                        PaymentAccount.User.NaturalUser(
                          user
                            .withUserId(userId)
                            .withNaturalUserType(NaturalUserType.PAYER)
                        )
                      )
                      .withLastUpdated(lastUpdated)
                  ),
                  registerWallet
                )
            }
        }
      case _ => (None, registerWallet)
    }
  }
}
