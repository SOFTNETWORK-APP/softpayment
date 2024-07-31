package app.softnetwork.payment.spi

import app.softnetwork.payment.model.{
  Card,
  CardPreRegistration,
  PreAuthorizationTransaction,
  Transaction
}

trait CardApi { _: PaymentContext =>

  /** @param maybeUserId
    *   - owner of the card
    * @param currency
    *   - currency
    * @param externalUuid
    *   - external unique id
    * @return
    *   card pre registration
    */
  def preRegisterCard(
    maybeUserId: Option[String],
    currency: String,
    externalUuid: String
  ): Option[CardPreRegistration]

  /** @param cardPreRegistrationId
    *   - card registration id
    * @param maybeRegistrationData
    *   - card registration data
    * @return
    *   card id
    */
  def createCard(
    cardPreRegistrationId: String,
    maybeRegistrationData: Option[String]
  ): Option[String]

  /** @param cardId
    *   - card id
    * @return
    *   card
    */
  def loadCard(cardId: String): Option[Card]

  /** @param cardId
    *   - the id of the card to disable
    * @return
    *   the card disabled or none
    */
  def disableCard(cardId: String): Option[Card]

  /** @param preAuthorizationTransaction
    *   - pre authorization transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pre authorization transaction result
    */
  def preAuthorizeCard(
    preAuthorizationTransaction: PreAuthorizationTransaction,
    idempotency: Option[Boolean] = None
  ): Option[Transaction]

  /** @param orderUuid
    *   - order unique id
    * @param cardPreAuthorizedTransactionId
    *   - card pre authorized transaction id
    * @return
    *   card pre authorized transaction
    */
  def loadCardPreAuthorized(
    orderUuid: String,
    cardPreAuthorizedTransactionId: String
  ): Option[Transaction]

  /** @param orderUuid
    *   - order unique id
    * @param cardPreAuthorizedTransactionId
    *   - card pre authorized transaction id
    * @return
    *   whether pre authorization transaction has been cancelled or not
    */
  def cancelPreAuthorization(orderUuid: String, cardPreAuthorizedTransactionId: String): Boolean

  /** @param orderUuid
    *   - order unique id
    * @param cardPreAuthorizedTransactionId
    *   - card pre authorized transaction id
    * @return
    *   whether pre authorization transaction has been validated or not
    */
  def validatePreAuthorization(orderUuid: String, cardPreAuthorizedTransactionId: String): Boolean

}
