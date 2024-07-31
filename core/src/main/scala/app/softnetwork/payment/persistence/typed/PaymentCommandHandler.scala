package app.softnetwork.payment.persistence.typed

import akka.actor.typed.ActorSystem
import app.softnetwork.kv.handlers.GenericKeyValueDao
import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.handlers.{PaymentDao, PaymentKvDao, SoftPayAccountDao}
import app.softnetwork.payment.model.PaymentAccount
import app.softnetwork.payment.spi.{PaymentProvider, PaymentProviders}
import app.softnetwork.persistence.now
import app.softnetwork.concurrent.Completion
import app.softnetwork.time._
import org.slf4j.Logger

import scala.util.{Failure, Success}

trait PaymentCommandHandler { _: Completion =>

  lazy val keyValueDao: GenericKeyValueDao = PaymentKvDao

  def paymentDao: PaymentDao

  def softPayAccountDao: SoftPayAccountDao

  @InternalApi
  private[payment] def loadPaymentAccount(
    entityId: String,
    state: Option[PaymentAccount],
    user: PaymentAccount.User,
    clientId: Option[String]
  )(implicit
    system: ActorSystem[_],
    log: Logger,
    softPayClientSettings: SoftPayClientSettings
  ): Option[PaymentAccount] = {
    val pa = PaymentAccount.defaultInstance.withUser(user).copy(clientId = clientId)
    val uuid = pa.externalUuidWithProfile
    state match {
      case None =>
        keyValueDao.lookupKeyValue(uuid) complete () match {
          case Success(s) =>
            s match {
              case Some(t) if t != entityId =>
                log.warn(
                  s"another payment account entity $t has already been associated with this uuid $uuid"
                )
                None
              case _ =>
                keyValueDao.addKeyValue(uuid, entityId)
                Some(pa.withUuid(entityId).withCreatedDate(now()))
            }
          case Failure(f) =>
            log.error(f.getMessage, f)
            None
        }
      case Some(paymentAccount) =>
        if (paymentAccount.externalUuid != pa.externalUuid) {
          log.warn(
            s"the payment account entity $entityId has already been associated with another external uuid ${paymentAccount.externalUuid}"
          )
          None
        } else {
          keyValueDao.addKeyValue(uuid, entityId)
          Some(
            paymentAccount.copy(clientId =
              paymentAccount.clientId
                .orElse(clientId)
                .orElse(Option(softPayClientSettings.clientId))
            )
          )
        }
    }
  }

  @InternalApi
  private[payment] def loadPaymentProvider(
    clientId: Option[String]
  )(implicit system: ActorSystem[_]): PaymentProvider = {
    PaymentProviders.paymentProvider(
      clientId
        .flatMap(softPayAccountDao.loadProvider(_) complete () match {
          case Success(s) => s
          case Failure(_) => None
        })
        .getOrElse(throw new Exception("Payment provider not found"))
    )
  }

}
