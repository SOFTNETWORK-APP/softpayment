package app.softnetwork.payment.handlers

import akka.actor.typed.ActorSystem
import app.softnetwork.kv.handlers.GenericKeyValueDao
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.persistence.typed.scaladsl.EntityPattern
import app.softnetwork.payment.model._
import app.softnetwork.persistence._
import app.softnetwork.persistence.typed.CommandTypeKey

import scala.concurrent.{ExecutionContextExecutor, Future, Promise}
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

trait GenericPaymentHandler extends EntityPattern[PaymentCommand, PaymentResult] {
  _: CommandTypeKey[PaymentCommand] =>
  lazy val keyValueDao: GenericKeyValueDao =
    PaymentKvDao //FIXME app.softnetwork.payment.persistence.data.paymentKvDao

  protected override def lookup[T](
    key: T
  )(implicit system: ActorSystem[_]): Future[Option[Recipient]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    val promise = Promise[Option[Recipient]]
    keyValueDao.lookupKeyValue(key) onComplete {
      case Success(value) =>
        value match {
          case None => promise.success(Some(generateUUID(Some(key))))
          case some => promise.success(some)
        }
      case Failure(_) => promise.success(Some(generateUUID(Some(key))))
    }
    promise.future
  }

  override def !?(
    command: PaymentCommand
  )(implicit tTag: ClassTag[PaymentCommand], system: ActorSystem[_]): Future[PaymentResult] = {
    command match {
      case cmd: PaymentCommandWithKey => ??(cmd.key, cmd)
      case _                          => super.!?(command)
    }
  }

  override def !!(
    command: PaymentCommand
  )(implicit tTag: ClassTag[PaymentCommand], system: ActorSystem[_]): Unit = {
    command match {
      case cmd: PaymentCommandWithKey => ?!(cmd.key, cmd)
      case _                          => super.!!(command)
    }
  }
}

trait GenericPaymentDao { _: GenericPaymentHandler =>

  protected[payment] def loadPaymentAccount(
    key: String
  )(implicit system: ActorSystem[_]): Future[Option[PaymentAccount]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(LoadPaymentAccount(key)) map {
      case result: PaymentAccountLoaded => Some(result.paymentAccount)
      case _                            => None
    }
  }

  def preRegisterCard(orderUuid: String, user: PaymentUser, currency: String = "EUR")(implicit
    system: ActorSystem[_]
  ): Future[Option[CardPreRegistration]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(PreRegisterCard(orderUuid, user, currency)) map {
      case result: CardPreRegistered => Some(result.cardPreRegistration)
      case _                         => None
    }
  }

  def payIn(
    orderUuid: String,
    debitedAccount: String,
    debitedAmount: Int = 100,
    currency: String = "EUR",
    creditedAccount: String,
    registrationId: Option[String] = None,
    registrationData: Option[String] = None,
    registerCard: Boolean = false,
    ipAddress: Option[String] = None,
    browserInfo: Option[BrowserInfo] = None,
    statementDescriptor: Option[String] = None,
    paymentType: Transaction.PaymentType = Transaction.PaymentType.CARD
  )(implicit
    system: ActorSystem[_]
  ): Future[Either[PayInFailed, Either[PaymentRedirection, PaidIn]]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(
      PayIn(
        orderUuid,
        debitedAccount,
        debitedAmount,
        currency,
        creditedAccount,
        registrationId,
        registrationData,
        registerCard,
        ipAddress,
        browserInfo,
        statementDescriptor,
        paymentType
      )
    ) map {
      case result: PaymentRedirection => Right(Left(result))
      case result: PaidIn             => Right(Right(result))
      case error: PayInFailed         => Left(error)
      case _                          => Left(PayInFailed("unknown"))
    }
  }

  def preAuthorizeCard(
    orderUuid: String,
    debitedAccount: String,
    debitedAmount: Int = 100,
    currency: String = "EUR",
    registrationId: Option[String] = None,
    registrationData: Option[String] = None,
    registerCard: Boolean = false,
    ipAddress: Option[String] = None,
    browserInfo: Option[BrowserInfo] = None
  )(implicit
    system: ActorSystem[_]
  ): Future[Either[CardPreAuthorizationFailed, Either[PaymentRedirection, CardPreAuthorized]]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(
      PreAuthorizeCard(
        orderUuid,
        debitedAccount,
        debitedAmount,
        currency,
        registrationId,
        registrationData,
        registerCard,
        ipAddress,
        browserInfo
      )
    ) map {
      case result: PaymentRedirection        => Right(Left(result))
      case result: CardPreAuthorized         => Right(Right(result))
      case error: CardPreAuthorizationFailed => Left(error)
      case _                                 => Left(CardPreAuthorizationFailed("unknown"))
    }
  }

  def payInWithCardPreAuthorized(preAuthorizationId: String, creditedAccount: String)(implicit
    system: ActorSystem[_]
  ): Future[Either[PayInFailed, PaidIn]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(PayInWithCardPreAuthorized(preAuthorizationId, creditedAccount)) map {
      case result: PaidIn     => Right(result)
      case error: PayInFailed => Left(error)
      case _                  => Left(PayInFailed("unknown"))
    }
  }

  def refund(
    orderUuid: String,
    payInTransactionId: String,
    refundAmount: Int,
    currency: String = "EUR",
    reasonMessage: String,
    initializedByClient: Boolean
  )(implicit system: ActorSystem[_]): Future[Either[RefundFailed, Refunded]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(
      Refund(
        orderUuid,
        payInTransactionId,
        refundAmount,
        currency,
        reasonMessage,
        initializedByClient
      )
    ) map {
      case result: Refunded    => Right(result)
      case error: RefundFailed => Left(error)
      case _                   => Left(RefundFailed("unknown"))
    }
  }

  def payOut(orderUuid: String, creditedAccount: String, creditedAmount: Int, feesAmount: Int)(
    implicit system: ActorSystem[_]
  ): Future[Either[PayOutFailed, PaidOut]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(PayOut(orderUuid, creditedAccount, creditedAmount, feesAmount)) map {
      case result: PaidOut     => Right(result)
      case error: PayOutFailed => Left(error)
      case _                   => Left(PayOutFailed("unknown"))
    }
  }

  def transfer(
    orderUuid: Option[String] = None,
    debitedAccount: String,
    creditedAccount: String,
    debitedAmount: Int,
    feesAmount: Int = 0,
    payOutRequired: Boolean = true
  )(implicit system: ActorSystem[_]): Future[Either[TransferFailed, Transfered]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(
      Transfer(
        orderUuid,
        debitedAccount,
        creditedAccount,
        debitedAmount,
        feesAmount,
        payOutRequired
      )
    ) map {
      case result: Transfered    => Right(result)
      case error: TransferFailed => Left(error)
      case _                     => Left(TransferFailed("unknown"))
    }
  }
}
