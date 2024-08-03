package app.softnetwork.payment.handlers

import akka.actor.typed.ActorSystem
import akka.cluster.sharding.typed.scaladsl.EntityTypeKey
import app.softnetwork.kv.handlers.GenericKeyValueDao
import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.message.PaymentMessages.{MandateCanceled, _}
import app.softnetwork.persistence.typed.scaladsl.EntityPattern
import app.softnetwork.payment.model._
import app.softnetwork.payment.persistence.typed.PaymentBehavior
import app.softnetwork.persistence._
import app.softnetwork.persistence.typed.CommandTypeKey
import org.slf4j.{Logger, LoggerFactory}

import java.util.Date
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.language.implicitConversions
import scala.reflect.ClassTag

trait PaymentTypeKey extends CommandTypeKey[PaymentCommand] {
  override def TypeKey(implicit tTag: ClassTag[PaymentCommand]): EntityTypeKey[PaymentCommand] =
    PaymentBehavior.TypeKey
}

trait PaymentHandler extends EntityPattern[PaymentCommand, PaymentResult] with PaymentTypeKey {
  lazy val keyValueDao: GenericKeyValueDao =
    PaymentKvDao //FIXME app.softnetwork.payment.persistence.data.paymentKvDao

  protected override def lookup[T](
    key: T
  )(implicit system: ActorSystem[_]): Future[Option[Recipient]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    keyValueDao.lookupKeyValue(key) map {
      case None => Some(generateUUID(Some(key)))
      case some => some
    }
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

trait PaymentDao extends PaymentHandler {

  @InternalApi
  private[payment] def loadPaymentAccount(
    key: String,
    clientId: Option[String]
  )(implicit system: ActorSystem[_]): Future[Option[PaymentAccount]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(LoadPaymentAccount(key, clientId)) map {
      case result: PaymentAccountLoaded => Some(result.paymentAccount)
      case _                            => None
    }
  }

  @InternalApi
  private[payment] def loadBankAccount(
    externalUuid: String,
    clientId: Option[String] = None
  )(implicit system: ActorSystem[_]): Future[Option[BankAccount]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(LoadBankAccount(externalUuid, clientId)) map {
      case result: BankAccountLoaded => Some(result.bankAccount)
      case _                         => None
    }
  }

  @InternalApi
  private[payment] def preRegisterCard(
    orderUuid: String,
    user: NaturalUser,
    currency: String = "EUR",
    clientId: Option[String] = None
  )(implicit
    system: ActorSystem[_]
  ): Future[Either[String, CardPreRegistration]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(PreRegisterCard(orderUuid, user, currency, clientId)) map {
      case result: CardPreRegistered => Right(result.cardPreRegistration)
      case error: PaymentError       => Left(error.message)
      case _                         => Left("unknown")
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
    paymentType: Transaction.PaymentType = Transaction.PaymentType.CARD,
    printReceipt: Boolean = false
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
        paymentType,
        printReceipt
      )
    ) map {
      case result: PaymentRedirection => Right(Left(result))
      case result: PaidIn             => Right(Right(result))
      case error: PayInFailed         => Left(error)
      case _ =>
        Left(PayInFailed("", Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED, "unknown"))
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
    browserInfo: Option[BrowserInfo] = None,
    printReceipt: Boolean = false,
    creditedAccount: Option[String] = None,
    feesAmount: Option[Int] = None
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
        browserInfo,
        printReceipt,
        creditedAccount,
        feesAmount
      )
    ) map {
      case result: PaymentRedirection        => Right(Left(result))
      case result: CardPreAuthorized         => Right(Right(result))
      case error: CardPreAuthorizationFailed => Left(error)
      case _                                 => Left(CardPreAuthorizationFailed("unknown"))
    }
  }

  @InternalApi
  private[payment] def cancelPreAuthorization(
    orderUuid: String,
    cardPreAuthorizedTransactionId: String,
    clientId: Option[String] = None
  )(implicit system: ActorSystem[_]): Future[Either[String, PreAuthorizationCanceled]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(CancelPreAuthorization(orderUuid, cardPreAuthorizedTransactionId, clientId)) map {
      case result: PreAuthorizationCanceled => Right(result)
      case error: PaymentError              => Left(error.message)
      case _                                => Left("unknown")
    }
  }

  @InternalApi
  private[payment] def payInWithCardPreAuthorized(
    preAuthorizationId: String,
    creditedAccount: String,
    debitedAmount: Option[Int],
    feesAmount: Option[Int] = None,
    clientId: Option[String] = None
  )(implicit
    system: ActorSystem[_]
  ): Future[Either[PayInFailed, PaidIn]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(
      PayInWithCardPreAuthorized(
        preAuthorizationId,
        creditedAccount,
        debitedAmount,
        feesAmount,
        clientId
      )
    ) map {
      case result: PaidIn     => Right(result)
      case error: PayInFailed => Left(error)
      case error: PaymentError =>
        Left(
          PayInFailed("", Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED, error.message)
        )
      case _ =>
        Left(PayInFailed("", Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED, "unknown"))
    }
  }

  @InternalApi
  private[payment] def loadPayInTransaction(
    orderUuid: String,
    transactionId: String,
    clientId: Option[String] = None
  )(implicit
    system: ActorSystem[_]
  ): Future[Either[TransactionNotFound.type, PayInTransactionLoaded]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(LoadPayInTransaction(orderUuid, transactionId, clientId)) map {
      case result: PayInTransactionLoaded => Right(result)
      case _                              => Left(TransactionNotFound)
    }
  }

  @InternalApi
  private[payment] def refund(
    orderUuid: String,
    payInTransactionId: String,
    refundAmount: Int,
    feesRefundAmount: Option[Int] = None,
    currency: String = "EUR",
    reasonMessage: String,
    initializedByClient: Boolean,
    clientId: Option[String] = None
  )(implicit system: ActorSystem[_]): Future[Either[RefundFailed, Refunded]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(
      Refund(
        orderUuid,
        payInTransactionId,
        refundAmount,
        feesRefundAmount,
        currency,
        reasonMessage,
        initializedByClient,
        clientId
      )
    ) map {
      case result: Refunded    => Right(result)
      case error: RefundFailed => Left(error)
      case error: PaymentError =>
        Left(
          RefundFailed("", Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED, error.message)
        )
      case _ =>
        Left(RefundFailed("", Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED, "unknown"))
    }
  }

  @InternalApi
  private[payment] def payOut(
    orderUuid: String,
    creditedAccount: String,
    creditedAmount: Int,
    feesAmount: Int,
    currency: String = "EUR",
    externalReference: Option[String],
    payInTransactionId: Option[String] = None,
    clientId: Option[String] = None
  )(implicit
    system: ActorSystem[_]
  ): Future[Either[PayOutFailed, PaidOut]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(
      PayOut(
        orderUuid,
        creditedAccount,
        creditedAmount,
        feesAmount,
        currency,
        externalReference,
        payInTransactionId,
        clientId
      )
    ) map {
      case result: PaidOut     => Right(result)
      case error: PayOutFailed => Left(error)
      case error: PaymentError =>
        Left(
          PayOutFailed("", Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED, error.message)
        )
      case _ =>
        Left(PayOutFailed("", Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED, "unknown"))
    }
  }

  @InternalApi
  private[payment] def loadPayOutTransaction(
    orderUuid: String,
    transactionId: String,
    clientId: Option[String] = None
  )(implicit
    system: ActorSystem[_]
  ): Future[Either[TransactionNotFound.type, PayOutTransactionLoaded]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(LoadPayOutTransaction(orderUuid, transactionId, clientId)) map {
      case result: PayOutTransactionLoaded => Right(result)
      case _                               => Left(TransactionNotFound)
    }
  }

  @InternalApi
  private[payment] def transfer(
    orderUuid: Option[String] = None,
    debitedAccount: String,
    creditedAccount: String,
    debitedAmount: Int,
    feesAmount: Int = 0,
    currency: String = "EUR",
    payOutRequired: Boolean = true,
    externalReference: Option[String] = None,
    clientId: Option[String] = None
  )(implicit system: ActorSystem[_]): Future[Either[TransferFailed, Transferred]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(
      Transfer(
        orderUuid,
        debitedAccount,
        creditedAccount,
        debitedAmount,
        feesAmount,
        currency,
        payOutRequired,
        externalReference,
        clientId
      )
    ) map {
      case result: Transferred   => Right(result)
      case error: TransferFailed => Left(error)
      case error: PaymentError =>
        Left(
          TransferFailed("", Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED, error.message)
        )
      case _ =>
        Left(TransferFailed("", Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED, "unknown"))
    }
  }

  @InternalApi
  private[payment] def cancelMandate(
    creditedAccount: String,
    clientId: Option[String] = None
  )(implicit system: ActorSystem[_]): Future[Either[String, Boolean]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(CancelMandate(creditedAccount, clientId)) map {
      case MandateCanceled     => Right(true)
      case error: PaymentError => Left(error.message)
      case _                   => Left("unknown")
    }
  }

  @InternalApi
  private[payment] def directDebit(
    creditedAccount: String,
    debitedAmount: Int,
    feesAmount: Int = 0,
    currency: String = "EUR",
    statementDescriptor: String,
    externalReference: Option[String] = None,
    clientId: Option[String] = None
  )(implicit
    system: ActorSystem[_]
  ): Future[Either[DirectDebitFailed, DirectDebited]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(
      DirectDebit(
        creditedAccount,
        debitedAmount,
        feesAmount,
        currency,
        statementDescriptor,
        externalReference,
        clientId
      )
    ) map {
      case result: DirectDebited    => Right(result)
      case error: DirectDebitFailed => Left(error)
      case error: PaymentError =>
        Left(
          DirectDebitFailed(
            "",
            Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
            error.message
          )
        )
      case _ =>
        Left(
          DirectDebitFailed("", Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED, "unknown")
        )
    }
  }

  @InternalApi
  private[payment] def loadDirectDebitTransaction(
    directDebitTransactionId: String,
    clientId: Option[String] = None
  )(implicit
    system: ActorSystem[_]
  ): Future[Either[DirectDebitFailed, DirectDebited]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(LoadDirectDebitTransaction(directDebitTransactionId, clientId)) map {
      case result: DirectDebited    => Right(result)
      case error: DirectDebitFailed => Left(error)
      case error: PaymentError =>
        Left(
          DirectDebitFailed(
            "",
            Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
            error.message
          )
        )
      case _ =>
        Left(
          DirectDebitFailed("", Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED, "unknown")
        )
    }
  }

  @InternalApi
  private[payment] def registerRecurringPayment(
    debitedAccount: String,
    firstDebitedAmount: Int,
    firstFeesAmount: Int,
    currency: String = "EUR",
    `type`: RecurringPayment.RecurringPaymentType = RecurringPayment.RecurringPaymentType.CARD,
    startDate: Option[Date] = None,
    endDate: Option[Date] = None,
    frequency: Option[RecurringPayment.RecurringPaymentFrequency] = None,
    fixedNextAmount: Option[Boolean] = None,
    nextDebitedAmount: Option[Int] = None,
    nextFeesAmount: Option[Int] = None,
    statementDescriptor: Option[String] = None,
    externalReference: Option[String] = None,
    clientId: Option[String] = None
  )(implicit
    system: ActorSystem[_]
  ): Future[Either[String, RecurringPaymentRegistered]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    !?(
      RegisterRecurringPayment(
        debitedAccount,
        firstDebitedAmount,
        firstFeesAmount,
        currency,
        `type`,
        startDate,
        endDate,
        frequency,
        fixedNextAmount,
        nextDebitedAmount,
        nextFeesAmount,
        statementDescriptor,
        externalReference,
        Some(clientId)
      )
    ) map {
      case result: RecurringPaymentRegistered => Right(result)
      case error: PaymentError                => Left(error.message)
      case _ =>
        Left(
          "unknown"
        )
    }
  }
}

object PaymentDao extends PaymentDao {
  lazy val log: Logger = LoggerFactory.getLogger(getClass.getName)
}
