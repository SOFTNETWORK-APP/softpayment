package app.softnetwork.payment.launch

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.GrpcService
import app.softnetwork.payment.PaymentCoreBuildInfo
import app.softnetwork.payment.api.{
  ClientGrpcService,
  ClientServer,
  PaymentGrpcService,
  PaymentServer
}
import app.softnetwork.payment.handlers.SoftPayAccountDao
import app.softnetwork.payment.persistence.data.paymentKvDao
import app.softnetwork.payment.persistence.query.{
  PaymentCommandProcessorStream,
  Scheduler2PaymentProcessorStream
}
import app.softnetwork.payment.persistence.typed.{PaymentBehavior, SoftPayAccountBehavior}
import app.softnetwork.payment.spi.PaymentProviders
import app.softnetwork.persistence.launch.PersistentEntity
import app.softnetwork.persistence.query.EventProcessorStream
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.persistence.typed.Singleton
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.launch.SessionGuardian

import scala.concurrent.ExecutionContext

trait PaymentGuardian extends SessionGuardian { _: SchemaProvider with CsrfCheck =>

  import app.softnetwork.persistence.launch.PersistenceGuardian._

  def softPayAccountBehavior: ActorSystem[_] => SoftPayAccountBehavior = _ => SoftPayAccountBehavior

  def paymentBehavior: ActorSystem[_] => PaymentBehavior = _ => PaymentBehavior

  def paymentEntities: ActorSystem[_] => Seq[PersistentEntity[_, _, _, _]] = sys =>
    Seq(
      softPayAccountBehavior(sys),
      paymentBehavior(sys)
    )

  /** initialize all entities
    */
  override def entities: ActorSystem[_] => Seq[PersistentEntity[_, _, _, _]] = sys =>
    sessionEntities(sys) ++ paymentEntities(sys)

  /** initialize all singletons
    */
  override def singletons: ActorSystem[_] => Seq[Singleton[_]] = _ => Seq(paymentKvDao)

  def paymentCommandProcessorStream: ActorSystem[_] => PaymentCommandProcessorStream

  def scheduler2PaymentProcessorStream: ActorSystem[_] => Scheduler2PaymentProcessorStream

  def paymentEventProcessorStreams: ActorSystem[_] => Seq[EventProcessorStream[_]] = sys =>
    Seq(paymentCommandProcessorStream(sys)) :+ scheduler2PaymentProcessorStream(sys)

  /** initialize all event processor streams
    */
  override def eventProcessorStreams: ActorSystem[_] => Seq[EventProcessorStream[_]] = sys =>
    paymentEventProcessorStreams(sys)

  override def systemVersion(): String =
    sys.env.getOrElse("VERSION", PaymentCoreBuildInfo.version)

  def softPayAccountDao: SoftPayAccountDao = SoftPayAccountDao

  def paymentServer: ActorSystem[_] => PaymentServer = system => PaymentServer(system)

  def clientServer: ActorSystem[_] => ClientServer = system => ClientServer(system)

  def paymentGrpcServices: ActorSystem[_] => Seq[GrpcService] = system =>
    Seq(
      new PaymentGrpcService(paymentServer(system), softPayAccountDao),
      new ClientGrpcService(clientServer(system))
    )

  def registerProvidersAccount: ActorSystem[_] => Unit = system => {
    PaymentProviders
      .defaultPaymentProviders(config)
      .foreach(provider => {
        implicit val ec: ExecutionContext = system.executionContext
        softPayAccountDao.registerAccountWithProvider(provider)(system) map {
          case Some(account) =>
            system.log.info(s"Registered provider account for ${provider.providerId}: $account")
          case _ =>
            system.log.warn(s"Failed to register provider account for ${provider.providerId}")
        }
      })
  }

  override def initSystem: ActorSystem[_] => Unit = system => {
    registerProvidersAccount(system)
    super.initSystem(system)
  }

  override def banner: String =
    """
      |█████████              ██████   █████    ███████████                                                            █████
      | ███░░░░░███            ███░░███ ░░███    ░░███░░░░░███                                                          ░░███
      |░███    ░░░   ██████   ░███ ░░░  ███████   ░███    ░███  ██████   █████ ████ █████████████    ██████  ████████   ███████
      |░░█████████  ███░░███ ███████   ░░░███░    ░██████████  ░░░░░███ ░░███ ░███ ░░███░░███░░███  ███░░███░░███░░███ ░░░███░
      | ░░░░░░░░███░███ ░███░░░███░      ░███     ░███░░░░░░    ███████  ░███ ░███  ░███ ░███ ░███ ░███████  ░███ ░███   ░███
      | ███    ░███░███ ░███  ░███       ░███ ███ ░███         ███░░███  ░███ ░███  ░███ ░███ ░███ ░███░░░   ░███ ░███   ░███ ███
      |░░█████████ ░░██████   █████      ░░█████  █████       ░░████████ ░░███████  █████░███ █████░░██████  ████ █████  ░░█████
      | ░░░░░░░░░   ░░░░░░   ░░░░░        ░░░░░  ░░░░░         ░░░░░░░░   ░░░░░███ ░░░░░ ░░░ ░░░░░  ░░░░░░  ░░░░ ░░░░░    ░░░░░
      |                                                                   ███ ░███
      |                                                                  ░░██████
      |                                                                   ░░░░░░
      |""".stripMargin
}
