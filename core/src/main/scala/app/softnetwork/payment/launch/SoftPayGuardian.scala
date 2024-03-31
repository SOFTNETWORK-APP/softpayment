package app.softnetwork.payment.launch

import akka.actor.typed.ActorSystem
import app.softnetwork.account.handlers.AccountDao
import app.softnetwork.account.launch.AccountGuardian
import app.softnetwork.account.model.BasicAccountProfile
import app.softnetwork.account.persistence.typed.AccountBehavior
import app.softnetwork.payment.PaymentCoreBuildInfo
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.persistence.launch.PersistentEntity
import app.softnetwork.persistence.query.EventProcessorStream
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck

trait SoftPayGuardian
    extends AccountGuardian[SoftPayAccount, BasicAccountProfile]
    with PaymentGuardian { _: SchemaProvider with CsrfCheck =>

  import app.softnetwork.persistence.launch.PersistenceGuardian._

  override def accountBehavior
    : ActorSystem[_] => AccountBehavior[SoftPayAccount, BasicAccountProfile] =
    softPayAccountBehavior

  override def paymentEntities: ActorSystem[_] => Seq[PersistentEntity[_, _, _, _]] = sys =>
    Seq(
      paymentBehavior(sys)
    )

  /** initialize all entities
    */
  override def entities: ActorSystem[_] => Seq[PersistentEntity[_, _, _, _]] = sys =>
    sessionEntities(sys) ++ accountEntities(sys) ++ paymentEntities(sys)

  /** initialize all event processor streams
    */
  override def eventProcessorStreams: ActorSystem[_] => Seq[EventProcessorStream[_]] = sys =>
    paymentEventProcessorStreams(sys) ++ accountEventProcessorStreams(sys)

  final override def accountDao: AccountDao = softPayAccountDao

  override def systemVersion(): String =
    sys.env.getOrElse("VERSION", PaymentCoreBuildInfo.version)
}
