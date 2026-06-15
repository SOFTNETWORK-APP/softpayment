package app.softnetwork.payment.message

import app.softnetwork.payment.message.PaymentMessages.{ExecuteNextRecurringPayment, PayIn}
import com.esotericsoftware.kryo.io.{Input, Output}
import com.twitter.chill.ScalaKryoInstantiator
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Story 13.7 — locks the `AuditableCommand` contract for payment commands: a `correlationId` set at
  * the checkout endpoint (from `X-Correlation-Id`) must ride the command across the cluster-sharding
  * boundary, since commands are sent via `!?` under the chill/Kryo serializer. The handler then stamps
  * it onto the persisted payment event (the durable hop to the licensing pod). If this regresses we
  * fall back to a constructor field or a dedicated `serialization-binding` for `AuditableCommand`.
  */
class AuditableCommandSpec extends AnyWordSpec with Matchers {

  private def kryoRoundTrip[T <: AnyRef](value: T): T = {
    val instantiator = new ScalaKryoInstantiator()
    instantiator.setRegistrationRequired(false)
    val kryo = instantiator.newKryo()
    val out = new Output(4096)
    kryo.writeClassAndObject(out, value)
    out.flush()
    kryo.readClassAndObject(new Input(out.toBytes)).asInstanceOf[T]
  }

  "A payment AuditableCommand" should {

    "expose auditable=false until a correlation id is set, then carry it in place" in {
      val cmd = PayIn("o1", "debited", 5100, creditedAccount = "credited")
      cmd.auditable shouldBe false
      val same = cmd.withCorrelationId("cid-xyz")
      same should be theSameInstanceAs cmd // in-place; keeps the concrete type for `!?`
      cmd.correlationId shouldBe Some("cid-xyz")
      cmd.auditable shouldBe true
    }

    "carry correlationId (the trait var) across a chill/Kryo round-trip on PayIn" in {
      val cmd = PayIn("o1", "debited", 5100, creditedAccount = "credited")
      cmd.withCorrelationId("cid-xyz")
      val restored = kryoRoundTrip(cmd)
      restored.correlationId shouldBe Some("cid-xyz")
      restored.orderUuid shouldBe "o1"
      restored.debitedAmount shouldBe 5100
    }

    // The recurring path dispatches ExecuteNextRecurringPayment (scheduled renewals) — lock the
    // inherited trait `var` round-trips on that shape too.
    "carry correlationId across a Kryo round-trip on ExecuteNextRecurringPayment" in {
      val cmd = ExecuteNextRecurringPayment("reg#1", "debited")
      cmd.withCorrelationId("schedule#cid")
      val restored = kryoRoundTrip(cmd)
      restored.correlationId shouldBe Some("schedule#cid")
      restored.recurringPaymentRegistrationId shouldBe "reg#1"
    }
  }
}
