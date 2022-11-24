package app.softnetwork.payment.handlers

import app.softnetwork.kv.handlers.{KvDao, KvHandler}
import app.softnetwork.payment.persistence.typed.PaymentKvBehavior

trait PaymentKvDao extends KvDao with KvHandler with PaymentKvBehavior

object PaymentKvDao extends PaymentKvDao
