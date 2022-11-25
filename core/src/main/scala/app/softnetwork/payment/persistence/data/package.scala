package app.softnetwork.payment.persistence

import app.softnetwork.kv.handlers.Kv2Dao

package object data {
  lazy val paymentKvDao: Kv2Dao = Kv2Dao("payment")
}
