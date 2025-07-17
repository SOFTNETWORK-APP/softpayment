package app.softnetwork.payment.scalatest

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.RawHeader
import app.softnetwork.api.server.ApiRoutes
import app.softnetwork.api.server.config.ServerSettings.RootPath
import app.softnetwork.payment.config.{PaymentSettings, StripeApi}
import app.softnetwork.payment.model.SoftPayAccount.Client.Provider.ProviderType
import app.softnetwork.security.sha256
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import com.google.gson.Gson
import com.stripe.Stripe
import com.stripe.model.Account
import com.stripe.net.Webhook
import org.scalatest.Suite

import java.time.Instant
import scala.sys.process.Process
import scala.util.{Failure, Success, Try}

trait StripePaymentRouteTestKit[SD <: SessionData with SessionDataDecorator[SD]]
    extends PaymentRouteTestKit[SD]
    with StripePaymentTestKit { _: Suite with ApiRoutes with SessionMaterials[SD] =>

  private[this] var stripeCLi: Process = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    val hash = sha256(clientId)
    stripeCLi = Process(
      s"stripe listen --forward-to ${providerConfig.hooksBaseUrl.replace("9000", s"$port").replace("localhost", interface)}?hash=$hash"
    ).run()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    if (stripeCLi.isAlive())
      stripeCLi.destroy()
  }

  override def validateKycDocuments(): Unit = {
    val paymentAccount = loadPaymentAccount()
    paymentAccount.userId match {
      case Some(accountId) =>
        Try {
          Account.retrieve(accountId, StripeApi().requestOptions())
        } match {
          case Success(account) =>
            val accountPayload = new Gson().toJson(account)
            log.info(s"Account object: $accountPayload")

            val testPayload = //FIXME use the account payload
              s"""{
                 |  "id": "$accountId",
                 |  "object": "account",
                 |  "business_profile": {
                 |    "mcc": "5734",
                 |    "name": "My Business Name",
                 |    "product_description": "Online video streaming service",
                 |    "support_address": {
                 |      "city": "San Francisco",
                 |      "country": "US",
                 |      "line1": "123 Main Street",
                 |      "line2": null,
                 |      "postal_code": "94111",
                 |      "state": "CA"
                 |    },
                 |    "support_email": "support@mybusiness.com",
                 |    "support_phone": "+15555555555",
                 |    "support_url": "https://www.mybusiness.com/support",
                 |    "url": "https://www.mybusiness.com"
                 |  },
                 |  "business_type": "company",
                 |  "capabilities": {
                 |    "card_payments": "active",
                 |    "transfers": "active"
                 |  },
                 |  "charges_enabled": true,
                 |  "country": "US",
                 |  "default_currency": "usd",
                 |  "details_submitted": true,
                 |  "email": "admin@mybusiness.com",
                 |  "external_accounts": {
                 |    "object": "list",
                 |    "data": [
                 |      {
                 |        "id": "ba_1AbCdEfGhIjKlMnOpQrStUvWx",
                 |        "object": "bank_account",
                 |        "account_holder_name": "Jane Doe",
                 |        "account_holder_type": "individual",
                 |        "bank_name": "STRIPE TEST BANK",
                 |        "country": "US",
                 |        "currency": "usd",
                 |        "fingerprint": "1a2b3c4d5e6f7g8h",
                 |        "last4": "6789",
                 |        "routing_number": "110000000",
                 |        "status": "verified"
                 |      }
                 |    ],
                 |    "has_more": false,
                 |    "total_count": 1,
                 |    "url": "/v1/accounts/acct_1A2b3C4D5e6F7g8H/external_accounts"
                 |  },
                 |  "metadata": {
                 |    "user_id": "123456"
                 |  },
                 |  "payouts_enabled": true,
                 |  "requirements": {
                 |    "currently_due": [],
                 |    "eventually_due": [],
                 |    "past_due": [],
                 |    "pending_verification": []
                 |  },
                 |  "settings": {
                 |    "branding": {
                 |      "icon": null,
                 |      "logo": null,
                 |      "primary_color": "#123456",
                 |      "secondary_color": "#654321"
                 |    },
                 |    "card_payments": {
                 |      "decline_on": {
                 |        "avs_failure": false,
                 |        "cvc_failure": false
                 |      },
                 |      "statement_descriptor_prefix": "MYBIZ"
                 |    },
                 |    "payouts": {
                 |      "debit_negative_balances": true,
                 |      "schedule": {
                 |        "interval": "manual"
                 |      },
                 |      "statement_descriptor": "MYBUSINESS PAYOUT"
                 |    }
                 |  },
                 |  "tos_acceptance": {
                 |    "date": 1622546190,
                 |    "ip": "192.168.1.1",
                 |    "user_agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.77 Safari/537.36"
                 |  },
                 |  "type": "standard"
                 |}
                 |""".stripMargin

            val payload =
              s"""{
                 |  "id":"evt_${System.currentTimeMillis()}",
                 |  "object":"event",
                 |  "api_version":"${Stripe.API_VERSION}",
                 |  "created":${Instant.now().getEpochSecond},
                 |  "data":{
                 |    "object":$testPayload,
                 |    "previous_attributes":{}
                 |  },
                 |  "livemode":false,
                 |  "pending_webhooks": 1,
                 |  "request": {
                 |    "id": "req_${System.currentTimeMillis()}",
                 |    "idempotency_key": null
                 |  },
                 |  "type":"account.updated"
                 |}""".stripMargin.replaceAll("\\s", "")
            log.info(s"Payload: $payload")
            executeWebhook(payload)
          case Failure(f) =>
            fail(f)
        }
      case None => fail("No account found")
    }
  }

  private[this] def executeWebhook(payload: String): Unit = {
    val stripeApi = StripeApi()
    stripeApi.secret match {
      case Some(secret) =>
        val timestamp = Instant.now().getEpochSecond
        val signature = Webhook.Util.computeHmacSha256(secret, s"$timestamp.$payload")
        val sigHeader = s"t=$timestamp,${Webhook.Signature.EXPECTED_SCHEME}=$signature"
        Post(
          s"/$RootPath/${PaymentSettings.PaymentConfig.path}/${PaymentSettings.PaymentConfig.hooksRoute}/${ProviderType.STRIPE.name.toLowerCase}?hash=${stripeApi.hash}",
          payload
        ).withHeaders(
          RawHeader("Stripe-Signature", sigHeader)
        ) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
        }
      case None => fail("No secret found")
    }
  }
}
