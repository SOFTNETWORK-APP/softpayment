package app.softnetwork.payment.config

import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.payment.model.SoftPayAccount.Client.Provider
import app.softnetwork.security.sha256
import com.stripe.Stripe
import com.stripe.model.WebhookEndpoint
import com.stripe.net.RequestOptions
import com.stripe.net.RequestOptions.RequestOptionsBuilder
import com.stripe.param.{WebhookEndpointCreateParams, WebhookEndpointListParams}

import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

case class StripeApi(
  private val baseUrl: String,
  private val clientId: String,
  private val apiKey: String,
  hash: String
) {
  private def requestOptionsBuilder(stripeAccount: Option[String]): RequestOptionsBuilder = {
    val options = RequestOptions
      .builder()
      .setBaseUrl(baseUrl)
      .setClientId(clientId)
      .setApiKey(apiKey)
    stripeAccount match {
      case Some(value) =>
        options.setStripeAccount(value)
      case _ =>
        options
    }
  }

  def requestOptions(stripeAccount: Option[String] = None): RequestOptions =
    requestOptionsBuilder(stripeAccount).build()

  lazy val secret: Option[String] = StripeApi.loadSecret(hash)
}

object StripeApi {

  case class Config(
    override val clientId: String,
    override val apiKey: String,
    override val baseUrl: String,
    override val version: String = Stripe.VERSION,
    override val debug: Boolean,
    override val secureModePath: String,
    override val hooksPath: String,
    override val mandatePath: String,
    override val paypalPath: String,
    // Whether the (single) webhook endpoint registered for this provider is a Connect endpoint:
    //   true  -> receives events from CONNECTED accounts (account.updated, person.updated, …)
    //   false -> receives events from the PLATFORM account (customer.updated, invoice.*,
    //            customer.subscription.*, payment_method.*) — what the license-server needs.
    // Defaults to false. See the setConnect(...) call below.
    connected: Boolean = false,
    paymentConfig: Payment.Config = PaymentSettings.PaymentConfig
  ) extends ProviderConfig(
        clientId,
        apiKey,
        baseUrl,
        version,
        debug,
        secureModePath,
        hooksPath,
        mandatePath,
        paypalPath
      ) {
    override val `type`: Provider.ProviderType = Provider.ProviderType.STRIPE

    override def withPaymentConfig(paymentConfig: Payment.Config): Config =
      this.copy(paymentConfig = paymentConfig)
  }

  private[this] lazy val log: Logger = LoggerFactory.getLogger(getClass)

  private[this] var stripeApis: Map[String, StripeApi] = Map.empty

  private[this] var stripeWebHooks: Map[String, String] = Map.empty

  private[this] lazy val STRIPE_SECRETS_DIR: String = s"${SoftPayClientSettings.SP_SECRETS}/stripe"

  // TODO migrate webhook secrets to encrypted storage (Sealed Secrets / Vault)
  private[this] def addSecret(hash: String, secret: String): Unit = {
    val dir = s"$STRIPE_SECRETS_DIR/$hash"
    Paths.get(dir).toFile.mkdirs()
    val file = Paths.get(dir, "webhook-secret").toFile
    file.createNewFile()
    val secretWriter = new java.io.BufferedWriter(new java.io.FileWriter(file))
    secretWriter.write(secret)
    secretWriter.close()
    stripeWebHooks = stripeWebHooks.updated(hash, secret)
  }

  private[StripeApi] def loadSecret(hash: String): Option[String] = {
    val dir = s"$STRIPE_SECRETS_DIR/$hash"
    val file = Paths.get(dir, "webhook-secret").toFile
    log.debug(s"Loading secret from: ${file.getAbsolutePath}")
    if (file.exists()) {
      import scala.io.Source
      val source = Source.fromFile(file)
      val secret = source.getLines().mkString
      source.close()
      stripeWebHooks = stripeWebHooks.updated(hash, secret)
      Some(secret)
    } else {
      None
    }
  }

  def apply()(implicit provider: SoftPayAccount.Client.Provider, config: Config): StripeApi = {
    stripeApis.get(provider.providerId) match {
      case Some(stripeApi) => stripeApi
      case _               =>
        // init Stripe request options
        val baseUrl = config.baseUrl
        val clientId = provider.providerId
        val apiKey = provider.providerApiKey

        // (re)create stripe webhook endpoint

        val hash = sha256(provider.clientId)

        val stripeApi = StripeApi(baseUrl, clientId, apiKey, hash)

        val requestOptions = stripeApi.requestOptions()

        val url = s"${config.hooksBaseUrl}?hash=$hash"

        log.info(
          s"Provisioning (delete + recreate) Stripe webhook endpoint for provider ${provider.providerId} at ${config.hooksBaseUrl}?hash=*****"
        )

        import scala.jdk.CollectionConverters._

        Try {
          // Stripe returns a webhook endpoint's signing secret ONLY at creation time — never on
          // list / retrieve / update, and stripe-java 26.12.0 exposes no roll-secret API. The only
          // way to guarantee that the locally stored secret matches the one Stripe actually uses is
          // therefore to (re)create the endpoint and capture the secret it returns. So we always
          // delete any endpoint already registered for this provider's URL, then create a fresh one.
          // The URL carries the per-client hash, so we only ever match — and delete — THIS
          // provider's endpoint(s), never another client's. We materialize the matches before
          // deleting so paging is not disturbed by the deletions.
          WebhookEndpoint
            .list(
              WebhookEndpointListParams.builder().setLimit(100L).build(),
              requestOptions
            )
            .autoPagingIterable()
            .asScala
            .filter(endpoint => Option(endpoint.getUrl).exists(_.contains(url)))
            .toList
            .foreach { endpoint =>
              log.info(
                s"Deleting existing Stripe webhook endpoint ${endpoint.getId} to refresh its signing secret"
              )
              Try(endpoint.delete(requestOptions)) match {
                case Failure(f) =>
                  log.warn(
                    s"Failed to delete Stripe webhook endpoint ${endpoint.getId}: ${f.getMessage}"
                  )
                case _ =>
              }
            }

          WebhookEndpoint
            .create(
              WebhookEndpointCreateParams
                .builder()
                .addEnabledEvent(
                  WebhookEndpointCreateParams.EnabledEvent.ACCOUNT__UPDATED
                )
                .addEnabledEvent(
                  WebhookEndpointCreateParams.EnabledEvent.PERSON__UPDATED
                )
                .addEnabledEvent(
                  WebhookEndpointCreateParams.EnabledEvent.INVOICE__PAYMENT_SUCCEEDED
                )
                .addEnabledEvent(
                  WebhookEndpointCreateParams.EnabledEvent.INVOICE__PAYMENT_FAILED
                )
                .addEnabledEvent(
                  WebhookEndpointCreateParams.EnabledEvent.CUSTOMER__SUBSCRIPTION__DELETED
                )
                .addEnabledEvent(
                  WebhookEndpointCreateParams.EnabledEvent.CUSTOMER__SUBSCRIPTION__UPDATED
                )
                .addEnabledEvent(
                  WebhookEndpointCreateParams.EnabledEvent.CUSTOMER__UPDATED
                )
                .addEnabledEvent(
                  WebhookEndpointCreateParams.EnabledEvent.PAYMENT_METHOD__ATTACHED
                )
                .addEnabledEvent(
                  WebhookEndpointCreateParams.EnabledEvent.PAYMENT_METHOD__DETACHED
                )
                .setUrl(url)
                .setApiVersion(WebhookEndpointCreateParams.ApiVersion.VERSION_2024_06_20)
                // connect=true -> events from connected accounts only; connect=false -> events from
                // the platform account (customer.updated, invoice.*, subscription.*, payment_method.*).
                // Driven by payment.stripe.connected (default false). NOTE: `connect` is immutable
                // after endpoint creation, so flipping this requires deleting the existing endpoint
                // so it is recreated. FUTURE: support several webhook endpoints per provider (one per
                // scope) — not on the roadmap and not needed by the license-server (single endpoint,
                // connected=false, suffices to receive customer.updated for org sync).
                .setConnect(config.connected)
                .build(),
              requestOptions
            )
            .getSecret

        } match {
          case Success(secret) =>
            stripeApis = stripeApis.updated(provider.providerId, stripeApi)
            addSecret(hash, secret)
            stripeApi
          case Failure(f) =>
            Console.err.println(s"Error creating stripe webhook endpoint: ${f.getMessage}")
            if (apiKey.startsWith("sk_test_")) { // In test mode, we can proceed without a webhook endpoint
              stripeApis = stripeApis.updated(provider.providerId, stripeApi)
              stripeApi
            } else {
              throw f
            }
        }
    }
  }

  def webHookSecret(hash: String): Option[String] =
    stripeWebHooks.get(hash).orElse(loadSecret(hash))

  /** Override the cached webhook secret for a given hash. Only allowed for test API keys
    * (sk_test_*) — used by test kit when Stripe CLI provides its own signing secret.
    */
  def overrideWebHookSecret(hash: String, secret: String)(implicit
    provider: SoftPayAccount.Client.Provider
  ): Unit = {
    if (provider.providerApiKey.startsWith("sk_test_")) {
      stripeWebHooks = stripeWebHooks.updated(hash, secret)
      addSecret(hash, secret)
    }
  }
}
