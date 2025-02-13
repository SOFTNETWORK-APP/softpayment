package app.softnetwork.payment.config

import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.payment.model.SoftPayAccount.Client.Provider
import app.softnetwork.security.sha256
import com.stripe.Stripe
import com.stripe.model.WebhookEndpoint
import com.stripe.net.RequestOptions
import com.stripe.net.RequestOptions.RequestOptionsBuilder
import com.stripe.param.{
  WebhookEndpointCreateParams,
  WebhookEndpointListParams,
  WebhookEndpointUpdateParams
}

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

  private[this] var stripeApis: Map[String, StripeApi] = Map.empty

  private[this] var stripeWebHooks: Map[String, String] = Map.empty

  private[this] lazy val STRIPE_SECRETS_DIR: String = s"${SoftPayClientSettings.SP_SECRETS}/stripe"

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
    Console.println(s"Loading secret from: ${file.getAbsolutePath}")
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

        // create / update stripe webhook endpoint

        val hash = sha256(provider.clientId)

        val stripeApi = StripeApi(baseUrl, clientId, apiKey, hash)

        val requestOptions = stripeApi.requestOptions()

        val url = s"${config.hooksBaseUrl}?hash=$hash"

        import collection.JavaConverters._

        Try {
          ((Option(
            WebhookEndpoint
              .list(
                WebhookEndpointListParams.builder().setLimit(3L).build(),
                requestOptions
              )
              .getData
          ) match {
            case Some(data) =>
              data.asScala.headOption
            case _ =>
              None
          }) match {
            case Some(webhookEndpoint) =>
              Console.println(s"Webhook endpoint found: ${webhookEndpoint.getId}")
              loadSecret(hash) match {
                case None =>
                  Try(webhookEndpoint.delete(requestOptions))
                  None
                case value =>
                  val url = s"${config.hooksBaseUrl}?hash=$hash"
                  Try(
                    webhookEndpoint.update(
                      WebhookEndpointUpdateParams
                        .builder()
                        .addEnabledEvent(
                          WebhookEndpointUpdateParams.EnabledEvent.ACCOUNT__UPDATED
                        )
                        .addEnabledEvent(
                          WebhookEndpointUpdateParams.EnabledEvent.PERSON__UPDATED
                        )
                        .setUrl(url)
                        .build(),
                      requestOptions
                    )
                  )
                  value
              }
            case _ =>
              None
          }).getOrElse {
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
                  .setUrl(url)
                  .setApiVersion(WebhookEndpointCreateParams.ApiVersion.VERSION_2024_06_20)
                  .setConnect(true)
                  .build(),
                requestOptions
              )
              .getSecret
          }

        } match {
          case Success(secret) =>
            stripeApis = stripeApis.updated(provider.providerId, stripeApi)
            addSecret(hash, secret)
            stripeApi
          case Failure(f) =>
            Console.err.println(s"Error creating stripe webhook endpoint: ${f.getMessage}")
            throw f
        }
    }
  }

  def webHookSecret(hash: String): Option[String] =
    stripeWebHooks.get(hash).orElse(loadSecret(hash))
}
