package app.softnetwork.payment.scalatest

import app.softnetwork.account.message.{BasicAccountSignUp, SignUp}
import app.softnetwork.account.model._
import app.softnetwork.account.scalatest.AccountRouteSpec
import app.softnetwork.api.server.ApiRoutes
import app.softnetwork.payment.message.AccountMessages.SoftPaySignUp
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.persistence.ManifestWrapper
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import org.slf4j.{Logger, LoggerFactory}

trait SoftPayAccountRouteSpec[SD <: SessionData with SessionDataDecorator[SD]]
    extends AccountRouteSpec[
      SoftPayAccount,
      BasicAccountProfile,
      DefaultProfileView,
      DefaultAccountDetailsView,
      DefaultAccountView[DefaultProfileView, DefaultAccountDetailsView],
      SD
    ]
    with SoftPayRouteTestKit[SD]
    with ManifestWrapper[DefaultAccountView[DefaultProfileView, DefaultAccountDetailsView]] {
  _: ApiRoutes with SessionMaterials[SD] =>

  override protected val manifestWrapper: ManifestW = ManifestW()

  lazy val log: Logger = LoggerFactory getLogger getClass.getName

  override val profile: BasicAccountProfile =
    BasicAccountProfile.defaultInstance
      .withName("name")
      .withType(ProfileType.CUSTOMER)
      .withFirstName(firstName)
      .withLastName(lastName)

  override implicit def lppToSignUp: ((String, String, Option[BasicAccountProfile])) => SignUp = {
    case (login, password, profile) =>
      SoftPaySignUp(login, password, provider = provider, profile = profile)
  }

}
