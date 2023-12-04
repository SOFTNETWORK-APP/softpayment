package app.softnetwork.payment.service

import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.handlers.SoftPaymentAccountDao
import app.softnetwork.payment.model.{computeExternalUuidWithProfile, SoftPaymentAccount}
import app.softnetwork.session.model.{SessionData, SessionDataCompanion, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import org.softnetwork.session.model.{ApiKey, JwtClaims}
import com.softwaremill.session._

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success}

trait ClientSession[SD <: SessionData with SessionDataDecorator[SD]] extends Completion {
  self: SessionMaterials[SD] =>

  def softPaymentAccountDao: SoftPaymentAccountDao = SoftPaymentAccountDao

  implicit def sessionConfig: SessionConfig

  implicit def companion: SessionDataCompanion[SD]

  implicit def toSession(client: SoftPaymentAccount.Client): SD = {
    var session = companion.newSession
      .withAdmin(false)
      .withAnonymous(false)
      .withClientId(client.clientId)
    session += ("scope", client.accessToken.flatMap(_.scope).getOrElse(""))
    session
  }

  def encodeClient(
    client: SoftPaymentAccount.Client
  ): String = {
    clientSessionManager(client).clientSessionManager.encode(client)
  }

  def decodeClient(data: String): Option[SD] = manager.clientSessionManager.decode(data).toOption

  def clientSessionManager(client: SoftPaymentAccount.Client): SessionManager[SD] = {
    implicit val innerSessionConfig: SessionConfig =
      sessionConfig.copy(
        jwt = sessionConfig.jwt.copy(
          subject = Some(client.clientId)
        ),
        serverSecret = client.getClientApiKey
      )
    manager(innerSessionConfig, companion)
  }

  def clientCookieName: String = sessionConfig.sessionCookieConfig.name

  def sendToClientHeaderName: String =
    sessionConfig.sessionHeaderConfig.sendToClientHeaderName

  def getFromClientHeaderName: String =
    sessionConfig.sessionHeaderConfig.getFromClientHeaderName

  def sessionManager(clientId: Option[String]): SessionManager[SD] = {
    clientId match {
      case Some(id) =>
        softPaymentAccountDao.loadClient(id) complete () match {
          case Success(s) => clientSessionManager(s)
          case Failure(_) => manager
        }
      case _ => manager
    }
  }

  def clientSessionManager(client: Option[SoftPaymentAccount.Client]): SessionManager[SD] = {
    client match {
      case Some(c) =>
        implicit val innerSessionConfig: SessionConfig =
          sessionConfig.copy(
            jwt = sessionConfig.jwt.copy(
              subject = Some(c.clientId)
            ),
            serverSecret = c.getClientApiKey
          )
        manager(innerSessionConfig, companion)
      case _ => manager
    }
  }

  def loadApiKey(clientId: String): Future[Option[ApiKey]] =
    softPaymentAccountDao.loadApiKey(clientId)

  protected def toClient(token: Option[String]): Future[Option[SoftPaymentAccount.Client]] = {
    token match {
      case Some(value) =>
        softPaymentAccountDao.oauthClient(value) flatMap {
          case Some(client) => Future.successful(Some(client))
          case _ =>
            val jwt = JwtClaims(value)
            if (jwt.iss.contains(sessionConfig.jwt.issuer.getOrElse("")) && jwt.sub.isDefined) {
              softPaymentAccountDao.loadClient(jwt.sub.get) flatMap {
                case Some(client) => Future.successful(Some(client))
                case _            => Future.successful(None)
              }
            } else {
              Future.successful(None)
            }
        }
      case _ => Future.successful(None)
    }
  }

  protected[payment] def externalUuidWithProfile(session: SD): String =
    computeExternalUuidWithProfile(session.id, session.profile)

}
