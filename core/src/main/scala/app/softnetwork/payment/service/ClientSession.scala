package app.softnetwork.payment.service

import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.config.PaymentSettings.ClientSessionConfig
import app.softnetwork.payment.handlers.SoftPaymentAccountDao
import app.softnetwork.payment.model.SoftPaymentAccount
import app.softnetwork.session.model.JwtClaims
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session._
import org.softnetwork.session.model.Session

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success}

trait ClientSession extends Completion { _: SessionMaterials =>

  def softPaymentAccountDao: SoftPaymentAccountDao = SoftPaymentAccountDao

  implicit def sessionConfig: SessionConfig

  implicit def toSession(client: SoftPaymentAccount.Client): Session = {
    var session = Session(client.clientId)
    session += (Session.adminKey, "false")
    session += (Session.anonymousKey, "false")
    session += ("iss", ClientSessionConfig.jwt.issuer.getOrElse(""))
    session += ("sub", client.clientId)
    session += ("scope", client.accessToken.flatMap(_.scope).getOrElse(""))
    session
  }

  def encodeClient(
    client: SoftPaymentAccount.Client
  ): String = {
    clientSessionManager(client).clientSessionManager.encode(client)
  }

  def decodeClient(data: String): Option[Session] =
    manager(ClientSessionConfig).clientSessionManager.decode(data).toOption

  def clientSessionManager(client: SoftPaymentAccount.Client): SessionManager[Session] = {
    implicit val innerSessionConfig: SessionConfig =
      ClientSessionConfig.copy(
        jwt = ClientSessionConfig.jwt.copy(subject = Some(client.clientId)),
        serverSecret = client.getClientApiKey
      )
    manager(innerSessionConfig)
  }

  def clientCookieName: String = ClientSessionConfig.sessionCookieConfig.name

  def sendToClientHeaderName: String =
    ClientSessionConfig.sessionHeaderConfig.sendToClientHeaderName

  def getFromClientHeaderName: String =
    ClientSessionConfig.sessionHeaderConfig.getFromClientHeaderName

  def sessionManager(clientId: Option[String]): SessionManager[Session] = {
    clientId match {
      case Some(id) =>
        softPaymentAccountDao.loadClient(id) complete () match {
          case Success(s) => clientSessionManager(s)
          case Failure(_) => manager
        }
      case _ => manager
    }
  }

  def clientSessionManager(client: Option[SoftPaymentAccount.Client]): SessionManager[Session] = {
    client match {
      case Some(c) =>
        implicit val innerSessionConfig: SessionConfig =
          sessionConfig.copy(
            jwt = sessionConfig.jwt.copy(
              issuer = ClientSessionConfig.jwt.issuer,
              subject = Some(c.clientId)
            ),
            sessionEncryptData = true,
            serverSecret = c.getClientApiKey
          )
        manager(innerSessionConfig)
      case _ => manager
    }
  }

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

}
