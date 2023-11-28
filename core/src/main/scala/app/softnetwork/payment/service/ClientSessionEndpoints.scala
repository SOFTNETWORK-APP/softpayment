package app.softnetwork.payment.service

import app.softnetwork.account.config.AccountSettings
import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.model.SoftPaymentAccount
import app.softnetwork.session._
import app.softnetwork.session.service.{SessionEndpoints, SessionMaterials}
import com.softwaremill.session.{
  CookieOrHeaderST,
  CookieST,
  HeaderST,
  SessionManager,
  SessionResult
}
import org.json4s.Formats
import org.softnetwork.session.model.Session
import sttp.model.headers.WWWAuthenticateChallenge
import sttp.monad.FutureMonad
import sttp.tapir._
import sttp.tapir.server.PartialServerEndpointWithSecurityOutput

import scala.concurrent.Future

trait ClientSessionEndpoints extends SessionEndpoints with ClientSession {
  _: SessionMaterials =>

  implicit def formats: Formats

  import TapirSessionOptions._

  @InternalApi
  private[payment] def requiredClientSession: PartialServerEndpointWithSecurityOutput[Seq[
    Option[String]
  ], (Option[SoftPaymentAccount.Client], Session), Unit, Unit, Seq[
    Option[String]
  ], Unit, Any, Future] = {
    val partial = clientSession(Some(true))
    partial.endpoint
      .out(partial.securityOutput)
      .serverSecurityLogicWithOutput { inputs =>
        partial.securityLogic(new FutureMonad())(inputs).map {
          case Left(l) => Left(l)
          case Right(r) =>
            r._2._2.toOption match {
              case Some(session) => Right((r._1, (r._2._1, session)))
              case _             => Left(())
            }
        }
      }
  }

  @InternalApi
  private[payment] def optionalClientSession: PartialServerEndpointWithSecurityOutput[Seq[
    Option[String]
  ], (Option[SoftPaymentAccount.Client], Option[Session]), Unit, Unit, Seq[
    Option[String]
  ], Unit, Any, Future] = {
    val partial = clientSession(Some(false))
    partial.endpoint
      .out(partial.securityOutput)
      .serverSecurityLogicWithOutput { inputs =>
        partial.securityLogic(new FutureMonad())(inputs).map {
          case Left(l)  => Left(l)
          case Right(r) => Right((r._1, (r._2._1, r._2._2.toOption)))
        }
      }
  }

  @InternalApi
  private[payment] def clientSession(
    required: Option[Boolean]
  ): PartialServerEndpointWithSecurityOutput[Seq[
    Option[String]
  ], (Option[SoftPaymentAccount.Client], SessionResult[Session]), Unit, Unit, Seq[
    Option[String]
  ], Unit, Any, Future] = {
    val partial = sc.session(gt, required)
    partial.endpoint
      .prependSecurityIn(
        auth.bearer[Option[String]](WWWAuthenticateChallenge.bearer(AccountSettings.Realm))
      )
      .mapSecurityIn(inputs => Seq(inputs._1) ++ inputs._2)(seq =>
        (seq.head, seq.slice(1, seq.size))
      )
      .out(partial.securityOutput)
      .serverSecurityLogicWithOutput { inputs =>
        (inputs.head match {
          case Some(token) => softPaymentAccountDao.oauthClient(token)
          case _           => Future.successful(None)
        }) flatMap { client =>
          implicit val manager: SessionManager[Session] = clientSessionManager(client)
          sessionType match {
            case Session.SessionType.OneOffCookie | Session.SessionType.OneOffHeader => // oneOff
              (gt match {
                case CookieST =>
                  sc.sessionLogic(None, inputs.tail.head, None, gt, required)
                case HeaderST =>
                  sc.sessionLogic(None, None, inputs.tail.head, gt, required)
                case CookieOrHeaderST =>
                  sc.sessionLogic(None, inputs.tail.head, inputs.last, gt, required)
              }) match {
                case Left(l)  => Future.successful(Left(l))
                case Right(r) => Future.successful(Right((r._1, (client, r._2))))
              }
            case _ => // refreshable
              (gt match {
                case CookieST =>
                  oneOff.sessionLogic(None, inputs.tail.head, None, gt, required) match {
                    case Left(l) => Left(l)
                    case Right(r) =>
                      refreshable.sessionLogic(Some(r._2), inputs.last, None, gt, required)
                  }
                case HeaderST =>
                  oneOff.sessionLogic(None, None, inputs.tail.head, gt, required) match {
                    case Left(l) => Left(l)
                    case Right(r) =>
                      refreshable.sessionLogic(Some(r._2), None, inputs.last, gt, required)
                  }
                case CookieOrHeaderST =>
                  val oneOffInputs = inputs.tail.take(2)
                  val refreshableInputs = inputs.takeRight(2)
                  oneOff.sessionLogic(
                    None,
                    oneOffInputs.head,
                    oneOffInputs.last,
                    gt,
                    required
                  ) match {
                    case Left(l) => Left(l)
                    case Right(r) =>
                      refreshable.sessionLogic(
                        Some(r._2),
                        refreshableInputs.head,
                        refreshableInputs.last,
                        gt,
                        required
                      )
                  }
              }) match {
                case Left(l)  => Future.successful(Left(l))
                case Right(r) => Future.successful(Right((r._1, (client, r._2))))
              }
          }
//          case None =>
//            partial.securityLogic(new FutureMonad())(inputs.tail).map {
//              case Left(l)  => Left(l)
//              case Right(r) => Right((r._1, (None, r._2)))
//            }
        }
      }
  }

}