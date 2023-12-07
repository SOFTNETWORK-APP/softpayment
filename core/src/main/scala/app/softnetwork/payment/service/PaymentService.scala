package app.softnetwork.payment.service

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.{Directives, Route}
import app.softnetwork.api.server.DefaultComplete
import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.serialization._
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.config.{Settings => CommonSettings}
import akka.http.javadsl.model.headers.{AcceptLanguage, UserAgent}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.directives.FileInfo
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.ByteString
import app.softnetwork.session.service.{ServiceWithSessionDirectives, SessionMaterials}
import com.softwaremill.session.CsrfDirectives.hmacTokenCsrfProtection
import com.softwaremill.session.CsrfOptions.checkHeader
import com.typesafe.scalalogging.StrictLogging
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.{jackson, Formats}
import org.json4s.jackson.Serialization
import app.softnetwork.api.server._
import app.softnetwork.payment.config.PaymentSettings._
import app.softnetwork.payment.model._
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import com.softwaremill.session.SessionConfig

import java.io.ByteArrayOutputStream
import scala.concurrent.Await
import scala.language.implicitConversions

trait PaymentService[SD <: SessionData with SessionDataDecorator[SD]]
    extends Directives
    with DefaultComplete
    with Json4sSupport
    with StrictLogging
    with BasicPaymentService
    with ServiceWithSessionDirectives[PaymentCommand, PaymentResult, SD]
    with ClientSessionDirectives[SD]
    with ApiRoute { _: PaymentHandler with SessionMaterials[SD] =>

  implicit def serialization: Serialization.type = jackson.Serialization

  implicit def formats: Formats = paymentFormats

  implicit def sessionConfig: SessionConfig

  override implicit def ts: ActorSystem[_] = system

  val route: Route = {
    pathPrefix(PaymentSettings.PaymentPath) {
      hooks ~
      card ~
      payInFor3ds ~
      payInForPayPal ~
      preAuthorizeCardFor3ds ~
      payInFirstRecurringFor3ds ~
      bank ~
      declaration ~
      kyc ~
      mandate ~
      recurringPayment ~
      payment
    }
  }

  lazy val card: Route = pathPrefix(CardRoute) {
    // check anti CSRF token
    hmacTokenCsrfProtection(checkHeader) {
      // check if a session exists
      requiredClientSession { (client, session) =>
        pathEnd {
          get {
            run(LoadCards(externalUuidWithProfile(session))) completeWith {
              case r: CardsLoaded =>
                complete(
                  HttpResponse(
                    StatusCodes.OK,
                    entity = r.cards.map(_.view)
                  )
                )
              case other => error(other)
            }
          } ~
          post {
            entity(as[PreRegisterCard]) { cmd =>
              var updatedUser =
                if (cmd.user.externalUuid.trim.isEmpty) {
                  cmd.user.withExternalUuid(session.id)
                } else {
                  cmd.user
                }
              session.profile match {
                case Some(profile) if updatedUser.profile.isEmpty =>
                  updatedUser = updatedUser.withProfile(profile)
                case _ =>
              }
              run(
                cmd.copy(
                  user = updatedUser,
                  clientId = client.map(_.clientId).orElse(session.clientId)
                )
              ) completeWith {
                case r: CardPreRegistered =>
                  complete(
                    HttpResponse(
                      StatusCodes.OK,
                      entity = r.cardPreRegistration
                    )
                  )
                case other => error(other)
              }
            }
          } ~
          delete {
            parameter("cardId") { cardId =>
              run(DisableCard(externalUuidWithProfile(session), cardId)) completeWith {
                case _: CardDisabled.type => complete(HttpResponse(StatusCodes.OK))
                case other                => error(other)
              }
            }
          }
        }
      }
    }
  }

  lazy val payment: Route = {
    // check anti CSRF token
    hmacTokenCsrfProtection(checkHeader) {
      // check if a session exists
      requiredClientSession { (client, session) =>
        get {
          pathEnd {
            run(
              LoadPaymentAccount(
                externalUuidWithProfile(session),
                clientId = client.map(_.clientId).orElse(session.clientId)
              )
            ) completeWith {
              case r: PaymentAccountLoaded =>
                complete(HttpResponse(StatusCodes.OK, entity = r.paymentAccount.view))
              case other => error(other)
            }
          }
        } ~
        post {
          optionalHeaderValueByType[AcceptLanguage]((): Unit) { language =>
            optionalHeaderValueByType[Accept]((): Unit) { acceptHeader =>
              optionalHeaderValueByType[UserAgent]((): Unit) { userAgent =>
                extractClientIP { ipAddress =>
                  entity(as[Payment]) { payment =>
                    import payment._
                    val browserInfo =
                      extractBrowserInfo(
                        language.map(_.value()),
                        acceptHeader.map(_.value()),
                        userAgent.map(_.value()),
                        payment
                      )
                    pathPrefix(PreAuthorizeCardRoute) {
                      run(
                        PreAuthorizeCard(
                          orderUuid,
                          externalUuidWithProfile(session),
                          debitedAmount,
                          currency,
                          registrationId,
                          registrationData,
                          registerCard,
                          if (browserInfo.isDefined) Some(ipAddress) else None,
                          browserInfo,
                          printReceipt
                        )
                      ) completeWith {
                        case r: CardPreAuthorized =>
                          complete(
                            HttpResponse(
                              StatusCodes.OK,
                              entity = r
                            )
                          )
                        case r: PaymentRedirection =>
                          complete(
                            HttpResponse(
                              StatusCodes.Accepted,
                              entity = r
                            )
                          )
                        case other => error(other)
                      }
                    } ~ pathPrefix(PayInRoute) {
                      pathPrefix(Segment) { creditedAccount =>
                        run(
                          PayIn(
                            orderUuid,
                            externalUuidWithProfile(session),
                            debitedAmount,
                            currency,
                            creditedAccount,
                            registrationId,
                            registrationData,
                            registerCard,
                            if (browserInfo.isDefined) Some(ipAddress) else None,
                            browserInfo,
                            statementDescriptor,
                            paymentType,
                            printReceipt
                          )
                        ) completeWith {
                          case r: PaidIn =>
                            complete(
                              HttpResponse(
                                StatusCodes.OK,
                                entity = r
                              )
                            )
                          case r: PaymentRedirection =>
                            complete(
                              HttpResponse(
                                StatusCodes.Accepted,
                                entity = r
                              )
                            )
                          case other => error(other)
                        }
                      }
                    } ~ pathPrefix(RecurringPaymentRoute) {
                      pathPrefix(Segment) { recurringPaymentRegistrationId =>
                        run(
                          PayInFirstRecurring(
                            recurringPaymentRegistrationId,
                            externalUuidWithProfile(session),
                            if (browserInfo.isDefined) Some(ipAddress) else None,
                            browserInfo,
                            statementDescriptor
                          )
                        ) completeWith {
                          case r: FirstRecurringPaidIn =>
                            complete(
                              HttpResponse(
                                StatusCodes.OK,
                                entity = r
                              )
                            )
                          case r: PaymentRedirection =>
                            complete(
                              HttpResponse(
                                StatusCodes.Accepted,
                                entity = r
                              )
                            )
                          case other => error(other)
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  lazy val payInFor3ds: Route = pathPrefix(SecureModeRoute / PayInRoute) {
    pathPrefix(Segment) { orderUuid =>
      parameters("transactionId", "registerCard".as[Boolean], "printReceipt".as[Boolean]) {
        (transactionId, registerCard, printReceipt) =>
          run(PayInFor3DS(orderUuid, transactionId, registerCard, printReceipt)) completeWith {
            case r: PaidIn =>
              complete(
                HttpResponse(
                  StatusCodes.OK,
                  entity = r
                )
              )
            case r: PaymentRedirection =>
              complete(
                HttpResponse(
                  StatusCodes.Accepted,
                  entity = r
                )
              )
            case other => error(other)
          }
      }
    }
  }

  lazy val payInForPayPal: Route = pathPrefix(PayPalRoute) {
    pathPrefix(Segment) { orderUuid =>
      parameters("transactionId", "printReceipt".as[Boolean]) { (transactionId, printReceipt) =>
        run(PayInForPayPal(orderUuid, transactionId, printReceipt)) completeWith {
          case r: PaidIn =>
            complete(
              HttpResponse(
                StatusCodes.OK,
                entity = r
              )
            )
          case r: PaymentRedirection =>
            complete(
              HttpResponse(
                StatusCodes.Accepted,
                entity = r
              )
            )
          case other => error(other)
        }
      }
    }
  }

  lazy val preAuthorizeCardFor3ds: Route = pathPrefix(SecureModeRoute / PreAuthorizeCardRoute) {
    pathPrefix(Segment) { orderUuid =>
      parameters("preAuthorizationId", "registerCard".as[Boolean], "printReceipt".as[Boolean]) {
        (preAuthorizationId, registerCard, printReceipt) =>
          run(
            PreAuthorizeCardFor3DS(orderUuid, preAuthorizationId, registerCard, printReceipt)
          ) completeWith {
            case _: CardPreAuthorized =>
              complete(
                HttpResponse(
                  StatusCodes.OK
                )
              )
            case r: PaymentRedirection =>
              complete(
                HttpResponse(
                  StatusCodes.Accepted,
                  entity = r
                )
              )
            case other => error(other)
          }
      }
    }
  }

  lazy val payInFirstRecurringFor3ds: Route = pathPrefix(SecureModeRoute / RecurringPaymentRoute) {
    pathPrefix(Segment) { recurringPayInRegistrationId =>
      parameter("transactionId") { transactionId =>
        run(PayInFirstRecurringFor3DS(recurringPayInRegistrationId, transactionId)) completeWith {
          case r: PaidIn =>
            complete(
              HttpResponse(
                StatusCodes.OK,
                entity = r
              )
            )
          case r: PaymentRedirection =>
            complete(
              HttpResponse(
                StatusCodes.Accepted,
                entity = r
              )
            )
          case other => error(other)
        }
      }
    }
  }

  lazy val hooks: Route = pathPrefix(HooksRoute) {
    pathEnd {
      complete(HttpResponse(StatusCodes.NotImplemented))
    }
  }

  lazy val bank: Route = pathPrefix(BankRoute) {
    // check anti CSRF token
    hmacTokenCsrfProtection(checkHeader) {
      // check if a session exists
      requiredClientSession { (client, session) =>
        pathEnd {
          get {
            run(
              LoadBankAccount(
                externalUuidWithProfile(session),
                clientId = client.map(_.clientId).orElse(session.clientId)
              )
            ) completeWith {
              case r: BankAccountLoaded =>
                complete(
                  HttpResponse(
                    StatusCodes.OK,
                    entity = r.bankAccount.view
                  )
                )
              case other => error(other)
            }
          } ~
          post {
            entity(as[BankAccountCommand]) { bank =>
              import bank._
              var externalUuid: String = ""
              val updatedUser: Option[PaymentAccount.User] =
                user match {
                  case Left(naturalUser) =>
                    var updatedNaturalUser = {
                      if (naturalUser.externalUuid.trim.isEmpty) {
                        naturalUser.withExternalUuid(session.id)
                      } else {
                        naturalUser
                      }
                    }
                    session.profile match {
                      case Some(profile) if updatedNaturalUser.profile.isEmpty =>
                        updatedNaturalUser = updatedNaturalUser.withProfile(profile)
                      case _ =>
                    }
                    externalUuid = updatedNaturalUser.externalUuid
                    Some(PaymentAccount.User.NaturalUser(updatedNaturalUser))
                  case Right(legalUser) =>
                    var updatedLegalRepresentative = legalUser.legalRepresentative
                    if (updatedLegalRepresentative.externalUuid.trim.isEmpty) {
                      updatedLegalRepresentative =
                        updatedLegalRepresentative.withExternalUuid(session.id)
                    }
                    session.profile match {
                      case Some(profile) if updatedLegalRepresentative.profile.isEmpty =>
                        updatedLegalRepresentative = updatedLegalRepresentative.withProfile(profile)
                      case _ =>
                    }
                    externalUuid = updatedLegalRepresentative.externalUuid
                    Some(
                      PaymentAccount.User.LegalUser(
                        legalUser.withLegalRepresentative(updatedLegalRepresentative)
                      )
                    )
                }
              run(
                CreateOrUpdateBankAccount(
                  externalUuidWithProfile(session),
                  bankAccount.withExternalUuid(externalUuid),
                  updatedUser,
                  acceptedTermsOfPSP,
                  client.map(_.clientId).orElse(session.clientId)
                )
              ) completeWith {
                case r: BankAccountCreatedOrUpdated =>
                  complete(HttpResponse(StatusCodes.OK, entity = r))
                case other => error(other)
              }
            }
          } ~
          delete {
            run(DeleteBankAccount(externalUuidWithProfile(session), Some(false))) completeWith {
              case _: BankAccountDeleted.type => complete(HttpResponse(StatusCodes.OK))
              case other                      => error(other)
            }
          }
        }
      }
    }
  }

  lazy val declaration: Route = pathPrefix(DeclarationRoute) {
    // check anti CSRF token
    hmacTokenCsrfProtection(checkHeader) {
      // check if a session exists
      requiredClientSession { (_, session) =>
        pathEnd {
          get {
            run(GetUboDeclaration(externalUuidWithProfile(session))) completeWith {
              case r: UboDeclarationLoaded =>
                complete(HttpResponse(StatusCodes.OK, entity = r.declaration.view))
              case other => error(other)
            }
          } ~ post {
            entity(as[UboDeclaration.UltimateBeneficialOwner]) { ubo =>
              run(CreateOrUpdateUbo(externalUuidWithProfile(session), ubo)) completeWith {
                case r: UboCreatedOrUpdated =>
                  complete(HttpResponse(StatusCodes.OK, entity = r.ubo))
                case other => error(other)
              }
            }
          } ~ put {
            run(ValidateUboDeclaration(externalUuidWithProfile(session))) completeWith {
              case _: UboDeclarationAskedForValidation.type =>
                complete(HttpResponse(StatusCodes.OK))
              case other => error(other)
            }
          }
        }
      }
    }
  }

  lazy val kyc: Route = {
    pathPrefix(KycRoute) {
      parameter("documentType") { documentType =>
        val maybeKycDocumentType: Option[KycDocument.KycDocumentType] =
          KycDocument.KycDocumentType.enumCompanion.fromName(documentType)
        maybeKycDocumentType match {
          case None =>
            complete(HttpResponse(StatusCodes.BadRequest))
          case Some(kycDocumentType) =>
            // check anti CSRF token
            hmacTokenCsrfProtection(checkHeader) {
              // check if a session exists
              requiredClientSession { (_, session) =>
                pathEnd {
                  get {
                    run(
                      LoadKycDocumentStatus(externalUuidWithProfile(session), kycDocumentType)
                    ) completeWith {
                      case r: KycDocumentStatusLoaded =>
                        complete(HttpResponse(StatusCodes.OK, entity = r.report))
                      case other => error(other)
                    }
                  } ~
                  post {
                    extractRequestContext { ctx =>
                      implicit val materializer: Materializer = ctx.materializer
                      fileUploadAll("pages") {
                        case files: Seq[(FileInfo, Source[ByteString, Any])] =>
                          val pages = for (file <- files) yield {
                            val bos = new ByteArrayOutputStream()
                            val future = file._2
                              .map { s =>
                                bos.write(s.toArray)
                              }
                              .runWith(Sink.ignore)
                            Await.result(future, CommonSettings.DefaultTimeout) // FIXME
                            val bytes = bos.toByteArray
                            bos.close()
                            bytes
                          }
                          run(
                            AddKycDocument(externalUuidWithProfile(session), pages, kycDocumentType)
                          ) completeWith {
                            case r: KycDocumentAdded =>
                              complete(HttpResponse(StatusCodes.OK, entity = r))
                            case other => error(other)
                          }
                        case _ => complete(HttpResponse(StatusCodes.BadRequest))
                      }
                    }
                  }
                }
              }
            }
        }
      }
    }
  }

  lazy val mandate: Route =
    pathPrefix(MandateRoute) {
      parameter("MandateId") { mandateId =>
        run(UpdateMandateStatus(mandateId)) completeWith {
          case r: MandateStatusUpdated => complete(HttpResponse(StatusCodes.OK, entity = r.result))
          case other                   => error(other)
        }
      } ~
      // check anti CSRF token
      hmacTokenCsrfProtection(checkHeader) {
        // check if a session exists
        requiredClientSession { (client, session) =>
          post {
            run(CreateMandate(externalUuidWithProfile(session))) completeWith {
              case r: MandateConfirmationRequired =>
                complete(HttpResponse(StatusCodes.OK, entity = r))
              case MandateCreated => complete(HttpResponse(StatusCodes.OK))
              case other          => error(other)
            }
          } ~
          delete {
            run(
              CancelMandate(
                externalUuidWithProfile(session),
                clientId = client.map(_.clientId).orElse(session.clientId)
              )
            ) completeWith {
              case MandateCanceled => complete(HttpResponse(StatusCodes.OK))
              case other           => error(other)
            }
          }
        }
      }
    }

  lazy val recurringPayment: Route = pathPrefix(RecurringPaymentRoute) {
    // check anti CSRF token
    hmacTokenCsrfProtection(checkHeader) {
      // check if a session exists
      requiredClientSession { (client, session) =>
        get {
          pathPrefix(Segment) { recurringPaymentRegistrationId =>
            run(
              LoadRecurringPayment(externalUuidWithProfile(session), recurringPaymentRegistrationId)
            ) completeWith {
              case r: RecurringPaymentLoaded =>
                complete(HttpResponse(StatusCodes.OK, entity = r.recurringPayment.view))
              case other => error(other)
            }
          }
        } ~ post {
          entity(as[RegisterRecurringPayment]) { cmd =>
            run(
              cmd.copy(
                debitedAccount = externalUuidWithProfile(session),
                clientId = client.map(_.clientId).orElse(session.clientId)
              )
            ) completeWith {
              case r: RecurringPaymentRegistered =>
                complete(HttpResponse(StatusCodes.OK, entity = r))
              case r: MandateConfirmationRequired =>
                complete(HttpResponse(StatusCodes.OK, entity = r))
              case other => error(other)
            }
          }
        } ~ put {
          entity(as[UpdateRecurringCardPaymentRegistration]) { cmd =>
            run(cmd.copy(debitedAccount = externalUuidWithProfile(session))) completeWith {
              case r: RecurringCardPaymentRegistrationUpdated =>
                complete(HttpResponse(StatusCodes.OK, entity = r.result))
              case other => error(other)
            }
          }
        } ~ delete {
          pathPrefix(Segment) { recurringPaymentRegistrationId =>
            run(
              UpdateRecurringCardPaymentRegistration(
                externalUuidWithProfile(session),
                recurringPaymentRegistrationId,
                None,
                Some(RecurringPayment.RecurringCardPaymentStatus.ENDED)
              )
            ) completeWith {
              case r: RecurringCardPaymentRegistrationUpdated =>
                complete(HttpResponse(StatusCodes.OK, entity = r.result))
              case other => error(other)
            }
          }
        }
      }
    }
  }

}
