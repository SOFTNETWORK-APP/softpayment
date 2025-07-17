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
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.{jackson, Formats}
import org.json4s.jackson.Serialization
import app.softnetwork.api.server._
import app.softnetwork.payment.config.PaymentSettings.PaymentConfig._
import app.softnetwork.payment.model._
import app.softnetwork.payment.spi.PaymentProviders
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import com.softwaremill.session.SessionConfig

import java.io.ByteArrayOutputStream
import scala.concurrent.Await
import scala.language.implicitConversions

trait PaymentService[SD <: SessionData with SessionDataDecorator[SD]]
    extends Directives
    with DefaultComplete
    with Json4sSupport
    with BasicPaymentService
    with ServiceWithSessionDirectives[PaymentCommand, PaymentResult, SD]
    with ClientSessionDirectives[SD]
    with ApiRoute { _: PaymentHandler with SessionMaterials[SD] =>

  implicit def serialization: Serialization.type = jackson.Serialization

  implicit def formats: Formats = paymentFormats

  implicit def sessionConfig: SessionConfig

  override implicit def ts: ActorSystem[_] = system

  val route: Route = {
    pathPrefix(PaymentSettings.PaymentConfig.path) {
      hooks ~
      card ~
      paymentMethod ~
      payInCallback ~
      preAuthorizeCardCallback ~
      firstRecurringPaymentCallback ~
      bank ~
      declaration ~
      kyc ~
      mandate ~
      recurringPayment ~
      payment
    }
  }

  lazy val card: Route = pathPrefix(cardRoute) {
    // check anti CSRF token
    hmacTokenCsrfProtection(checkHeader) {
      // check if a session exists
      requiredClientSession { (client, session) =>
        pathEnd {
          get {
            run(LoadPaymentMethods(externalUuidWithProfile(session))) completeWith {
              case r: PaymentMethodsLoaded =>
                complete(
                  HttpResponse(
                    StatusCodes.OK,
                    entity = PaymentMethodsView(r.paymentMethods).cards
                  )
                )
              case other => error(other)
            }
          } ~
          post {
            entity(as[PreRegisterPaymentMethod]) { cmd =>
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
                  paymentType = Transaction.PaymentType.CARD,
                  clientId = client.map(_.clientId).orElse(session.clientId)
                )
              ) completeWith {
                case r: PaymentMethodPreRegistered =>
                  complete(
                    HttpResponse(
                      StatusCodes.OK,
                      entity = r.preRegistration
                    )
                  )
                case other => error(other)
              }
            }
          } ~
          delete {
            parameter("cardId") { cardId =>
              run(DisablePaymentMethod(externalUuidWithProfile(session), cardId)) completeWith {
                case _: PaymentMethodDisabled.type => complete(HttpResponse(StatusCodes.OK))
                case other                         => error(other)
              }
            }
          }
        }
      }
    }
  }

  lazy val paymentMethod: Route = pathPrefix(paymentMethodRoute) {
    // check anti CSRF token
    hmacTokenCsrfProtection(checkHeader) {
      // check if a session exists
      requiredClientSession { (client, session) =>
        pathEnd {
          get {
            run(LoadPaymentMethods(externalUuidWithProfile(session))) completeWith {
              case r: PaymentMethodsLoaded =>
                complete(
                  HttpResponse(
                    StatusCodes.OK,
                    entity = PaymentMethodsView(r.paymentMethods)
                  )
                )
              case other => error(other)
            }
          } ~
          post {
            entity(as[PreRegisterPaymentMethod]) { cmd =>
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
                case r: PaymentMethodPreRegistered =>
                  complete(
                    HttpResponse(
                      StatusCodes.OK,
                      entity = r.preRegistration
                    )
                  )
                case other => error(other)
              }
            }
          } ~
          delete {
            parameter("paymentMethodId") { paymentMethodId =>
              run(
                DisablePaymentMethod(externalUuidWithProfile(session), paymentMethodId)
              ) completeWith {
                case _: PaymentMethodDisabled.type => complete(HttpResponse(StatusCodes.OK))
                case other                         => error(other)
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
                    pathPrefix(preAuthorizeRoute) {
                      pathEnd {
                        run(
                          PreAuthorize(
                            orderUuid,
                            externalUuidWithProfile(session),
                            debitedAmount,
                            currency,
                            registrationId,
                            registrationData,
                            registerCard,
                            if (browserInfo.isDefined) Some(ipAddress) else None,
                            browserInfo,
                            printReceipt,
                            None,
                            feesAmount,
                            user,
                            paymentMethodId = paymentMethodId,
                            registerMeansOfPayment = registerMeansOfPayment
                          )
                        ) completeWith {
                          case r: PaymentPreAuthorized =>
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
                          case r: PaymentRequired =>
                            complete(
                              HttpResponse(
                                StatusCodes.PaymentRequired,
                                entity = r
                              )
                            )
                          case other => error(other)
                        }
                      } ~ pathSuffix(Segment) { creditedAccount =>
                        run(
                          PreAuthorize(
                            orderUuid,
                            externalUuidWithProfile(session),
                            debitedAmount,
                            currency,
                            registrationId,
                            registrationData,
                            registerCard,
                            if (browserInfo.isDefined) Some(ipAddress) else None,
                            browserInfo,
                            printReceipt,
                            Some(creditedAccount),
                            feesAmount,
                            user,
                            paymentMethodId = paymentMethodId,
                            registerMeansOfPayment = registerMeansOfPayment
                          )
                        ) completeWith {
                          case r: PaymentPreAuthorized =>
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
                          case r: PaymentRequired =>
                            complete(
                              HttpResponse(
                                StatusCodes.PaymentRequired,
                                entity = r
                              )
                            )
                          case other => error(other)
                        }
                      }
                    } ~ pathPrefix(payInRoute) {
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
                            printReceipt,
                            feesAmount = feesAmount,
                            user = user, // required for Pay in without registered card (eg PayPal)
                            registerMeansOfPayment = registerMeansOfPayment,
                            paymentMethodId = paymentMethodId,
                            clientId = client.map(_.clientId).orElse(session.clientId)
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
                          case r: PaymentRequired =>
                            complete(
                              HttpResponse(
                                StatusCodes.PaymentRequired,
                                entity = r
                              )
                            )
                          case other => error(other)
                        }
                      }
                    } ~ pathPrefix(recurringPaymentRoute) {
                      pathPrefix(Segment) { recurringPaymentRegistrationId =>
                        run(
                          ExecuteFirstRecurringPayment(
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

  lazy val payInCallback: Route = pathPrefix(callbacksRoute / payInRoute) {
    pathPrefix(Segment) { orderUuid =>
      parameterMap { params =>
        val transactionIdParameter =
          params.getOrElse("transactionIdParameter", "transactionId")
        parameters(
          transactionIdParameter,
          "registerMeansOfPayment".as[Boolean],
          "printReceipt".as[Boolean]
        ) { (transactionId, registerMeansOfPayment, printReceipt) =>
          run(
            PayInCallback(orderUuid, transactionId, registerMeansOfPayment, printReceipt)
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
            case r: PaymentRequired =>
              complete(
                HttpResponse(
                  StatusCodes.PaymentRequired,
                  entity = r
                )
              )
            case other => error(other)
          }
        }
      }
    }
  }

  lazy val preAuthorizeCardCallback: Route = pathPrefix(callbacksRoute / preAuthorizeRoute) {
    pathPrefix(Segment) { orderUuid =>
      parameterMap { params =>
        val preAuthorizationIdParameter =
          params.getOrElse("preAuthorizationIdParameter", "preAuthorizationId")
        parameters(
          preAuthorizationIdParameter,
          "registerMeansOfPayment".as[Boolean],
          "printReceipt".as[Boolean]
        ) { (preAuthorizationId, registerMeansOfPayment, printReceipt) =>
          run(
            PreAuthorizeCallback(
              orderUuid,
              preAuthorizationId,
              registerMeansOfPayment,
              printReceipt
            )
          ) completeWith {
            case _: PaymentPreAuthorized =>
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
  }

  lazy val firstRecurringPaymentCallback: Route =
    pathPrefix(callbacksRoute / recurringPaymentRoute) {
      pathPrefix(Segment) { recurringPayInRegistrationId =>
        parameter("transactionId") { transactionId =>
          run(
            FirstRecurringPaymentCallback(recurringPayInRegistrationId, transactionId)
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
      }
    }

  private lazy val hooksRoutes: List[Route] = PaymentProviders.hooksDirectives.map { case (k, v) =>
    log.info("registering hooks for provider: {}", k)
    pathPrefix(k) {
      v.hooks
    }
  }.toList

  lazy val hooks: Route = pathPrefix(hooksRoute) {
    concat(hooksRoutes: _*)
  }

  lazy val bank: Route = pathPrefix(bankRoute) {
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
            optionalHeaderValueByType[UserAgent]((): Unit) { userAgent =>
              extractClientIP { ipAddress =>
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
                            updatedLegalRepresentative =
                              updatedLegalRepresentative.withProfile(profile)
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
                      Some(ipAddress.value),
                      userAgent.map(_.name()),
                      tokenId = tokenId,
                      bankTokenId = bankTokenId
                    )
                  ) completeWith {
                    case r: BankAccountCreatedOrUpdated =>
                      complete(HttpResponse(StatusCodes.OK, entity = r))
                    case other => error(other)
                  }
                }
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

  lazy val declaration: Route = pathPrefix(declarationRoute) {
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
            optionalHeaderValueByType[UserAgent]((): Unit) { userAgent =>
              extractClientIP { ipAddress =>
                parameter("tokenId".?) { tokenId =>
                  run(
                    ValidateUboDeclaration(
                      externalUuidWithProfile(session),
                      ipAddress.value,
                      userAgent.map(_.name()),
                      tokenId
                    )
                  ) completeWith {
                    case _: UboDeclarationAskedForValidation.type =>
                      complete(HttpResponse(StatusCodes.OK))
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

  lazy val kyc: Route = {
    pathPrefix(kycRoute) {
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
    pathPrefix(mandateRoute) {
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
            entity(as[Option[IbanMandate]]) { maybeIban =>
              run(
                CreateMandate(
                  externalUuidWithProfile(session),
                  iban = maybeIban.map(_.iban),
                  clientId = client.map(_.clientId).orElse(session.clientId)
                )
              ) completeWith {
                case r: MandateConfirmationRequired =>
                  complete(HttpResponse(StatusCodes.OK, entity = r))
                case r: MandateRequired =>
                  complete(HttpResponse(StatusCodes.PaymentRequired, entity = r))
                case MandateCreated => complete(HttpResponse(StatusCodes.OK))
                case other          => error(other)
              }
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

  lazy val recurringPayment: Route = pathPrefix(recurringPaymentRoute) {
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
