package app.softnetwork.payment.launch

import app.softnetwork.api.server.launch.Application

trait PaymentApplication extends Application with PaymentRoutes
