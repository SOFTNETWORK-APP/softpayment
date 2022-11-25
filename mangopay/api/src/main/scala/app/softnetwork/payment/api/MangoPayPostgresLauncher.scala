package app.softnetwork.payment.api

import app.softnetwork.persistence.jdbc.query.PostgresSchemaProvider

object MangoPayPostgresLauncher extends MangoPayApi with PostgresSchemaProvider
