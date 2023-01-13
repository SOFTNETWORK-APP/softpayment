package app.softnetwork.payment.api

import app.softnetwork.persistence.jdbc.query.PostgresSchemaProvider

object MangoPayWithSchedulerPostgresLauncher
    extends MangoPayWithSchedulerApi
    with PostgresSchemaProvider
