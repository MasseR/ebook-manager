\(conf : ./config/Configuration.dhall)
->
''
flyway.locations=filesystem:${conf.database.migrations}/
flyway.url=jdbc:postgresql://${conf.database.host}/${conf.database.database}
flyway.user=${conf.database.username}
flyway.password=${conf.database.password}
''
