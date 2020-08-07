
# Scala Implementation of the Eager Words Game Server

## Modules

### eagerwords-server

The top level module which implements the eagerwords game API. It depends on 
the following modules for its functions.

### kernel

This module implements basic functions needed by many applications, such as
storing and retrieving user data.

### plane

This module supports working with points on the 2-dimensional plane and with grids
of such points. It implements a data structure called BlackWhiteGrid, a grid 
of points on the plane that may be enabled or disabled, and if enabled may
or may not have an associated value of some type. This data structure is used 
to represent game boards.

### scalautil 

This module provides general utility functions that may be of use in a variety of 
applications.


## Production Considerations

- For secure encryption, the play application requires a secret key, as the 
  value of the configuration parameter `play.http.secret.key`. 

    http://playframework.com/documentation/latest/ApplicationSecret

  All runtime parameters including secrets are provided to the application 
  through environment variables used in application.conf. See below.

- The play application uses a lock file while it is up.
  After a crash, the lock file stays put and prevents a restart.
  In order to be able to easily remove the lock file, it is [by default]
  externalized to the host directory `/var/run/eagerwords`. The scripts
  create the directory of the pid file on the host system. But they expect
  to be permitted to do so.

## Environment Variables

Runtime configuration options defined in application.conf generally have
associated overriding environment variables. The following environment variables
are used in application.conf, and are passed to the docker container for the
server.

- `MOCK_EMAIL`: Mock emails to users - log the message only - do not send.

- `MAIL_SMTP_USER`: The sending user.

- `MAIL_SMTP_PASSWORD`: The sender's password.

- `MAIL_SMTP_HOST`: The host address of the mail server.

- `MAIL_SMTP_PORT`: The port number of the mail server.

- `MAIL_SMTP_STARTTLS_ENABLE`: Use TLS for mail (true/false).

- `MAIL_SMTP_AUTH`: Authorize email transmission (true/false).

- `DB_HOST`: The database server's host address.

- `DB_PORT`: The database server's port number.

- `DB_NAME`: The name of the eagerwords database. By convention this is
  'eagerwords' for the application, and `eagerwords_test` for tests.

- `DB_USER`: The database user.

- `DB_PASS`: The database password.

- `DB_TYPE`: The DBMS system type: the name of a configuration 
  block in application.conf specifying the access details for the given
  type of DBMS. Valid types are: _postgres_, _sqlite_, and _h2mem_.

- `ENCRYPTION_KEY`: For encrypting sennsitive fields in the database.

- `PLAY_SECRET`: The play http secret key.

## To Do

- Change logging calls to debug and set up a run-debug to
  log at debug level.

- Implement validations everywhere. 

## Technical Debt

- API versioning - just include in URL.

- Streamline and document seeding and migration. Use Liquibase-type migration.

- Standardize errors in the API itself - so that different server implementations
  would look the same to the client in case of errors. Include validation of requests.

- Parallel execution of tests. 

## Dev Issues

- The addition of the circe JSON library broke Intellij compiles. They fail with
  stack overflow even with extremely large stacks of several GB. Likely because
  circe uses macros that are not supported by Intellij. For now, the tradeoff is
  in favor of code simplicity by using circe over ability to compile within
  Intellij. If we end up being hampered by this issue in development, e.g., in
  debugging within Intellij, we may have to revert to more traditional JSON
  libraries.

## Improvements

- Load balancing.

- Monitoring crashes and restarting.


