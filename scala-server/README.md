
# Scala Implementation of the EagerWords Game Server

## Modules

### eagerwords-server

The top level module which implements the eagerwords game API. It depends on 
the following modules for its functions.

### kernel

This module implements basic functions needed by many applications, such as
storing and retrieving user data.

### grid

This module supports working with points on the 2-dimensional grid.  It
implements a data structure called BlackWhiteGrid, a grid of points that may be
enabled or disabled, and if enabled may or may not have an associated value of
some type. This data structure is used to represent game boards.

### scalautil 

This module provides general utility functions that may be of use in a variety of 
applications.

## Testing Considerations

The transmission and receipt of authentication tokens via email complicates
tests involving user registration and login. 

For manual testing, a configuration parameter `MOCK_EMAIL` is provided that instructs 
the server not send emails, but instead to log their content. The manual tester
can then direct the log to stdout and copy and paste the logged authentication
tokens to validate signup and login.

For automated testing, two configuration variables, `TESTING_EMAIL`, and 
`TESTING_TOKEN` are provided. A test that uses `TESTING_EMAIL` as its user's
identifying email address would cause the server not to send an authentication
token through email, and accept `TESTING_TOKEN` as the legitimate authentication
token that would have been sent to the user. Of course, tests would then 
have to be made aware of the values of these configuration parameters. 
These settings are ignored if `MOCK_EMAIL` is false.

The server-side code for recognizing `TESTING_EMAIL` and `TESTING_TOKEN` has yet to be 
implemented. 

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

- `ENCRYPTION_KEY`: For encrypting sensitive fields in the database.

- `PLAY_SECRET`: The play http secret key.

- `OPTIONAL_ORIGIN`: Add an optional origin to allow testing of remote server 
  from a UI served on your local machine.
  
- `TESTING_EMAIL`: The special email address used in automated tests to
  forgo the authentication email.
  
- `TESTING_TOKEN`: The authentication token recognized as that which would 
  have been sent to the user's email for verification of signin and login.
  An automated test that uses `TESTING_EMAIL` for user identification can
  use `TESTING_TOKEN` in confirming of its authenticity to the server. 

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


