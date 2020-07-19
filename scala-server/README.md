
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

  In a production environment, secrets will be securely stored in an
  external vault and securely communicated to the application. 
  The retrieval of secure parameters is abstracted out in the script
  `get-dynamic-params.sh`. The wrapper script, `run-server.sh`, used 
  to start the application gets the dynamic parameters through this
  script, and provides them to the server via system properties.

  The script `get-dynamic-parameters.sh` needs to be customized to 
  access the secure vault provided by each specific deployment 
  environment. This remains a manual step for now. TODO. Automate
  the provision of different implementations for for
  `get-dynamic-parameters.sh`.

- The play application uses a lock file while it is up.
  After a crash, the lock file stays put and prevents a restart.
  In order to be able to easily remove the lock file, it is [by default]
  externalized to the host directory `/var/run/eagerwords`. The scripts
  create the directory of the pid file on the host system. But they expect
  to be permitted to do so.

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


