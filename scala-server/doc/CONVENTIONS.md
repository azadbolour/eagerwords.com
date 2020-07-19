
## JSON

Note. Haskell server development is currently in abeyance. 

In Haskell's Servant http server, unit () is encoded
in JSON as []. That is, unit is considered an empty tuple. And since
tuples are encoded in arrays, so is unit. We will follow this standard
in the Scala server as well, as the specification of the JSON API
should be independent of the server implementation (but we don't 
have a choice in the case of Haskell's Servant).

## Server vs Common 

Ideally code that is common to both the server and a scala (or jvm) client would
reside in a separate project so that it gets its own jar file that can be used
as a dependency in client.

But it is not yet a requirement of this project to support JVM clients.
The supported clients are Javascript clients using JSON to interact 
with the API.

So while common code is separated out in its own common packages, for initial
simplicity of the build process, those packages are not housed in separate
projects. They remain in corresponding modules of the server side code.

TODO. At some point in the future, it would be nice to separate out the code
into different projects, so that the common scala classes can be compiled to
Javascript and used by Javascript clients. For the moment, however, the
Javascript UI code duplicates the common data structures separately in
Javascript.

## Utilities

General utilities are housed in the module scalautil. Currently this module is too small to 
deserve its own project. At some point in the future, it should become a 
separate project with its own jar to be reused by other projects.

## Kernel

Server code that is generally useful for a variety of applications is isolated
in the _kernel_ module. Currently it is too small to deserve its own separate 
project. 

