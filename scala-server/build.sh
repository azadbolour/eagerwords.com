#!/bin/sh -x

#
# Sanity check the environment and the source tree.
#
. ../prepare.sh

# Get env variables needed by application.conf.
. local.exports.sh
. ./env.dev.sh

#
# Must 'test' for resources in the conf directory to be copied to the classpath.
#
sbt compile 'test'
