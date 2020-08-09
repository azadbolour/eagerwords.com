#!/bin/sh

#
# Sanity check the environment and the source tree.
#
. ../prepare.sh

#
# Get application.conf env variables.
#
. env.dev.sh

#
# Run scala server on given port.
#

sbt "run 6587"
