#!/bin/sh

#
# Minimal production environment for testing.
# Run before starting the container in development machine for testing.
#

#
# Use sqlite for testing - internal to the container.
# To finesse need to network to the host for postgres.
#
export DB_TYPE='sqlite'
export ENCRYPTION_KEY='testing'
export PLAY_SECRET='testing'

#
# For now these are just dummy values.
# DO NOT CHECK IN THEIR REAL VALUES (if/when necessary for testing). 
#
export TESTING_EMAIL="testing@eagerwords.com"
export TESTING_TOKEN="testing"

