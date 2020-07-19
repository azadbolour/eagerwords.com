#!/bin/sh

#
# Example GameService - NOT GameServiceSpec.
#
testBaseNameWithoutSpecHs=$1
stack test eagerwords:spec --ta "-m ${testBaseNameWithoutSpecHs}"
