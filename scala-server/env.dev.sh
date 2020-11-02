#!/bin/sh

#
# Required server configuration parameters for running the development server.
#
export DB_TYPE=sqlite
export PLAY_SECRET=changeme
export ENCRYPTION_KEY=changemetoo

#
# These values remain sensitive for now - can be provided here or externally
# in a gitignored file by convention called local.exports.sh.
#
for VAR in TESTING_EMAIL TESTING_TOKEN; do 
  COMMAND="echo \$${VAR}"
  VALUE=`eval $COMMAND`
  if [ -z "${VALUE}" ]; then
    echo -n "$VAR: "; stty -echo; read VALUE; stty echo; echo ""
    export $VAR=$VALUE
  fi
done

