#!/bin/sh

PROJECT="eagerwords.com"
# /private needed for he MAC - as docker file sharing changes /var to /private/var!
EAGERWORDS_VAR=/private/var/run/${PROJECT} #
DEFAULT_PID_FILE=${EAGERWORDS_VAR}/play.pid

HTTP_PORT=6587
ALLOWED_HOST="localhost:$HTTP_PORT"
# ALLOWED_HOST="host:port"
run-server-container.sh --tag 0.9.2 --http-port $HTTP_PORT --allowed-host "$ALLOWED_HOST" \
  --pid-file ${DEFAULT_PID_FILE} \
  --config-file /opt/data/eagerwords.com/conf/prod.conf
