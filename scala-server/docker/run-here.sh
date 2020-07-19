#!/bin/sh

HTTP_PORT=6587
ALLOWED_HOST="localhost:$HTTP_PORT"
# ALLOWED_HOST="host:port"
run-server-container.sh --tag 0.5.4 --http-port $HTTP_PORT --allowed-host "$ALLOWED_HOST"
