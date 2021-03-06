#!/bin/sh -x

# 
# Run a production docker container for the application.
# Parameters passed to the entry point of the container.
#
#   HTTP_PORT for the port of the server application
#   PROD_CONF for the Play production configuration file of the application
#   PID_FILE the location of play's pid lock file
#
# These variables are used to provide information about the production 
# deployment environment to the Play application. 
#

#
# Note. On MAC OS the default memory limit for a docker container
# is 2GB. That is tool low for running the server.
# The memory limit can be changed as a preference in the
# docker "whale" UI on the MAC. We are currently using 6GB.
# But we may be able to get away with 4GB.
#

# TODO. Try to redirect output of the container to a mounted volume in the container.
# Then can use docker run -d to create a detached container.
# And the logs will be on the host file system.

PROJECT="eagerwords.com"

errorout () {
  echo $1
  exit 1
}

#
# These defaults are repeated here and in the run-server script.
# Keeping them independent for now for simplicity.
#
DEFAULT_HTTP_PORT=6597
# For now we are only allowing one host.
# TODO. Allow multiple hosts for flexibility in testing.
DEFAULT_ALLOWED_HOST="127.0.0.1:${DEFAULT_HTTP_PORT}"
EAGERWORDS_VAR=/var/run/${PROJECT}
DEFAULT_PID_FILE=${EAGERWORDS_VAR}/play.pid
# By default, use application.conf not a special config file.
CONFIG_FILE=""

while [ $# -gt 0 ]; do
    case "$1" in
        --http-port) 
            shift && HTTP_PORT="$1" || (echo "missing http-port value"; exit 1) ;;
        --allowed-host) 
            shift && ALLOWED_HOST="$1" || (echo "missing allowed-host value"; exit 1) ;;
        --config-file)
            shift && CONFIG_FILE="$1" || (echo "missing config-file value"; exit 1) ;;
        --pid-file)
            shift && PID_FILE="$1" || (echo "missing pid-file value"; exit 1) ;;
        --tag)
            shift && TAG="$1" || (echo "missing tag value"; exit 1) ;;
        *) 
            echo "$0: unknown option $1" && die ;;
    esac
    shift
done

if [ -z "$TAG" ]; then 
    echo "required parameter 'tag' [of the docker image to run] is missing"
    exit 1
fi

if [ -z "$HTTP_PORT" ]; then HTTP_PORT=${DEFAULT_HTTP_PORT}; fi
if [ -z "$PID_FILE" ]; then PID_FILE=${DEFAULT_PID_FILE}; fi
if [ -z "$ALLOWED_HOST" ]; then ALLOWED_HOST=${DEFAULT_ALLOWED_HOST}; fi

#
# To make it easy to remove a stray pid [lock] file, we map its
# directory to an external directory on the host system.
# Create the directory if it does not exist, and map it in the
# docker run below.
#
PID_DIR=`dirname ${PID_FILE}`
sudo mkdir -p ${PID_DIR}
sudo chmod 777 ${PID_DIR}

mapConfig=""
envConfig=""

if [ -n "$CONFIG_FILE" ]; then
    test -e "${CONFIG_FILE}" || errorout "production conf file ${CONFIG_FILE} does not exist"
    CONFIG_DIR=`dirname ${CONFIG_FILE}`
    mapConfig="-v ${CONFIG_DIR}:${CONFIG_DIR}"
    envConfig="-e CONFIG_FILE"
fi

NAMESPACE=azadbolour
REPOSITORY=${PROJECT}.server
CONTAINER_NAME="${REPOSITORY}.${TAG}"

nohup docker run -p ${HTTP_PORT}:${HTTP_PORT} --restart on-failure:5 --name ${CONTAINER_NAME} \
    --workdir="" \
    -e HTTP_PORT="${HTTP_PORT}" -e ALLOWED_HOST="${ALLOWED_HOST}" -e PID_FILE="${PID_FILE}" \
    -e DB_HOST -e DB_PORT -e DB_NAME -e DB_USER -e DB_PASS -e DB_TYPE \
    -e MAIL_SMTP_USER -e MAIL_SMTP_PASSWORD -e MAIL_SMTP_HOST -e MAIL_SMTP_PORT \
    -e MOCK_EMAIL -e MAIL_SMTP_STARTTLS_ENABLE -e MAIL_SMTP_AUTH \
    -e ENCRYPTION_KEY -e PLAY_SECRET \
    -e TESTING_EMAIL -e TESTING_TOKEN ${envConfig} \
    -v ${PID_DIR}:${PID_DIR} \
    ${mapConfig} \
    ${NAMESPACE}/${REPOSITORY}:${TAG} &
