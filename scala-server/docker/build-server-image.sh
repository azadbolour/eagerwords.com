#!/bin/sh -x

#
# Build the server image for the board game.
#
# The application distribution packager puts the tarball for the eagerwords
# scala server in the well-known location PACKAGE_DIR (see below).
#
# Create a docker image exposing the scala server by using the tarball.
#

NAMESPACE=$1
TAG=$2
BASE_IMAGE=$3

PACKAGE_DIR=/opt/data/eagerwords/package/

if [ -z "${NAMESPACE}" -o -z "${TAG}" ]; then
  echo "usage: $0 docker-namespace docker-tag [base-image]"
  exit 1
fi

if [ -z "${BASE_IMAGE}" ]; then
  BASE_IMAGE="azadbolour/jvm:1.0"
fi

if [ ! -d "${PACKAGE_DIR}" ]; then
    echo "expected application distribution package directory ${PACKAGE_DIR} does not exist"
    exit 1
fi

TAR_BALL=${PACKAGE_DIR}/eagerwords.tar

if [ ! -f "${TAR_BALL}" ]; then
    echo "eagerwords distribution tarball not found at ${TAR_BALL}"
    exit 1
fi

set -e
set -u

cd ${PACKAGE_DIR}
tar xvf eagerwords.tar

REPOSITORY=eagerwords-server
DOCKERFILE=docker/Dockerfile.${REPOSITORY}

#
# Use --no-cache so that the latest source will be pulled.
# Otherwise docker just compares the pull request with a previously-built and cached layer's 
# command and since they are the same it will use the old cached layer.
#
docker build --no-cache --force-rm=true -f ${DOCKERFILE} \
  --build-arg BASE=${BASE_IMAGE} \
  -t ${NAMESPACE}/${REPOSITORY}:${TAG} .

