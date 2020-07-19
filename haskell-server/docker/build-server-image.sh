#!/bin/sh -x

#
# Build the server image for the eagerwords server.
#
# The application distribution packager puts the tarball for the eagerwords
# scala server in the well-known location PACKAGE_DIR (see below).
#
# Create a docker image exposing the server by using the tarball.
#

NAMESPACE=$1
TAG=$2

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

REPOSITORY=eagerwords-haskell-server
DOCKERFILE=docker/Dockerfile.${REPOSITORY}

cd ${PACKAGE_DIR}
tar -xf ${TARBALL} ${DOCKERFILE}

docker build --no-cache --force-rm=true -f ${DOCKERFILE} -t ${NAMESPACE}/${REPOSITORY}:${TAG} .
