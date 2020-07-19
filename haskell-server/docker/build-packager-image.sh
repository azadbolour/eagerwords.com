#!/bin/sh

#
# Build the packager image for the eagerwords haskell game server.
#
# Expected to be run from the docker directory.
#
# The docker file will copy the eagerwords clone into the docker image,
# so it can be built and packaged by sbt.
#

namespace=$1
tag=$2

if [ -z "${namespace}" -o -z "${tag}" ]; then
  echo "usage: $0 docker-namespace repository-tag"
  exit 1
fi

set -e
set -u

#
# Use a well-known location for cloning the eagerwords project to use with docker.
#
EAGERWORDS_DATA=/opt/data/eagerwords
dockerDir=${EAGERWORDS_DATA}/docker
cloneDir=${dockerDir}/clone

# Clear out the existing clone.
sudo rm -rf ${cloneDir}/*

#
# Clone the eagerwords project.
# The repository is cloned externally to the docker file to avoid having to pass
# github secrets to docker.
#
../../clone-tag.sh ${cloneDir} ${tag}

repository=eagerwords-haskell-packager
dockerfile=Dockerfile.${repository}

docker build --no-cache --force-rm=true -f ${dockerfile} -t ${namespace}/${repository}:${tag} .
