#!/bin/sh -x

#
# Build the packager image for the eagerwords scala game server.
#
# Expected to be run from the docker directory.
#
# The docker file will copy the eagerwords clone into the docker image,
# so it can be built and packaged by sbt.
#

namespace=$1
tag=$2
baseImage=$3

if [ -z "${namespace}" -o -z "${tag}" ]; then
  echo "usage: $0 docker-namespace repository-tag [base-image]"
  exit 1
fi

if [ -z "$baseImage" ]; then 
  baseImage="azadbolour/jvm:1.0"
fi

set -e
set -u

PROJECT="eagerwords.com"

#
# Use a well-known location for cloning the eagerwords project to use with docker.
#
EAGERWORDS_DATA=/opt/data/${PROJECT}
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

repository=${PROJECT}.packager
dockerfile=${cloneDir}/eagerwords/scala-server/docker/Dockerfile.${repository}

# Go to the working directory of docker build - the parent of the clone.
cd ${cloneDir}

#
# Use --no-cache so that the latest source will be pulled.
# Otherwise docker just compares the pull request with a previously-built and cached layer's 
# command and since they are the same it will use the old cached layer.
#
docker build --no-cache --force-rm=true -f ${dockerfile} \
    --build-arg BASE=${baseImage} \
    -t ${namespace}/${repository}:${tag} .

