#!/bin/sh -x

#
# Build a builder image for the react UI.
#

namespace=$1
tag=$2
serverport=$3

# serverport is '' in production.
if [ -z "${namespace}" -o -z "${tag}" ]; then
  echo "usage: $0 docker-namespace repository-tag server-port"
  exit 1
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

repository="${PROJECT}.react-builder"
dockerfile=${cloneDir}/${PROJECT}/eagerwords-web/docker/Dockerfile.${repository}

# Go to the working directory of docker build - the parent of the clone.
cd ${cloneDir}

# Add a .env file to the UI source to provide the server url as an env variable.

dotenv=${PROJECT}/eagerwords-web/.env
cat <<EOF > ${dotenv}
REACT_APP_API=client
REACT_APP_SERVER_PORT=${serverport}
EOF

#
# Use --no-cache so that the latest source will be pulled.
# Otherwise docker just compares the pull request with a previously-built and cached layer's
# command and since they are the same it will use the old cached layer.
#
docker build --no-cache --force-rm=true -f ${dockerfile} \
    -t ${namespace}/${repository}:${tag} .
