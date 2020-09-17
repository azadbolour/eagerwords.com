#!/bin/sh -x

# 
# Run the docker packager to build and package the application.
#
# We use the convention that external server data for the
# application is rooted on the host system at the well-known folder:
#
#       /opt/data/eagerwords
#
# And that this folder is mapped to a folder of the same absolute name in the 
# container file system. The mapping is done by using the -v option of the
# docker run command.
#

#
# This directory is mapped into the docker container for
# the output distribution archive.
#
# By default docker on MAC OS disallows this mapping.
# User docker UI preferences/file sharing to allow it.
#

PROJECT="eagerwords.com"

EAGERWORDS_DATA=/opt/data/${PROJCT}
PACKAGE_DIR=${EAGERWORDS_DATA}/package

while [ $# -gt 0 ]; do
    case "$1" in
        --tag) 
            shift && TAG="$1" || (echo "missing tag value"; exit 1) ;;
        *) 
            echo "$0: unknown option $1" && die ;;
    esac
    shift
done

if [ -z "${TAG}" ]; then
    echo "required parameter 'tag' [of the docker image to run] is missing"
    exit 1
fi

sudo mkdir -p ${PACKAGE_DIR}
sudo chmod 777 ${PACKAGE_DIR}
sudo rm -rf ${PACKAGE_DIR}/*

NAMESPACE=azadbolour
REPOSITORY=${PROJECT}.packager

# Remove existing distribution containers to avoid name clash.
../../remove-container.sh ${REPOSITORY} || true

#
# The following are used by integration tests. So they are required.
# But for now integration tests are not run in the build and test process.
# So the following values can be arbitrary for now.
# TODO. Run integration tests in CI builds.
# In that case, these values would be secrets.
# IMPORTANT. Make sure they go to a .gitignored version of this file.

export TESTING_EMAIL="nobody@nowhere.com"
export TESTING_TOKEN="123456"
CONTAINER_NAME="${REPOSITORY}.${TAG}"

nohup docker run --restart on-failure:5 --name ${CONTAINER_NAME} \
    --workdir="" \
    -e TESTING_EMAIL -e TESTING_TOKEN \
    -v ${EAGERWORDS_DATA}:${EAGERWORDS_DATA} \
    ${NAMESPACE}/${REPOSITORY}:${TAG} &
