#!/bin/sh -x

#
# Build the Haskell server and copy the installation files to
# to a package directory given in the argument (default see below).
#

PACKAGE_DIR=$1

#
# Make a distribution package for the eagerwords Haskell server.
#
# The distribution package consists of the eagerwords executable and
# dictionary files, as well as scripts to install that distribution
# and to run it. The distribution package is archived in a tarball and
# the tar ball copied to a well-known location.
#
# This script is intended to be used as the entry point of a docker
# container for reproducibly creating distribution packages from eagerwords
# source code. Because parameterization of docker runs can get complicated,
# when run as the entry point from a docker container, this script is
# not given any parameters and uses well-known constants as default
# values of its parameters.
#
# But the parameters allow flexibility in testing this script by itself.
#
# This script is always run from the haskell-server directory.
# It is up to the users of the script to cd there before calling this
# script.
#
# When called in a docker run, the docker image will include a clone of
# the eagerwords source (at a particular tag) in a well-known location, and
# the dockerfile would cd to that location.
#

DEFAULT_PACKAGE_DIR=/opt/data/eagerwords/haskell/package

if [ -z "${PACKAGE_DIR}" ]; then
  PACKAGE_DIR=${DEFAULT_PACKAGE_DIR}
fi

if [ -f "${PACKAGE_DIR}" ]; then
  echo "PACKAGE_DIR $PACKAGE_DIR not a directory"
  exit -1
fi

if [ -d "${PACKAGE_DIR}" ]; then
  found=`find $PACKAGE_DIR -maxdepth 0 -empty`
  if [ ! -n  "$found" ]; then
    echo "PACKAGE_DIR $PACKAGE_DIR is not empty - will not clobber"
    exit -1
  fi
fi

dirName=`basename $(pwd)`

if [ "${dirName}" != "haskell-server" ]; then
  echo "$0 must be run from the haskell-server directory - working directory is $dirName"
  exit -1
fi

set -u
set -e

sudo mkdir -p $PACKAGE_DIR
# Can't chmod a mapped directory in docker. Ignore error.
sudo chmod 777 $PACKAGE_DIR || true

(cd ../dict && ./unzip-masked-words.sh)

stack build
(cd haskell-server/test-data \
  && cp sqlite-config.yml test-config.yml)
stack "test"

PROG=`stack exec -- which eagerwords-server`

#
# Copy the installation artifacts to the staging area.
#
STAGING=/tmp/eagerwords/staging
BIN=$STAGING/bin
sudo rm -rf $STAGING
sudo mkdir -p $STAGING
sudo chmod 777 $STAGING
sudo mkdir $BIN
sudo chmod 777 $BIN

cp $PROG $BIN
cp -aL dict $STAGING
# TODO. Should use a config directory.
cp "test-data/sqlite-config.yml" $BIN/config.yml

#
# Copy the docker file needed to dockerize the server to the staging area.
#
DOCKER_DIR=$STAGING/docker
mkdir -p $DOCKER_DIR
cp -a docker/Dockerfile.eagerwords-haskell-server $DOCKER_DIR

#
# Copy the installation and startup scripts to the staging area.
#
cp -a install.sh $BIN

TAR_BALL=$PACKAGE_DIR/eagerwords-haskell.tar
cd $STAGING
tar -cf $TAR_BALL ./*

sudo chmod 777 $TAR_BALL
