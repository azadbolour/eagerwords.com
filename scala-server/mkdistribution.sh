#!/bin/sh -x

#
# Name of the directory to place the resulting distribution tarball.
# If not given use DEFAULT_PACKAGE_DIR (see below).
#
PACKAGE_DIR="$1"

#
# Make a distribution package for eagerwords.
#
# The distribution package consists of the distribution zip file for the
# game's play server, as well as scripts to install that distribution 
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
# This script is always run from the scala-server directory.
# It is up to the users of the script to cd there before calling this
# script.
#
# When called in a docker run, the docker image will include a clone of 
# the eagerwords source (at a particular tag) in a well-known location, and
# the dockerfile would cd to that location.
#

#
# Default directory of the resulting tarball. 
#
# When called in a docker run, this directory would be mapped to a directory 
# of the same name on the host, making the tarball available on the host.
#
PROJECT="eagerwords.com"

DEFAULT_PACKAGE_DIR=/opt/data/${PROJECT}/package

set -u
set -e

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

if [ "${dirName}" != "scala-server" ]; then
  echo "$0 must be run from the scala-server directory - working directory is $dirName"
  exit -1
fi

sudo mkdir -p $PACKAGE_DIR
# Can't chmod a mapped directory in docker. Ignore error.
sudo chmod 777 $PACKAGE_DIR || true

(cd ../dict && ./unzip-masked-words.sh)

#
# Build and package the play application.
#
sbt <<EOF
project scalautil
clean 
compile 
test 
project kernel
clean
compile
test
project plane
clean
compile
test
project scala-server
clean 
compile 
test 
dist
exit
EOF

#
# Copy the bundle to the default package directory.
#
# When building in a docker container, this well-known would be mapped to 
# the host file system so it can be accessed from the host and distributed 
# and used from there.
#

STAGING=/tmp/${PROJECT}/staging
sudo rm -rf $STAGING
sudo mkdir -p $STAGING
sudo chmod 777 $STAGING

# Copy the scala play distribution.
cp -a target/universal/*.zip $STAGING

#
# Copy the installation and startup scripts to the staging area.
#
SCRIPT_DIR=$STAGING/script
mkdir -p $SCRIPT_DIR
cp -a install.sh run-server.sh get-dynamic-params.sh $SCRIPT_DIR

#
# Copy the docker file needed to dockerize the server to the staging area.
#
DOCKER_DIR=$STAGING/docker
mkdir -p $DOCKER_DIR
cp -a docker/Dockerfile.${PROJECT}.server $DOCKER_DIR

TAR_BALL=$PACKAGE_DIR/${PROJECT}.tar
cd $STAGING
tar -cf $TAR_BALL ./*

sudo chmod 777 $TAR_BALL

