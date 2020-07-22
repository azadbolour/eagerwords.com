#!/bin/sh -x

#
# Install the game package to the default installation directory:
# /usr/local/eagerwords.
#
# Used to install the board game in docker container.
#

PROJECT="eagerwords.com"

#
# Root of the distribution. 
# Assumes the expanded distribution package has been copied here.
#
PACKAGE_DIR=/opt/data/${PROJECT}/package

errorout () {
  echo $1
  exit 1
}

test -d "$PACKAGE_DIR" || errorout "no distribution found at: ${PACKAGE_DIR}"

SERVER=eagerwords
INSTALL=/usr/local/${PROJECT}

set -e
set -u

cd $PACKAGE_DIR
VERSIONED_SERVER=`ls *.zip | sed -e "s/\.zip$//"`     # Assumes jus one zippped package.

sudo mkdir -p $INSTALL
test -d "$INSTALL" || errorout "unable to create installation directory: ${INSTALL}"
sudo chmod 777 $INSTALL

ZIPPED_BUNDLE=$PACKAGE_DIR/${VERSIONED_SERVER}.zip

cd $INSTALL
rm -rf ${VERSIONED_SERVER}/
rm -f $SERVER
unzip $ZIPPED_BUNDLE

# 
# Set up a well-known link so users become independent of the version.
#
ln -s ${VERSIONED_SERVER} $SERVER


