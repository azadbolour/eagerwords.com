#!/bin/sh -x

# WORK IN PROGRESS.
#
# Install the game package to the default installation directory:
# /usr/local/eagerwords.
#
# Used to install the board game in docker container.
#

#
# Location of the distribution tar ball.
#
PACKAGE_DIR=/opt/data/eagerwords/package

errorout () {
  echo $1
  exit 1
}

test -d "$PACKAGE_DIR" || errorout "no distribution found at: ${PACKAGE_DIR}"

TAR_BALL=$PACKAGE_DIR/eagerwords-haskell.tar

if [ ! -f "${TAR_BALL}" ]; then
    echo "eagerwords distribution tarball not found at ${TAR_BALL}"
    exit 1
fi


SERVER=eagerwords-haskell
INSTALL=/usr/local/eagerwords

set -e
set -u

sudo mkdir -p $INSTALL
test -d "$INSTALL" || errorout "unable to create installation directory: ${INSTALL}"
sudo chmod 777 $INSTALL

cd $INSTALL
tar xvf $TAR_BALL




