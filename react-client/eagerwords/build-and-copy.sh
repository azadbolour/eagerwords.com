#!/bin/sh -x

#
# Build production bundle and copy to a separate directory.
#

PROJECT=eagerwords.com
DEFAULT_PACKAGE_DIR=/opt/data/${PROJECT}/package

if [ -z "${PACKAGE_DIR}" ]; then
  PACKAGE_DIR=${DEFAULT_PACKAGE_DIR}
fi

set -u
set -e

./build.sh

out_dir=${PACKAGE_DIR}/react-client

mkdir -p ${out_dir}

cp -a build ${out_dir}

