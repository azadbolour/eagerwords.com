#!/bin/sh

cloneDir=$1
tag=$2

repo=eagerwords.com

# TODO. Check for tag as well.
if [ -z "${cloneDir}" -o -z "${tag}" ]; then
  echo "usage: $0 clone-parent tag"
  exit -1
fi

if [ -d "${cloneDir}/${repo}" ]; then
  echo "clone exists - will not clobber"
  exit 01
fi

sudo mkdir -p ${cloneDir}
sudo chmod 777 ${cloneDir}
cd ${cloneDir}

#
# Clone a given tag of the repo.
#
git clone --branch ${tag} --depth 1 https://github.com/azadbolour/${repo}

