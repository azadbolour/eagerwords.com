#!/bin/sh

# Serve the optimized build from the build directory.

buildparent=$1

if [ -z "${buildparent}" ]; then
  buildparent="."
fi

# npm install -g serve
NPM_PACKAGES=$HOME/software/npm/packages      # This is where serve gets installed.
# TODO. There must be a command that provides NPM_PACKAGES. find out and use it.
# On my MAC this is not the same area as where npm and node are installed. 
# May be due to change of installation procedure for node over time.
serve=${NPM_PACKAGES}/bin/serve

$serve -s ${buildparent}/build
