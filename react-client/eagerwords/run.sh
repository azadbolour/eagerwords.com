#!/bin/sh

# Serve the optimized build from the build directory.

buildparent=$1

if [ -z "${buildparent}" ]; then
  buildparent="."
fi

# npm install -g serve
NPM_PACKAGES=$HOME/software/npm/packages      # This is where serve gets installed.
#
# TODO. There must be a command that provides NPM_PACKAGES. find out and use it.
# On my MAC this is not the same area as where npm and node are installed. 
# May be due to change of installation procedure for node over time.
#

#
# NOTE. Serve will not serve public/xxxx.html correctly - it removes the .html 
# and xxxx cannot be interpreted by our app. The suggested workaround of adding 
# a serve.json file to the public directory with the following json did
# not work for me (as well as for others):
#
# {
#   "cleanUrls": false
# }
#
# Don't be surprised if Rules and Help don't show anything with serve.
#
serve=${NPM_PACKAGES}/bin/serve

$serve -s ${buildparent}/build
