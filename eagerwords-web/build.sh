#!/bin/sh -x

#
# Build production version of React application.
#
# Output goes under the build folder.
#

# Make sure we start with a clean slate.
rm -rf build/*

npm run build


