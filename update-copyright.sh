#!/bin/sh

find . -name \*.scala -exec sed -i '' -e 's?azadbolour/eagerwords/blob/?azadbolour/eagerwords.com/blob/?' {} \;
