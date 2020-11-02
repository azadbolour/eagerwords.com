#!/bin/sh

# TODO. URGENT. Tests pass individually, but there is a failure when run together.

# TODO. May need to upgrade related to upgrade of react-scripts and jest versions.

testregex=$1

if [ -z $testregex ]; then 
  testregex="*"
fi

export API_TYPE="client"
# npm test -t src/__integration.tests__/$testregex
npm test src/__integration.tests__/$testregex
