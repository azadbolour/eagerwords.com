#!/bin/sh

# TODO. URGENT. Tests pass individually, but there is a failure when run together.

testregex=$1

if [ -z $testregex ]; then 
  testregex="*"
fi

export API_TYPE="client"
npm test -t src/__integration.tests__/$testregex
