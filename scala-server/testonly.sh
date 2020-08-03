#!/bin/sh

testClass=$1
fullClass=`find . -name "$testClass.scala" |  \
      sed                                     \
      -e "s/\.scala//"                        \
      -e s"/\//./g"                           \
      -e "s/\.\.test\.//"                     \
`
echo "testing $fullClass"

sbt "testOnly $fullClass -- -oF"
