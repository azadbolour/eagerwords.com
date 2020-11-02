#!/bin/sh

# Note. In sbt go to project and testOnly followed by tab to get list of tests.

project=$1
testClass=$2

dir=`pwd`
projectDir=$project
if [ "$project" == "scala-server" ]; then
  projectDir="."
fi

cd "$projectDir/src/test/scala"

class=`find . -name "$testClass.scala" `
echo $class
fullClass=`find . -name "$testClass.scala" |  \
      sed                                     \
      -e "s/^\.\///"                          \
      -e "s/\.scala//"                        \
      -e s"/\//./g"                           \
`
echo "testing $fullClass"

cd $dir

. local.exports.sh
. ./env.dev.sh


sbt <<EOF
project $project
testOnly $fullClass -- -oF
exit
EOF
