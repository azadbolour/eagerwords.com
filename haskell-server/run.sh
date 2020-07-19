#!/bin/sh

confPath=$1

if [ -z "$confPath" ]; then
  confPath="./config.yml"
fi

command="stack exec eagerwords-server"

if [ -f ${confPath} ]; then 
  command="${command} ${confPath}"
fi

#
# Unzip the masked words file if necessary.
#
(cd ${WORKSPACE}/dict && ./unzip-masked-words.sh)

echo "${command}"

$command


