#!/bin/sh

configPath=$1

if [ -z "$configPath" ]; then
  configPath="./config.yml"
fi

if [ -f ${configPath} ]; then 
  stack exec eagerwords-seed-data-populator $configPath
else
  stack exec eagerwords-seed-data-populator
fi

