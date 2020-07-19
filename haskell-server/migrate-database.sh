#!/bin/sh

configPath=$1

if [ -z "$configPath" ]; then
  configPath="./config.yml"
fi

if [ -f ${configPath} ]; then 
  stack exec eagerwords-database-migrator $configPath
else
  stack exec eagerwords-database-migrator 
fi

