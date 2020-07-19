#!/bin/sh

name=$1

docker ps -a --filter "name=${name}" \
  | awk 'NR > 1 {print $1}' \
  | xargs docker rm

