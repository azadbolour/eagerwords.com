#!/bin/sh -x

bucketName=$1
buildparent=$2

if [ -z "${buildparent}" ]; then
  buildparent="."
fi


# Recursively copy changed files and delete obsolete files.
aws s3 sync ${buildparent}/build/ s3://${bucketName} --delete

