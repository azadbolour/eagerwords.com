#!/bin/sh -x

bucketName=$1
destination=$2

if [ -z "${destination}" ]; then
  destination="."
fi


aws s3 cp s3://${bucketName} ${destination} --recursive

