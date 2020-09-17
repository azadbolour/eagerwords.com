#!/bin/sh -x

tag=$1
message=$2

git tag -a $tag -m "$message"
git push origin $tag
