#!/bin/sh -x

tag=$1
message=$2

git tag -d $tag
git push origin ":refs/tags/${tag}"
git tag -a $tag -m "$message"
git push origin $tag
