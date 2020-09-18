#!/bin/sh -x

#
# Build the server docker image from a specific git tag.
# The same tag is used for tagging the built docker image.
#
namespace=$1      # azadbolour
tag=$2            # 0.9.2

project="eagerwords.com"

if [ -z "$namespace" ]; then 
  echo "missing namespace - aborting - usage: $0 namespace tag"
  exit 1
fi

if [ -z "$tag" ]; then 
  echo "missing tag - aborting - usage: $0 namespace tag"
  exit 1
fi

#
# For now assuming well-known names.
#
packager_image_name="${project}.packager"
packager_container_name="${packager_image_name}.$tag"

server_image_name="${project}.server"
server_container_name="${server_image_name}.$tag"

#
# Clean up any existing containers or images.
#
docker stop $packager_container_name || true
docker rm $packager_container_name || true
docker rmi "$namespace/$packager_image_name:$tag" || true

docker stop $server_container_name || true
docker rm $server_container_name || true
docker rmi "$namespace/$server_image_name:$tag" || true

#
# Create a distribution package for the Scala Play application.
#
build-packager-image.sh $namespace $tag 
run-packager-container.sh --tag $tag
sleep 2         # Startup happens in the background - so wait a little.
docker wait $packager_container_name

# TODO. Prints the exit code. Should check that.

#
# Build the server docker image.
#
build-server-image.sh $namespace $tag 

(docker images | grep $server_image_name) || true




