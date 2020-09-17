#!/bin/sh -x

# TODO. Conatiner name should include tag.

user=$1
repository=$2
tag=$3

if [ -z "$user" ]; then 
  echo "missing user - aborting - usage: $0 user repository tag"
  exit 1
fi

if [ -z "$repository" ]; then 
  echo "missing repositoty - aborting - usage: $0 user repository tag"
  exit 1
fi

if [ -z "$tag" ]; then 
  echo "missing tag - aborting - usage: $0 user repository tag"
  exit 1
fi

#
# For now assuming well-known names.
#
packager_image_name="${repository}.packager"
server_image_name="${repository}.server"
packager_container_name="${repository}.packager.$tag"
server_container_name="${repository}.server.$tag"

docker stop $packager_container_name 
docker rm $packager_container_name
docker rmi $packager_image_name

docker stop $server_container_name
docker rm $server_container_name
docker rmi $server_image_name

set -e
set -u

build-packager-image.sh $repository $tag 
run-packager-conatiner.sh --tag $tag
docker wait $packager_container_name

# TODO. Prints the exit code. Should check that.

build-server-image.sh $repository $tag 

(docker images | grep $server_image_name) || true




