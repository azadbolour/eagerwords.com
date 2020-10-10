#!/bin/sh

#
# Build the UI bundle reproducibly by using a specific git tag.
#
namespace=$1
tag=$2
serverport=$3

# serverport is '' for production.

if [ -z "$namespace" ]; then 
  echo "missing namespace - aborting - usage: $0 namespace tag serverport"
  exit 1
fi

if [ -z "$tag" ]; then 
  echo "missing tag - aborting - usage: $0 namespace tag serverport"
  exit 1
fi


project="eagerwords.com"
react_builder_image_name="${project}.react-builder"
react_builder_container_name="${react_builder_image_name}"

#
# Clean up the output directory.
# 
build_output="/opt/data/eagerwords.com/package/eagerwords-web/build"
rm -rf ${build_output}

#
# Clean up any existing container or image.
#
docker stop $react_builder_container_name || true
docker rm ${react_builder_container_name} || true
docker rmi "$namespace/$react_builder_image_name:$tag" || true

build-react-builder-image.sh ${namespace} ${tag} ${serverport}
run-react-builder-container.sh --tag ${tag}
sleep 2         # Startup happens in the background - so wait a little.
docker wait ${react_builder_container_name}

ls -lt ${build_output}

