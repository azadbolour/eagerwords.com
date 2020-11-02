#!/bin/sh -x

#
# Run the docker image for building the react UI package.
#

PROJECT=eagerwords.com

EAGERWORDS_DATA=/opt/data/${PROJECT}
PACKAGE_DIR=${EAGERWORDS_DATA}/package

while [ $# -gt 0 ]; do
    case "$1" in
        --tag)
            shift && TAG="$1" || (echo "missing tag value"; exit 1) ;;
        *)
            echo "$0: unknown option $1" && die ;;
    esac
    shift
done

if [ -z "${TAG}" ]; then
    echo "required parameter 'tag' [of the docker image to run] is missing"
    exit 1
fi

sudo mkdir -p ${PACKAGE_DIR}
sudo chmod 777 ${PACKAGE_DIR}
out_dir="${PACKAGE_DIR}/react-client"
sudo rm -rf ${out_dir}

NAMESPACE=azadbolour
REPOSITORY="${PROJECT}.react-builder"

../../../remove-container.sh ${REPOSITORY} || true

nohup docker run --restart on-failure:5 --name ${REPOSITORY} \
    --workdir="" \
    -v ${EAGERWORDS_DATA}:${EAGERWORDS_DATA} \
    ${NAMESPACE}/${REPOSITORY}:${TAG} &


