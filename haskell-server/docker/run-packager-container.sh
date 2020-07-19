#!/bin/sh -x

#
# Run the docker packager to build and package the application.
#
# We use the convention that external server data for the
# application is rooted on the host system at the well-known folder:
#
#       /opt/data/eagerwords
#
# And that this folder is mapped to a folder of the same absolute name in the 
# container file system. The mapping is done by using the -v option of the
# docker run command.
#

# TODO. Try to redirect output of the container to a mounted volume in the container.
# Then can use docker run -d to create a detached container.
# And the logs will be on the host file system.


EAGERWORDS_DATA=/opt/data/eagerwords
PACKAGE_DIR=${EAGERWORDS_DATA}/haskell/package

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

#
# For now be overly defensive.
#
if [ -e "$PACKAGE_DIR" -a ! -z "`ls -1 $PACKAGE_DIR`" ]; then
    echo "package directory $PACKAGE_DIR non-empty - will not be clobbered"
    exit 1
fi

#
# Create the mapped volume directory on the host.
#
sudo mkdir -p ${EAGERWORDS_DATA}
sudo chmod 777 ${EAGERWORDS_DATA}

NAMESPACE=azadbolour
REPOSITORY=boardgame-haskell-packager

# Remove existing distribution containers to avoid name clash.
../../remove-container.sh ${REPOSITORY} || true

nohup docker run --restart on-failure:5 --name ${REPOSITORY} \
    --workdir="" \
    -v ${EAGERWORDS_DATA}:${EAGERWORDS_DATA} \
    ${NAMESPACE}/${REPOSITORY}:${TAG} &
