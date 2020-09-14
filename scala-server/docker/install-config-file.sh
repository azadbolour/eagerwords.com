#!/bin/sh -x

#
# Install prod.conf (from the current directory) to a well-known location.
#
# Create prod.conf for specific deployment and run this script to install it.
# Then make sure it is picked up in run-here.sh.
#
CONFIG_DIR=/opt/data/eagerwords.com/conf

sudo mkdir -p $CONFIG_DIR
sudo chmod 777 $CONFIG_DIR
cp -a prod.conf $CONFIG_DIR
