#!/bin/sh

# 
# Check for package imports not imported through absolute lib path. Not allowed.
# Package imports must be imported via absolute paths, so packages can easily be
# externalized.
#
check_package_import () {
  package=$1
  grep -r "$package" * | grep import | awk '{print $1 " "  $NF}' | grep -v "lib/$package/index" | grep '\.js'
}

#
# Check for absolute imports of eagerwords packages in lib path. Not allowed.
# Supporting packages cannot depend on the main eagerwords code.
#
check_absolute_app_import () {
  grep -r import lib | awk '{print $1 " "  $NF}' | sed -e 's/'"'"'/"/g' | grep -v '"\.' | grep -E 'grid|game'
}

#
# Check for relative imports of eagerwords packages in lib path. Not allowed.
# Supporting packages cannot depend on the main eagerwords code.
#
check_relative_app_import () {
  grep -r import lib | awk '{print $1 " "  $NF}' | sed -e 's/'"'"'/"/g' | grep -E '"\.\./*/grid|"\.\./*/game'
}

check_package_import 'js-util'
check_package_import 'react-support'
check_package_import 'react-auth'

check_absolute_app_import
check_relative_app_import

