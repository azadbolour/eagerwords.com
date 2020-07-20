#!/bin/sh

# Serve the optimized build from the build directory.

# npm install -g serve

serve=$HOME/software/npm/packages/bin/serve
$serve -s build
