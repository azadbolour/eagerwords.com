#!/bin/sh

# Generate docs.

# location="`pwd`/.stack-work/dist/x86_64-osx/Cabal-1.22.5.0/doc"

# stack haddock --haddock-arguments "   "
# Adding links:
# --source-base=URL , --source-module=URL , --source-entity=URL , --source-entity-line=URL



stack haddock # -v for verbose

haddockdest="${WORKSPACE}/haskell-server/.stack-work/install/x86_64-osx/lts-6.25/7.10.3/doc"
hdocs=${WORKSPACE}/haskell-server/docs/hdocs

# TODO. Where does the 0.0.0 come from? Is it a version number for the project?
rm -rf {$hdocs}
cp -a "${haddockdest}/eagerwords-0.0.0" ${hdocs}
