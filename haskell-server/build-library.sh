#!/bin/sh

#
# Sanity check the environment and the source tree.
#
. ../prepare.sh

#
# Reduce the  .cabal file to include just the library.
#
save=eagerwords.cabal.save
endLibraryLine=`grep -n 'START' eagerwords.cabal | cut -d : -f 1`
mv eagerwords.cabal $save
head "-${endLibraryLine}" $save > eagerwords.cabal
stack build
mv $save eagerwords.cabal
