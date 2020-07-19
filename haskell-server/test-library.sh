#!/bin/sh

save=eagerwords.cabal.save
endLibraryLine=`grep -n 'START' eagerwords.cabal | cut -d : -f 1`
mv eagerwords.cabal $save
head "-${endLibraryLine}" $save > eagerwords.cabal
stack 'test'
mv $save eagerwords.cabal
