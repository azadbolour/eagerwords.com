#!/bin/sh

#
# Bring up Haskell server haddock documentaiton in chrome.
#

location="`pwd`/hdocs/html/eagerwords"
open -na /Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --args --kiosk ${location}/index.html
