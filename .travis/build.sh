#!/bin/bash
set -e -o pipefail
CSROOT=$PWD/chez/$TARGET_MACHINE
export SCHEMEHEAPDIRS=$CSROOT/boot/$TARGET_MACHINE
echo 'travis_fold:start:Swish'
echo Building Swish...
./configure --scheme=$CSROOT/bin/$TARGET_MACHINE/scheme
make
echo 'travis_fold:end:Swish'
