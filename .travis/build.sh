#!/bin/bash
set -e -o pipefail
CSV=csv9.5.2
echo 'travis_fold:start:ChezScheme'
echo Installing Chez Scheme...
echo Downloading $CSV
curl -Ls https://github.com/cisco/ChezScheme/releases/download/v9.5.2/csv9.5.2.tar.gz | tar -xzf -
echo Building $CSV
(cd $CSV; ./configure -m=$TARGET_MACHINE; make)
echo 'travis_fold:end:ChezScheme'
CSROOT=$PWD/$CSV/$TARGET_MACHINE
export SCHEMEHEAPDIRS=$CSROOT/boot/$TARGET_MACHINE
echo 'travis_fold:start:Swish'
echo Building Swish...
./configure --scheme=$CSROOT/bin/$TARGET_MACHINE/scheme
make
echo 'travis_fold:end:Swish'
echo Testing Swish...
make test
