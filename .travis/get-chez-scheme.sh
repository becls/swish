#!/bin/bash
set -e -o pipefail
CSV=9.5.2
echo Downloading Chez Scheme Version $CSV
curl -Ls https://github.com/cisco/ChezScheme/releases/download/v$CSV/csv$CSV.tar.gz | tar -xzf -
mv csv$CSV chez
echo 'travis_fold:start:ChezScheme'
echo Building Chez Scheme Version $CSV
cd chez
./configure -m=$TARGET_MACHINE
make
echo 'travis_fold:end:ChezScheme'
