#!/bin/bash
set -e -o pipefail
echo 'travis_fold:start:ChezScheme'
branch=${CHEZ_BRANCH:-v9.5.4}
echo Building Chez Scheme ${branch}
git clone --branch ${branch} --depth 1 --quiet https://github.com/cisco/ChezScheme.git
cd ChezScheme
./configure -m=$TARGET_MACHINE
make
echo 'travis_fold:end:ChezScheme'
