#!/bin/bash
set -e -o pipefail
echo Testing Swish...
make test || {
    rc=$?
    echo 'travis_fold:start:failures'
    echo Mat output
    cat src/swish/*.mo || true
    echo 'travis_fold:end:failures'
    exit $rc
}
make coverage
