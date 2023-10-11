#! /bin/bash -e
TARGET="$1"
if [ -f failures ]; then
  echo "Skipping ${TARGET} due to earlier failure:"
  cat failures
else
  make "${TARGET}" || echo "Run ${TARGET} tests failed" >> failures
fi
