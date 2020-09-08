#!/bin/bash

# Directory of this script
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

export NEXT_DIR=$DIR
export FAKE_ISO_PATH=/game/fake_iso.txt
cd $DIR/build/test
make init
make gcov
make lcov