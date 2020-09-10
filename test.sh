#!/bin/bash

# Directory of this script
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

export NEXT_DIR=$DIR
export FAKE_ISO_PATH=/game/fake_iso.txt
$DIR/build/test/goalc-test --gtest_color=yes "$@"
