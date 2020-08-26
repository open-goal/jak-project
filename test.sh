#!/bin/bash

# Directory of this script
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

export NEXT_DIR=$DIR
$DIR/build/test/goalc-test --gtest_color=yes --gtest_brief=1 "$@"