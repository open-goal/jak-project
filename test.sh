#!/usr/bin/env bash

# Directory of this script
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

"$DIR/build/goalc-test" --gtest_color=yes "$@" --gtest_filter="-*MANUAL_TEST*"
