#!/usr/bin/env bash
# Directory of this script
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

cd "${DIR}"/../../out
md5sum --check hash.md5
