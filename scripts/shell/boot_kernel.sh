#!/usr/bin/env bash

# Directory of this script
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

"${DIR}"/../../build/game/gk -- -fakeiso -debug -nodisplay"$@"
