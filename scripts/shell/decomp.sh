#!/usr/bin/env bash

# Directory of this script
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

"${DIR}"/../../build/decompiler/decompiler "${DIR}"/../../decompiler/config/jak1_ntsc_black_label.jsonc "${DIR}"/../../iso_data "${DIR}"/../../decompiler_out
