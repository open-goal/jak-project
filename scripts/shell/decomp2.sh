#!/usr/bin/env bash

# Directory of this script
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

"${DIR}"/../../build/decompiler/decompiler "${DIR}"/../../decompiler/config/jak2/jak2_config.jsonc "${DIR}"/../../iso_data/ "${DIR}"/../../decompiler_out --version ntsc_v1
