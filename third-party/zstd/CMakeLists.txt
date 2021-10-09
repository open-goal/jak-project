# ################################################################
# Copyright (c) 2015-present, Yann Collet, Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under both the BSD-style license (found in the
# LICENSE file in the root directory of this source tree) and the GPLv2 (found
# in the COPYING file in the root directory of this source tree).
# ################################################################


option(ZSTD_BUILD_STATIC "BUILD STATIC LIBRARIES" ON)
include_directories(lib/ lib/common)

if(UNIX)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O2")
elseif(WIN32)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /O2")
endif()

file(GLOB CommonSources lib/common/*.c)
file(GLOB CompressSources lib/compress/*.c)
file(GLOB DecompressSources lib/decompress/*.c)

set(Sources
        ${CommonSources}
        ${CompressSources}
        ${DecompressSources})


add_library(libzstd_static STATIC ${Sources} ${Headers})
set_property(TARGET libzstd_static PROPERTY POSITION_INDEPENDENT_CODE ON)