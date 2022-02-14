# Copyright (c) 2016, Project Chrono Development Team
# All rights reserved.

# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

#  - Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. 
#  - Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
#  - Neither the name of the nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission. 

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# This script checks for the highest level of AVX support on the host
# by compiling and running small C++ programs that use AVX intrinsics.
#
# You can invoke this module using the following command:
#
#   FIND_PACKAGE(AVX [major[.minor]] [EXACT] [QUIET|REQUIRED])
#
# where the version string is one of:
#
#   1.0 for AVX support
#   2.0 for AVX2 support
#
# Note that any ".0" in the above version string is optional.
#
# If any AVX support is detected, the following variables are set:
#
#   AVX_FOUND   = 1
#   AVX_VERSION = the requested version, if EXACT is true, or
#                 the highest AVX version found.
#   AVX_FLAGS = compile flags for the version of AVX found
#
# If AVX is not supported on the host platform, these variables are
# not set. If QUIET is true, the module does not print a message if
# AVX if missing. If REQUIRED is true, the module produces a fatal
# error if AVX support is missing.

set(AVX_FLAGS)
set(AVX_FOUND)
set(DETECTED_AVX_10)
set(DETECTED_AVX_20)

if(CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX)
  execute_process(COMMAND ${CMAKE_CXX_COMPILER} "-dumpversion" OUTPUT_VARIABLE GCC_VERSION_STRING)
  if(GCC_VERSION_STRING VERSION_GREATER 4.2 AND NOT APPLE AND NOT CMAKE_CROSSCOMPILING)
    SET(AVX_FLAGS "${AVX_FLAGS} -march=native")
    message(STATUS "Using CPU native flags for AVX optimization: ${AVX_FLAGS}")
  endif()
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang" AND NOT CMAKE_CROSSCOMPILING)
    SET(AVX_FLAGS "${AVX_FLAGS} -march=native")
    message(STATUS "Using CPU native flags for AVX optimization: ${AVX_FLAGS}")
endif()

include(CheckCXXSourceRuns)
set(CMAKE_REQUIRED_FLAGS)


# Generate a list of AVX versions to test.
if(AVX_FIND_VERSION_EXACT)
  if(AVX_FIND_VERSION VERSION_EQUAL "2.0")
    set(_AVX_TEST_20 1)
  elseif(AVX_FIND_VERSION VERSION_EQUAL "1.0")
    set(_AVX_TEST_10 1)
  endif()
else()
  if(NOT AVX_FIND_VERSION VERSION_GREATER "2.0")
    set(_AVX_TEST_20 1)
  endif()
  if(NOT AVX_FIND_VERSION VERSION_GREATER "1.0")
    set(_AVX_TEST_10 1)
  endif()  
endif()

# Check for AVX2 support.
if(_AVX_TEST_20)
  if(CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    set(CMAKE_REQUIRED_FLAGS "-mavx2")
  elseif(CMAKE_CXX_COMPILER_ID MATCHES "Intel")
    set(CMAKE_REQUIRED_FLAGS "-xHost")
  elseif(MSVC AND NOT CMAKE_CL_64)
    set(CMAKE_REQUIRED_FLAGS "/arch:AVX2")
  endif()
  check_cxx_source_runs("
  #include <immintrin.h>
  int main()
    {
    __m256i a = _mm256_set_epi32 (-1, 2, -3, 4, -1, 2, -3, 4);
    __m256i result = _mm256_abs_epi32 (a);
    return 0;
    }" DETECTED_AVX_20)
endif()

# Check for AVX support.
if(_AVX_TEST_10)
  if(CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    set(CMAKE_REQUIRED_FLAGS "-mavx")
  elseif(CMAKE_CXX_COMPILER_ID MATCHES "Intel")
    set(CMAKE_REQUIRED_FLAGS "-xHost")
  elseif(MSVC AND NOT CMAKE_CL_64)
    set(CMAKE_REQUIRED_FLAGS "/arch:AVX")
  endif()
  check_cxx_source_runs("
  #include <immintrin.h>
  int main()
    {
    __m256 a = _mm256_set_ps (-1.0f, 2.0f, -3.0f, 4.0f, -1.0f, 2.0f, -3.0f, 4.0f);
    __m256 b = _mm256_set_ps (1.0f, 2.0f, 3.0f, 4.0f, 1.0f, 2.0f, 3.0f, 4.0f);
    __m256 result = _mm256_add_ps (a, b);
    return 0;
    }" DETECTED_AVX_10)
endif()

set(CMAKE_REQUIRED_FLAGS)


if(DETECTED_AVX_20)
    set(AVX_VERSION "2.0")
    set(AVX_STR "2_0")
    set(AVX_FOUND 1)
elseif(DETECTED_AVX_10)
    set(AVX_VERSION "1.0")
    set(AVX_STR "1_0")
    set(AVX_FOUND 1)
endif()


if(CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  if(DETECTED_AVX_20)
      SET(AVX_FLAGS "${AVX_FLAGS} -mavx2")
  elseif(DETECTED_AVX_10)
      SET(AVX_FLAGS "${AVX_FLAGS} -mavx")
  endif()
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Intel")
  set(AVX_FLAGS "-xHost")
elseif(MSVC)
  if(DETECTED_AVX_20)
      SET(AVX_FLAGS "${AVX_FLAGS} /arch:AVX2")
  elseif(DETECTED_AVX_10)
      SET(AVX_FLAGS "${AVX_FLAGS} /arch:AVX")
  endif()
endif()

if(AVX_FOUND)
  message(STATUS "  Found AVX ${AVX_VERSION} extensions, using flags: ${AVX_FLAGS}")
else()
  message(STATUS "  No AVX support found")
  set(AVX_FLAGS "")
endif()

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} ${AVX_FLAGS}")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} ${AVX_FLAGS}")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} ${AVX_FLAGS}")

return()
#-------------------------------------

# If no AVX support is found, print an error message.
if(AVX_FIND_VERSION)
  set(_AVX_ERROR_MESSAGE "AVX ${AVX_FIND_VERSION} support is not found on this architecture")
else()
  set(_AVX_ERROR_MESSAGE "AVX support is not found on this architecture")
endif()

if(AVX_FIND_REQUIRED)
  message(FATAL_ERROR "${_AVX_ERROR_MESSAGE}")
elseif(NOT AVX_FIND_QUIETLY)
  message(STATUS "${_AVX_ERROR_MESSAGE}")
endif()
