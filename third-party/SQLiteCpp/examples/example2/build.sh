#!/bin/sh
# Copyright (c) 2012-2020 Sébastien Rombauts (sebastien.rombauts@gmail.com)
#
# Distributed under the MIT License (MIT) (See accompanying file LICENSE.txt
# or copy at http://opensource.org/licenses/MIT)

# exit on first error
set -e

mkdir -p build
cd build

# Generate a Makefile for GCC (or Clang, depanding on CC/CXX envvar)
cmake -DCMAKE_BUILD_TYPE=Debug ..

# Build (ie 'make')
cmake --build .

