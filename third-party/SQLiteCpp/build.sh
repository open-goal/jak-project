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
cmake -DCMAKE_BUILD_TYPE=Debug -DSQLITECPP_USE_ASAN=ON -DSQLITECPP_USE_GCOV=OFF -DSQLITECPP_BUILD_EXAMPLES=ON -DSQLITECPP_BUILD_TESTS=ON ..

# Build (ie 'make')
cmake --build .

# Build and run unit-tests (ie 'make test')
ctest --output-on-failure

# And with Valgrind
echo "Note: uncomment to run valgrind memcheck"
#valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes --error-exitcode=1 ./SQLiteCpp_example1
#valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes --error-exitcode=1 ./SQLiteCpp_tests
