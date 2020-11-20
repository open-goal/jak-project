#!/bin/bash

set -e

echo "======================================="
echo "=       Jak Project Offline Test      ="
echo "======================================="
echo ""
echo "  ================= Cloning..."

ISO_DATA_PATH=${1}
BRANCH_NAME=${2:-master}

if [ -z "$1" ]
  then
    echo "Must supply path to iso data folder!"
    exit 1
fi

echo " Branch: ${BRANCH_NAME}"
mkdir project
cd project
git clone https://github.com/water111/jak-project.git
cd jak-project
git checkout $BRANCH_NAME
git submodule update --init --recursive

# create symlink to the iso_data folder.
rm -r iso_data
ln -s $ISO_DATA_PATH 


mkdir build
cd build
echo "  =============== Building..."
cmake ..
make -j

echo " ================ Running unit tests..."
../test.sh

echo " ================ Decompiling..."
../decomp.sh

echo " ================ Building assets..."
./goalc/data_compiler

echo " ================ Checking assets..."
../check.sh

echo "Offline test has completed successfully!"
