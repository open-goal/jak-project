#!/usr/bin/env bash

set -e

echo "======================================="
echo "=       Jak Project Offline Test      ="
echo "======================================="
echo ""
echo "  ================= Cloning..."

ISO_DATA_PATH=${1}
BRANCH_NAME=${2:-master}
# Provide a default location to bind the ISO_DATA_PATH
ISO_DATA_SYM_LINK_DIR=${3:-iso_data}

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
ln -s $ISO_DATA_PATH $ISO_DATA_SYM_LINK_DIR


mkdir build
cd build
echo "  =============== Building..."
cmake ..
make -j

echo " ================ Running unit tests..."
../test.sh

echo " ================ Running offline decompiler tests..."
./offline-test

echo " ================ Decompiling..."
../scripts/shell/decomp.sh

echo " ================ Building project..."
../scripts/shell/gc.sh --cmd \(make-group\ \"iso\"\)

echo " ================ Checking assets..."
../scripts/shell/check.sh

echo " ================ Booting game..."
../scripts/shell/boot_game.sh

echo "Offline test has completed successfully!"
