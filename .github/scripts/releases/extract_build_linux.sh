#!/usr/bin/env bash

set -e

DEST=${1}
BIN_SOURCE=${2}
SOURCE=${3}

mkdir -p $DEST

cp $BIN_SOURCE/game/gk $DEST
cp $BIN_SOURCE/goalc/goalc $DEST
cp $BIN_SOURCE/decompiler/extractor $DEST

strip $DEST/gk
strip $DEST/goalc
strip $DEST/extractor

chmod +x $DEST/gk
chmod +x $DEST/goalc
chmod +x $DEST/extractor

mkdir -p $DEST/data
mkdir -p $DEST/data/launcher/
mkdir -p $DEST/data/decompiler/
mkdir -p $DEST/data/assets
mkdir -p $DEST/data/game
mkdir -p $DEST/data/log
mkdir -p $DEST/data/game/graphics/opengl_renderer/

cp -r $SOURCE/.github/scripts/releases/error-code-metadata.json $DEST/data/launcher/error-code-metadata.json
cp -r $SOURCE/decompiler/config $DEST/data/decompiler/
cp -r $SOURCE/goal_src $DEST/data
cp -r $SOURCE/game/assets $DEST/data/game/
cp -r $SOURCE/game/graphics/opengl_renderer/shaders $DEST/data/game/graphics/opengl_renderer
cp -r $SOURCE/custom_levels $DEST/data
