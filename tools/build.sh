#!/bin/bash

# tools/build.sh [folder]

set -e

# Set the default directory to .dist 
if [ "$#" -lt 1 ]; then
    DIST=.dist
else
    DIST="$1"
fi

# Setup the distribution
mkdir $DIST
mkdir $DIST/bin
mkdir $DIST/packages

# Compile the compiler
cabal build hazy

# Get the compiler from Cabal's store
HAZY=$(cabal list-bin hazy)

# Copy the compiler while preserving the file extension
cp $HAZY $DIST/bin/$(basename $HAZY)

# Copy and Compile the runtime
mkdir $DIST/packages/runtime
mkdir $DIST/packages/runtime/header
mkdir $DIST/packages/runtime/header/Hazy

mkdir $DIST/packages/runtime/artifact
mkdir $DIST/packages/runtime/artifact/Hazy

cp runtime/package $DIST/packages/runtime/package

cp runtime/header/Hazy.hs $DIST/packages/runtime/header/Hazy.hs
cp runtime/header/Hazy/Builtin.hs $DIST/packages/runtime/header/Hazy/Builtin.hs
cp runtime/source/Hazy/Helper.hs $DIST/packages/runtime/header/Hazy/Helper.hs

cp runtime/javascript/Hazy.mjs $DIST/packages/runtime/artifact/Hazy.mjs
cp runtime/javascript/Hazy/Builtin.mjs $DIST/packages/runtime/artifact/Hazy/Builtin.mjs
$DIST/bin/hazy --bare -c -I runtime/header runtime/source/Hazy/Helper.hs -o $DIST/packages/runtime/artifact

# Compile base
$DIST/bin/hazy --pack --bare-runtime library/base/source -o $DIST/packages/base $(cat library/base/flags)
