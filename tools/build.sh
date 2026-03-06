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

# Copy the runtime
cp -R library/runtime $DIST/packages/runtime

# Compile base
$DIST/bin/hazy --pack --bare-runtime library/base/source -o $DIST/packages/base $(cat library/base/flags)
