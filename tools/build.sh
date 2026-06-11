#!/bin/bash

# tools/build.sh [folder]

set -euo pipefail

# Set the default directory to .dist 
if [ "$#" -lt 1 ]; then
    DIST=.dist
else
    DIST="$1"
fi

# Setup the distribution
PACKAGE=$DIST/lib/hazy/packages

mkdir -p $DIST/bin
mkdir -p $PACKAGE

# Compile the compiler
cabal build hazy

# Get the compiler from Cabal's store
HAZY=$(cabal list-bin hazy)

# Copy the compiler while preserving the file extension
cp $HAZY $DIST/bin/$(basename $HAZY)

# Copy and Compile the runtime
mkdir $PACKAGE/hazy-internal
mkdir $PACKAGE/hazy-internal/header
mkdir $PACKAGE/hazy-internal/header/Hazy

mkdir $PACKAGE/hazy-internal/artifacts
mkdir $PACKAGE/hazy-internal/artifacts/Hazy

cp runtime/package $PACKAGE/hazy-internal/package

HEADER="Hazy Hazy/Builtin"
SOURCE="Hazy/Helper Hazy/Prelude Hazy/PreludeList Hazy/PreludeText Hazy/PreludeIO\
 Hazy/PreludeChar Hazy/PreludeRatio Hazy/PreludeNumeric Hazy/PreludeMonad\
 Hazy/PreludeString Hazy/PreludeMonoid Hazy/PreludeNonEmpty"

for FILE in $HEADER; do
    cp runtime/header/$FILE.hs $PACKAGE/hazy-internal/header/$FILE.hs
done

for FILE in $SOURCE; do
    cp runtime/source/$FILE.hs $PACKAGE/hazy-internal/header/$FILE.hs
done

for FILE in $HEADER; do
    cp runtime/javascript/$FILE.mjs $PACKAGE/hazy-internal/artifacts/$FILE.mjs
done

$DIST/bin/hazy --bare -c -I runtime/header runtime/source -o $PACKAGE/hazy-internal/artifacts $(cat runtime/flags)

# Compile base
$DIST/bin/hazy --pack --bare-runtime library/base/source -o $PACKAGE/base $(cat library/base/flags)
