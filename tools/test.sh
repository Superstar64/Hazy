#!/usr/bin/env bash

# tools/test.sh [executable]

set -euo pipefail
shopt -s globstar

# Clear out the old test temporaries
rm -rf .test
mkdir .test

# Get the executable or compile it ourselves
if [ "$#" -lt 1 ]; then
    tools/build.sh .test/dist
    HAZY=".test/dist/bin/hazy"
else
    HAZY="$1"
fi

# Run the bad parse tests
echo "=== Bad Parse ==="
for bad in test/bad/parse/**/*.hs; do
    echo "Parsing $(basename $bad)"
    $HAZY --debug-fail "Expected" --parse "$bad"
done

# Run the bad resolve tests
echo "=== Bad Resolve ==="
for bad in test/bad/resolve/**/*.hs; do
    base="${bad%.hs}"
    fail=$(cat "$base.test")
    $HAZY --debug-fail "$fail" --resolve "$bad"
done

# Run the bad check tests
echo "=== Bad Check ==="
for bad in test/bad/check/**/*.hs; do
    base="${bad%.hs}"
    fail=$(cat "$base.test")
    $HAZY --debug-fail "$fail" --check "$bad"
done

# Run the good tests
echo "=== Good ==="
for good in test/good/**/*.hs; do
    $HAZY "$good"
done

# Run the execution tests
echo "=== Execution ==="
for run_dir in test/run/*/; do
    run=$(basename "$run_dir")
    mkdir -p ".test/$run"
    echo "Running $run"
    $HAZY -q "test/run/$run/source" -o ".test/$run"
    node ".test/$run/index.mjs" > ".test/$run/result"
    diff "test/run/$run/result" ".test/$run/result"
done