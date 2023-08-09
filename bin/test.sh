#!/bin/sh
npm install @abaplint/transpiler-cli @abaplint/runtime
# rm -rf transpiled
echo "Building ..."
./node_modules/.bin/abap_transpile transpile_for_testing.json
echo "Running unit tests ..."
node transpiled/index.mjs
