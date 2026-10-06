#!/bin/sh
set -eu

usage() {
  cat <<'EOF'
Usage: bin/test.sh [-i] [-r] [-h]

Transpile the project's ABAP sources and run its ABAP unit tests offline.
Stops on installation, transpilation, or test failure; failed transpilation
does not run previously generated tests. Requires Node.js and installed
@abaplint/transpiler-cli and @abaplint/runtime dependencies.

  -i  Install/update the npm dependencies before transpiling (requires npm).
      Without -i, use the dependencies already installed in node_modules.
  -r  Remove the generated transpiled directory before building.
  -h  Show this help and exit without installing or running tests.
EOF
}

install_dependencies=false
remove_transpiled=false
while getopts ':irh' option; do
  case "$option" in
    i) install_dependencies=true ;;
    r) remove_transpiled=true ;;
    h) usage; exit 0 ;;
    *) echo "Unknown option: -$OPTARG" >&2; usage >&2; exit 2 ;;
  esac
done
shift "$((OPTIND - 1))"
if [ "$#" -ne 0 ]; then
  echo 'Unexpected arguments.' >&2
  usage >&2
  exit 2
fi

cd "$(dirname "$0")/.."
if [ "$install_dependencies" = true ]; then
  echo "Installing dependencies ..."
  npm install @abaplint/transpiler-cli @abaplint/runtime
fi

if [ "$remove_transpiled" = true ]; then
  echo "Removing generated tests ..."
  rm -rf -- ./transpiled
fi

echo "Building ..."
./node_modules/.bin/abap_transpile transpile_for_testing.json
echo "Running unit tests ..."
node transpiled/index.mjs
