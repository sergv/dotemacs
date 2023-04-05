#! /usr/bin/env bash
#
# File: rebuild-no-nix.sh
#
# Created: 13 January 2021
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail
set -e

if which nix 2>/dev/null && [[ "${RUNNING_UNDER_NIX:-0}" != 1 ]]; then
    echo "Building via nix"
    export RUNNING_UNDER_NIX=1
    exec nix develop --command "$0"
fi

if [[ "${RUNNING_UNDER_NIX:-0}" == 1 ]]; then
    export TMPDIR="/tmp"
fi

# ./scripts/dump.sh &&
which ghc >/dev/null && \
   ( cd third-party/flycheck-haskell;
     ghc -Wall -Werror -O2 -o get-cabal-configuration get-cabal-configuration.hs && \
         ( [[ -f get-cabal-configuration.exe ]] && strip get-cabal-configuration.exe || strip get-cabal-configuration)
   )

bash ./scripts/recompile.sh
bash ./scripts/dump.sh
bash ./tests/run-tests.sh "${@}"

exit 0

