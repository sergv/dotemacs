#! /usr/bin/env bash
#
# File: rebuild.sh
#
# Created: 13 January 2021
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

set -e

#./scripts/dump.sh &&
which ghc >/dev/null && \
    ( cd third-party/flycheck-haskell;
      ghc -Wall -Werror -O2 -o get-cabal-configuration get-cabal-configuration.hs && \
          ( [[ -f get-cabal-configuration.exe ]] && strip get-cabal-configuration.exe || strip get-cabal-configuration)
    )

./scripts/recompile.sh && ./tests/run-tests.sh "${@}"

exit 0

