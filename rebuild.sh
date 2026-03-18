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

# if [[ -z "${IN_NIX_SHELL-}" ]] && which nix 2>/dev/null; then
#     echo "Building via nix"
#     exec nix develop --no-warn-dirty --command "$0"
# fi

if [[ -z "${TMPDIR:-}" ]]; then
    export TMPDIR="/tmp"
fi

which ghc >/dev/null && \
   ( rm -f compiled/get-cabal-configuration* compiled/*.o compiled/*.hi;
     ghc -Wall -Werror -O2 -odir compiled/ -hidir compiled/ -o compiled/get-cabal-configuration "$EMACS_ROOT/third-party/flycheck-haskell/get-cabal-configuration.hs" && \
         ( [[ -f compiled/get-cabal-configuration.exe ]] && strip compiled/get-cabal-configuration.exe || strip compiled/get-cabal-configuration)
   )

bash ./scripts/recompile.sh
bash ./scripts/dump.sh
bash ./tests/run-tests.sh "${@}"

exit 0

