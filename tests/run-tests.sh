#! /usr/bin/env bash
#
# File: run-tests.sh
#
# Created: 21 October 2017
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail
set -e

cd "$(dirname "$0")"

emacs="${EMACS:-emacs}"

tests=""
matcher="t"

if [[ "$#" -gt 0 ]]; then
    for x in "${@}"; do
        if [[ -f "$x" ]]; then
            tests="$tests -l $x"
        else
            matcher="$x"
        fi
    done
fi

if [[ -z "$tests" ]]; then
    for x in "$EMACS_ROOT/tests"/*.el; do
        # tests="$tests -l $x"
        tests="$tests (require '$(basename "${x%%.el}"))"
    done
        # tests="$tests (require '$(basename "${x%%.el}"))"

    for y in "haskell-mode/tests" "nix-mode/tests" "f.el/test" "rainbow-delimiters"; do
        for x in "$EMACS_ROOT/third-party/$y"/*.el; do
            tests="$tests (require '$(basename "${x%%.el}"))"
            # tests="$tests -l $x"
        done
    done
fi

# "$emacs" -Q \
#       -L "$EMACS_ROOT/src" \
#       -L "$EMACS_ROOT/src/custom" \
#       -L "$EMACS_ROOT/tests" \
#       -L "$EMACS_ROOT/third-party/haskell-mode/tests" \
#       --eval "(progn (require 'cl))" \
#       --eval "(progn (require 'cl-lib))" \
#       -l start \
#       $tests \
#        --eval "(ert $matcher)"

"$emacs" -Q --batch \
      -L "$EMACS_ROOT/src" \
      -L "$EMACS_ROOT/src/custom" \
      -L "$EMACS_ROOT/tests" \
      -L "$EMACS_ROOT/third-party/haskell-mode/tests" \
      -L "$EMACS_ROOT/third-party/nix-mode/tests" \
      -L "$EMACS_ROOT/third-party/f.el/test" \
      -L "$EMACS_ROOT/third-party/rainbow-delimiters" \
      --eval "(progn (require 'cl))" \
      --eval "(progn (require 'cl-lib))" \
      -l start \
      --eval "(progn $tests)" \
       --eval "(ert-run-tests-batch-and-exit $matcher)"

      #-f ert-run-tests-batch-and-exit

exit 0

