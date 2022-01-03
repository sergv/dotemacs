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

cd "$(dirname "$0")"

tests=""
if [[ "$#" -gt 0 ]]; then
    for x in "${@}"; do
        tests="$tests -l $x"
    done
else
    for x in *.el; do
        tests="$tests -l $x"
    done
fi

emacs -Q --batch \
      -L "$EMACS_ROOT/src" \
      -L "$EMACS_ROOT/tests" \
      --eval "(progn (require 'cl))" \
      --eval "(progn (require 'cl-lib))" \
      -l start \
      $tests \
      -f ert-run-tests-batch-and-exit

exit 0

