#! /usr/bin/env bash
#
# File: recompile-one.sh
#
# Created: 10 January 2021
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

emacs --batch -Q --eval "(progn (toggle-debug-on-error) (load-library \"~/.emacs\") (load-library \"$1\") (byte-compile-file \"$1\"))"

exit 0

