#! /usr/bin/env bash
#
# File: recompile-tree-sitter.sh
#
# Created:  9 August 2023
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

set -e

for x in native/tree-sitter*; do
    echo "$x"
    if [[ -f "$x/src/scanner.c" ]]; then
        gcc -Os -fPIC "-I$x/src" "$x/src/parser.c" "$x/src/scanner.c" -shared -o "lib/lib$(basename "$x").so"
    else
        gcc -Os -fPIC "-I$x/src" "$x/src/parser.c" -shared -o "lib/lib$(basename "$x").so"
    fi
done

exit 0

