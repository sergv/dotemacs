#! /usr/bin/env bash
#
# File: recompile-tree-sitter.sh
#
# Created:  Wednesday, 15 July 2026
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail

set -e

for x in native/tree-sitter* native/tree-sitter-haskell/hsc; do
    for x in "$x"/src/{grammar.json,node-types.json,parser.c}; do
        if [[ -f "$x" ]]; then
            echo "Compressing $x"
            xz --compress -9 --extreme --stdout "$x" >"$x.xz" && rm "$x"
        fi
    done
done

exit 0

