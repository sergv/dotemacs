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

shared_ext=$((EMACS_FORCE_PRISTINE=1 emacs --batch --eval '(message "%s" (car dynamic-library-suffixes))' 2>&1 || true) | head -n 1)

case "${shared_ext}" in
    ".dll" | ".so" )
        ;;
    * )
        echo "Invalid shared extension: '$shared_ext'" >&2
        exit 1
        ;;
esac

mkdir -p "lib"

for x in native/tree-sitter* native/tree-sitter-haskell/hsc; do
    echo "$x"
    name="$(basename "$x")"
    name="${name##tree-sitter-}"
    if [[ -f "$x/src/scanner.c" ]]; then
        "${CC:-cc}" -O2 -fPIC "-I$x/src" "$x/src/parser.c" "$x/src/scanner.c" -shared -o "lib/libtree-sitter-$name$shared_ext"
    elif [[ -f "$x/src/parser.c" ]]; then
        "${CC:-cc}" -O2 -fPIC "-I$x/src" "$x/src/parser.c" -shared -o "lib/libtree-sitter-$name$shared_ext"
    else
        echo "Invalid treesitter library: '$x'" >&2
    fi
done

exit 0

