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

if [[ -z "${TMPDIR:-}" ]]; then
    echo "TMPDIR not set up" >&2
    exit 1
fi

shared_ext=$((EMACS_FORCE_PRISTINE=1 emacs --batch --eval '(message "%s" (car dynamic-library-suffixes))' 2>&1 || true) | awk '!/Inferior.*exited.*normally/' | tail -n 1)

case "${shared_ext}" in
    ".dll" | ".so" | ".dylib" )
        ;;
    * )
        echo "Invalid shared extension: '$shared_ext'" >&2
        exit 1
        ;;
esac

mkdir -p "tree-sitter"

for x in native/tree-sitter* native/tree-sitter-haskell/hsc; do
    echo "$x"
    name="$(basename "$x")"
    name="${name##tree-sitter-}"

    parser="$TMPDIR/parser.c"
    if [[ -f "$x/src/parser.c.xz" ]]; then
        xz --decompress --stdout "$x/src/parser.c.xz" >"$parser"
    else
        echo "Invalid treesitter library, compressed parser file does not exist: ‘$x/src/parser.c.xz’" >&2
        exit 1
    fi

    if [[ -f "$x/src/scanner.c" ]]; then
        "${CC:-cc}" -O2 -fPIC "-I$x/src" "$parser" "$x/src/scanner.c" -shared -o "tree-sitter/libtree-sitter-$name$shared_ext"
    else
        "${CC:-cc}" -O2 -fPIC "-I$x/src" "$parser" -shared -o "tree-sitter/libtree-sitter-$name$shared_ext"
    fi
done

exit 0

