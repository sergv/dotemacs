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

to_load=""
matcher=""

declare -a tests

if [[ "$#" -gt 0 ]]; then
    for x in "${@}"; do
        if [[ -f "$x" ]]; then
            to_load="$to_load -l $x"
        else
            matcher="$x"
        fi
    done
fi

if [[ -z "$to_load" ]]; then
    for x in "$EMACS_ROOT/tests"/*.el; do
        tests+=( "$(basename "${x%%.el}")" )
        # tests="$tests -l $x"
        # tests="$tests (require '$(basename "${x%%.el}"))"
    done

    # "lsp-mode/test"
    for y in "haskell-mode/tests" "nix-mode/tests" "nix-ts-mode/test" "f.el/test" "rainbow-delimiters" "poly-mode/tests"; do
        for x in "$EMACS_ROOT/third-party/$y"/*.el; do
            if [[ ! -f "$x" ]]; then
                echo "Test file does not exist: '$x'" >&2
                exit 1
            fi
            if [[ $(basename "$x") != "mock-lsp-server.el" ]]; then
                tests+=( "$(basename "${x%%.el}")" )
                # tests="$tests (require '$(basename "${x%%.el}"))"
            fi
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

declare -a load_elc

if [[ ${EMACS_SKIP_ELC:-0} == 1 ]]; then
    load_elc=()
else
    load_elc=( "-L" "$EMACS_ROOT/compiled/elc" )
fi

requires=$(cat <<EOF
(progn
  (require 'cl)
  (require 'cl-lib)
  (require 'cl-extra)
  (require 'cl-macs)
  (require 'cl-seq)
  (require 'subr-x)
  (require 'pcase)
  (require 'rx))
EOF
)


if [[ -z "$matcher" ]]; then

    if [[ -z "${TMPDIR:-}" ]]; then
        export TMPDIR="/tmp"
    fi

    logs_dest="$TMPDIR/emacs-test-logs"

    command=$(cat <<EOF
combined='INPUT';
mod_name="\${combined%,*}"
m="\${combined#*,}"
if [[ "\$m" != "nil" ]]; then
    suffix="-\${m}"
    m="\"\${m}\""
else
    suffix=""
fi
"$emacs" -Q --batch \\
    -L "$EMACS_ROOT/compiled" \\
    ${load_elc[*]} \\
    -L "$EMACS_ROOT/src" \\
    -L "$EMACS_ROOT/src/custom" \\
    -L "$EMACS_ROOT/tests" \\
    -L "$EMACS_ROOT/third-party/haskell-mode/tests" \\
    -L "$EMACS_ROOT/third-party/nix-mode/tests" \\
    -L "$EMACS_ROOT/third-party/f.el/test" \\
    -L "$EMACS_ROOT/third-party/rainbow-delimiters" \\
    -L "$EMACS_ROOT/third-party/poly-mode/tests" \\
    $to_load \\
    --eval "$requires" \\
    -l start \
    --eval "(require '\${mod_name})" \\
    --eval "(ert-run-tests-batch-and-exit \${m})" 2>"$logs_dest/\${mod_name}\${suffix}.log"
EOF
)


    [[ -d "$logs_dest" ]] && rm -f "$logs_dest"/*.log

    mkdir -p "$logs_dest"

    set +e

    n="1"
    if [[ -e /proc/cpuinfo ]]; then
        n="$(awk '/processor/' /proc/cpuinfo | wc -l)"
    fi
    if [[ "$n" -gt 5 ]]; then
        n="5"
    fi

    echo "Running $(( ${#tests[@]} - 1 )) test modules using $n threads"

    for x in "${tests[@]}"; do
        # if [[ "$x" == "vim-tests" ]]; then
        #     for y in text-mode haskell-mode haskell-ts-mode haskell-hsc-mode emacs-lisp-mode rust-ts-mode c-mode sh-mode bash-ts-mode nix-mode; do
        #         echo "$x,$y"
        #     done
        # else
        #     echo "$x,nil"
        # fi
        echo "$x,nil"
    done | xargs -P "$n" -I INPUT bash -c "$command"

    "$emacs" -Q --batch -l ert -f ert-summarize-tests-batch-and-exit "$logs_dest"/*.log

else
    # -L "$EMACS_ROOT/third-party/lsp-mode/test"
  "$emacs" -Q --batch \
        -L "$EMACS_ROOT/compiled" \
        "${load_elc[@]}" \
        -L "$EMACS_ROOT/src" \
        -L "$EMACS_ROOT/src/custom" \
        -L "$EMACS_ROOT/tests" \
        -L "$EMACS_ROOT/third-party/haskell-mode/tests" \
        -L "$EMACS_ROOT/third-party/nix-mode/tests" \
        -L "$EMACS_ROOT/third-party/f.el/test" \
        -L "$EMACS_ROOT/third-party/rainbow-delimiters" \
        -L "$EMACS_ROOT/third-party/poly-mode/tests" \
        $to_load \
        --eval "$requires" \
        -l start \
        --eval "(mapcar #'require '(${tests[*]}))" \
        --eval "(ert-run-tests-batch-and-exit $matcher)"

fi

exit 0

