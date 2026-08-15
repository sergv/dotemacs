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

emacs="${EMACS:-emacs}"
emacs_tests_dir="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"

root="${EMACS_TEST_ROOT:-${EMACS_ROOT:-$(dirname "$emacs_tests_dir")}}"

if [[ ! -v EMACS_DEBUG ]]; then
   # Disable debugging by default, but still allow allow to override.
   export EMACS_DEBUG=0
fi

function define() {
    IFS='\n' read -r -d '' ${1} || true
}

if [[ -v EMACS_TEST_EXTRA_SO_DIR ]]; then
    test_prelude_setup_flag="--directory=$EMACS_TEST_EXTRA_SO_DIR"
    define test_prelude_setup <<EOF
(progn
  (setf jka-compr-verbose nil)
  (push "$EMACS_TEST_EXTRA_SO_DIR" treesit-extra-load-path))
EOF
else
    test_prelude_setup_flag=""
    define test_prelude_setup <<EOF
(setf jka-compr-verbose nil)
EOF
fi

if [[ -v EMACS_TEST_EXTRA_EL_DIR ]]; then
    test_prelude_setup_flag2="--directory=$EMACS_TEST_EXTRA_EL_DIR"
else
    test_prelude_setup_flag2=""
fi

cd "$(dirname "$0")"

declare -a to_load
to_load=()

matcher=""

declare -a tests

if [[ "$#" -gt 0 ]]; then
    for x in "${@}"; do
        if [[ -f "$x" ]]; then
            to_load+=( "-l" "$x" )
        else
            matcher="$x"
        fi
    done
fi

if [[ "${#to_load[@]}" == 0 ]]; then
    for x in "$emacs_tests_dir"/*.el; do
        tests+=( "$(basename "${x%%.el}")" )
        # tests="$tests -l $x"
        # tests="$tests (require '$(basename "${x%%.el}"))"
    done

    # "lsp-mode/test"
    for y in "haskell-mode/tests" "nix-mode/tests" "nix-ts-mode/test" "f.el/test" "rainbow-delimiters" "poly-mode/tests"; do
        for x in "$emacs_tests_dir/../third-party/$y"/*.el; do
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
#       -L "$root/src" \
#       -L "$root/src/custom" \
#       -L "$root/tests" \
#       -L "$root/third-party/haskell-mode/tests" \
#       --eval "(progn (require 'cl))" \
#       --eval "(progn (require 'cl-lib))" \
#       -l start \
#       $tests \
#        --eval "(ert $matcher)"

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

if [[ -f "$root/init.elc" ]]; then
    init_file="$root/init.elc"
elif [[ -f "$root/init.el" ]]; then
    init_file="$root/init.el"
elif [[ -f "$root/init.el.gz" ]]; then
    init_file="$root/init.el.gz"
else
    echo "Unable to locate init file in root directory: ‘$root’" >&2
    exit 1
fi

if [[ -z "$matcher" ]]; then

    if [[ -z "${TMPDIR:-}" ]]; then
        export TMPDIR="/tmp"
    fi

    logs_dest="$TMPDIR/emacs-test-logs"

    define command <<EOF
combined='INPUT';
mod_name="\${combined%,*}"
m="\${combined#*,}"
if [[ "\$m" != "nil" ]]; then
    suffix="-\${m}"
    m="\"\${m}\""
else
    suffix=""
fi
"$emacs" \\
    -Q \\
    --batch \\
    $test_prelude_setup_flag \\
    $test_prelude_setup_flag2 \\
    --eval "$test_prelude_setup" \\
    -l "$init_file" \\
    -L "$emacs_tests_dir" \\
    -L "$emacs_tests_dir/../third-party/haskell-mode/tests" \\
    -L "$emacs_tests_dir/../third-party/nix-mode/tests" \\
    -L "$emacs_tests_dir/../third-party/nix-ts-mode/test" \\
    -L "$emacs_tests_dir/../third-party/f.el/test" \\
    -L "$emacs_tests_dir/../third-party/rainbow-delimiters" \\
    -L "$emacs_tests_dir/../third-party/poly-mode" \\
    -L "$emacs_tests_dir/../third-party/poly-mode/tests" \\
    ${to_load[@]} \\
    --eval "$requires" \\
    --eval "(require '\${mod_name})" \\
    --eval "(ert-run-tests-batch-and-exit \${m})" 2>"$logs_dest/\${mod_name}\${suffix}.log" >&2
EOF

    [[ -d "$logs_dest" ]] && rm -f "$logs_dest"/*.log

    mkdir -p "$logs_dest"

    jobs="1"
    if [[ -v NIX_BUILD_CORES ]]; then
        jobs="$NIX_BUILD_CORES"
    else
        cores="$(getconf _NPROCESSORS_ONLN)"
        if [[ "$OSTYPE" == "linux-gnu" ]] && command -v lscpu >/dev/null 2>&1; then
            threads_per_core=$(lscpu | awk '/^ *Thread\(s\) per core:/ { print $NF; }')
            jobs=$(( "$cores" / "$threads_per_core" ))
            # jobs=$(lscpu | awk 'BEGIN { cores = 0; threads = 0; } /^ *CPU\(s\):/ { cores = $NF; } /^ *Thread\(s\) per core:/ { threads = $NF; } END { print (cores / threads); }')
        elif [[ "$OSTYPE" == "darwin"* ]]; then
            jobs="$cores"
            # jobs=$(sysctl machdep.cpu.core_count | cut -w -f2)
        elif [[ -e /proc/cpuinfo ]]; then
            jobs="$(awk '/processor/' /proc/cpuinfo | wc -l)"
        fi
    fi
    if [[ "$jobs" -gt 5 ]]; then
        jobs="5"
    fi

    echo "Running $(( ${#tests[@]} - 1 )) test modules using $jobs threads"

    set +e

    for x in "${tests[@]}"; do
        # if [[ "$x" == "vim-tests" ]]; then
        #     for y in text-mode haskell-mode haskell-ts-mode haskell-hsc-mode emacs-lisp-mode rust-ts-mode c-mode sh-mode bash-ts-mode nix-mode; do
        #         echo "$x,$y"
        #     done
        # else
        #     echo "$x,nil"
        # fi
        echo "$x,nil"
    done | xargs -P "$jobs" -I INPUT bash -c "$command"

    # Make sure exit codes propagate back so that flake can know that tests failed.
    set -e

    "$emacs" -Q --batch -l ert -f ert-summarize-tests-batch-and-exit "$logs_dest"/*.log
else
    # -L "$root/third-party/lsp-mode/test"
  "$emacs" \
      -Q \
      --batch \
      $test_prelude_setup_flag \
      $test_prelude_setup_flag2 \
      --eval "$test_prelude_setup" \
      -l "$init_file" \
      -L "$emacs_tests_dir" \
      -L "$emacs_tests_dir/../third-party/haskell-mode/tests" \
      -L "$emacs_tests_dir/../third-party/nix-mode/tests" \
      -L "$emacs_tests_dir/../third-party/nix-ts-mode/test" \
      -L "$emacs_tests_dir/../third-party/f.el/test" \
      -L "$emacs_tests_dir/../third-party/rainbow-delimiters" \
      -L "$emacs_tests_dir/../third-party/poly-mode" \
      -L "$emacs_tests_dir/../third-party/poly-mode/tests" \
      ${to_load[@]} \
      --eval "$requires" \
      --eval "(mapcar #'require '(${tests[*]}))" \
      --eval "(ert-run-tests-batch-and-exit $matcher)"
fi

exit 0

