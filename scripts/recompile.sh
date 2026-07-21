#! /usr/bin/env bash
#
# File: recompile.sh
#
# Created: Tuesday, 11 September 2012
#

# treat undefined variable substitutions as errors
set -u
# propagate errors from all parts of pipes
set -o pipefail
set -e

export EMACS_FORCE_PRISTINE=1
emacs="${EMACS:-emacs}"
emacs_dir=${1:-"${EMACS_ROOT}"}

if [[ -z "${2:-}" ]]; then
    artifacts_dir="${emacs_dir}"
    zipped_el_dest="nil"
else
    artifacts_dir=${2:-"${EMACS_COMPILED_ROOT:-${emacs_dir}}"}
    zipped_el_dest="\"${artifacts_dir}\""
fi

compilation_dest="$artifacts_dir/compiled"

source "$(dirname "$(readlink -f ""${BASH_SOURCE[0]}"")")/utils.sh"

if [[ ! -d "$emacs_dir" ]]; then
    echo "Emacs directory is not configured properly: either set EMACS_ROOT environment variable or pass directory to this script"
    exit 1
fi

function inform() {
    local msg="$1"
    echo "[$msg]"
}

function fatal() {
    local msg="$1"
    echo "[$msg]" >&2
    exit 1
}

function define() {
    IFS='\n' read -r -d '' ${1} || true
}

function update-dir-autoloads() {
    local name="$1"
    shift 1
    declare -a dirs
    for dir in "${@}"; do
        if [[ ! -d "$dir" ]]; then
            echo "update-dir-autoloads: directory $dir does not exist"
            exit 1
        fi
        dirs+=("\"$dir\"")
    done
    define emacs_cmd <<EOF
(progn
  ;; Completely disable local variables because they cause much
  ;; trouble when files have invalid local variable entries.
  (defun hack-local-variables (&rest ignored) nil)
  (setq debug-on-error t
        generated-autoload-file "$name"
        make-backup-files nil
        backup-inhibited t)
  (update-directory-autoloads ${dirs[*]}))
EOF
    "$emacs" --batch --eval "$emacs_cmd"
    gzip --best --stdout "$name" >"$name.gz"
    rm "$name"
}

function gen-el-files() {
    local print="$1"
    find "$emacs_dir" \( -path '*/native' -o -path '*/tests' -o -path '*/testing' -o -path '*/test' -o -path '*/auctex/style' -o -name 'scripts' -o -name 'resources' -o -name '.cask' -o -name '.git' \) -prune -o -type f \( -name '*.el' -a -not \( -name '*test.el' -o -name '*tests.el' -o -name '*test-utils*' -o -name '.dir-locals.el' \) \) "$print"
}

inform "Removing generated autoload el files"
rm -f \
   "$compilation_dest/local-autoloads.el" \
   "src/local-autoloads.el" \
   "third-party/clojure-mode/clojure-mode-autoloads.el" \
   "third-party/smartparens/smartparens-autoloads.el"  \
   "third-party/sml-mode/sml-mode-autoloads.el" \
   "third-party/flycheck/flycheck-autoloads.el"

inform "Removing old *.elc files"

if [[ ! -d "$compilation_dest" && ! -L "$compilation_dest" ]]; then
    mkdir "$compilation_dest"
fi

if [[ ! -d "$compilation_dest/elc" ]]; then
    mkdir "$compilation_dest/elc"
fi


[[ -d "$emacs_dir/eln-cache" ]] && rm -frv "$emacs_dir/eln-cache"
find "$emacs_dir" \( -name '*.elc' -o -name '*.eln' -o -name "${emacs}.dmp" \) -delete
find -L "$compilation_dest" \( -name '*.elc' -o -name '*.eln' -o -name "${emacs}.dmp" \) -delete

inform "Generating $compilation_dest/local-autoloads.el"
update-dir-autoloads \
    "$compilation_dest/local-autoloads.el" \
    $(gen-el-files "-print0" | xargs -0 grep -l ';;;###autoload' | xargs dirname | sort | uniq | sed 's,^\./,,')

inform "Recompiling"

jobs="1"
if [[ -v NIX_BUILD_CORES ]]; then
    jobs="$NIX_BUILD_CORES"
else
    if [[ "$OSTYPE" == "linux-gnu" ]] && command -v lscpu >/dev/null 2>&1; then
        jobs=$(lscpu | awk 'BEGIN { cores = 0; threads = 0; } /^ *CPU\(s\):/ { cores = $NF; } /^ *Thread\(s\) per core:/ { threads = $NF; } END { print (cores / threads); }')
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        jobs=$(sysctl machdep.cpu.core_count | cut -w -f2)
    elif [[ -e /proc/cpuinfo ]]; then
        jobs="$(awk '/processor/' /proc/cpuinfo | wc -l)"
    fi
fi
# if [[ "$jobs" -gt 5 ]]; then
#     jobs="5"
# fi
# jobs="1"

declare -a load_path

while IFS= read -d $'\0' -r dir ; do
    load_path+=("-L" "$dir")
    # emacs  -Q --batch -L. -Lsrc -Lsrc/haskell -Lsrc/lisp
done < <(gen-el-files "-print" | xargs dirname | sort -u | awk '!/(auctex\/style|targets|template|tests?)([/]?|$)/' | sed -re 's,^\./,,' | tr '\n' '\0')

define eval_prelude <<EOF
(progn
  (defconst +emacs-config-path+ "$emacs_dir")

  (setf cl--optimize-speed 3
        cl--optimize-safety 0
        byte-compile-warnings '(not docstrings-wide docstrings))

  (setf with-editor-emacsclient-executable nil
        byte-compile-dest-file-function
        (lambda (path)
          (concat
            "${artifacts_dir}/compiled/elc/"
            (file-name-sans-extension (file-name-nondirectory path))
            ".elc"))))
EOF

# Either 't' or 'nil'
native_comp="$(native-comp-available)"

if [[ "$native_comp" = "t" ]]; then
    # With native compilation is enabled all loaded .elc files will automatically
    # get compiled into .eln. When multiple processes do this, race condition may
    # occur and all recompilation fails.

    if [[ ! -d "$compilation_dest/eln" ]]; then
        mkdir "$compilation_dest/eln"
    fi

    echo "todo: native compilation" >&2
    exti 1

    # # # Generate config and native-compile trampolines
    # # "$emacs" -Q --batch --load src/recompile.el --eval "(recompile-main \"$emacs_dir\" 0 1 nil \"$cfg\")"
    # #
    # # ( seq 0 "$((jobs - 1))" | xargs -I INPUT --max-args=1 -P "$jobs" --verbose "$emacs" -Q --batch --load src/recompile.el --eval "(recompile-main \"$emacs_dir\" INPUT $jobs nil nil)" && \
    # #       find . -type f -name '*.elc' -print | xargs -n 1 -P "$jobs" "$emacs" --batch -l "$cfg" -f batch-native-compile
    # # ) && rm "$cfg" || rm "$cfg"
    #
    # # Preload to native-compile trampolines
    # "$emacs" -Q --batch --load "$emacs_dir/src/recompile.el" --eval "(recompile-main \"$emacs_dir\" \"$compilation_dest/elc\" 0 1 nil t nil)"
    #
    # seq 0 "$((jobs - 1))" | xargs -I INPUT --max-args=1 -P "$jobs" --verbose "$emacs" -Q --batch --load "$emacs_dir/src/recompile.el" --eval "(recompile-main \"$emacs_dir\" \"$artifacts_dir\" INPUT $jobs nil nil nil)" && \
    # seq 0 "$((jobs - 1))" | xargs -I INPUT --max-args=1 -P "$jobs" --verbose "$emacs" -Q --batch --load "$emacs_dir/src/recompile.el" --eval "(recompile-main \"$emacs_dir\" \"$artifacts_dir\" INPUT $jobs t nil $zipped_el_dest)"

else
    gen-el-files "-print0" | \
        xargs -0 -P "$jobs" -n 1 \
              "$emacs" -Q --batch \
              "${load_path[@]}" \
              --eval "$eval_prelude" \
              -f batch-byte-compile
    # todo: use zipped_el_dest
fi

exit 0

