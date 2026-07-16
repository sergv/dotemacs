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

source "$(dirname "$(readlink -f "$0")")/utils.sh"

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
    local dirs=""
    for dir in "${@}"; do
        if [[ ! -d "$dir" ]]; then
            echo "update-dir-autoloads: directory $dir does not exist"
            exit 1
        fi
        if [[ -z "$dirs" ]]; then
            dirs="\"$emacs_dir/$dir\""
        else
            dirs="\"$emacs_dir/$dir\" $dirs"
        fi
    done
    define emacs_cmd <<EOF
(progn
  ;; Completely disable local variables because they cause much
  ;; trouble when files have invalid local variable entries.
  (defun hack-local-variables (&rest ignored) nil)
  (setq debug-on-error t
        generated-autoload-file "$emacs_dir/$name"
        make-backup-files nil
        backup-inhibited t)
  (update-directory-autoloads $dirs)
  (message
    (concat "Updated autoloads in "
            (mapconcat #'identity (list $dirs) ", "
            ))))
EOF
    "$emacs" --batch --eval "$emacs_cmd"
}

inform "Removing generated autoload el files"
rm -f \
   "compiled/local-autoloads.el" \
   "src/local-autoloads.el" \
   "third-party/clojure-mode/clojure-mode-autoloads.el" \
   "third-party/smartparens/smartparens-autoloads.el"  \
   "third-party/sml-mode/sml-mode-autoloads.el" \
   "third-party/flycheck/flycheck-autoloads.el"

inform "Removing old *.elc files"

if [[ ! -d "$emacs_dir/compiled" && ! -L "$emacs_dir/compiled" ]]; then
    mkdir "$emacs_dir/compiled"
fi

if [[ ! -d "$emacs_dir/compiled/elc" ]]; then
    mkdir "$emacs_dir/compiled/elc"
fi


rm -frv "$emacs_dir/eln-cache"
find -O3 "$emacs_dir" \( -name '*.elc' -o -name '*.eln' -o -name "${emacs}.dmp" \) -delete
find -L -O3 "$emacs_dir/compiled" \( -name '*.elc' -o -name '*.eln' -o -name "${emacs}.dmp" \) -delete

inform "Generating compiled/local-autoloads.el"
update-dir-autoloads \
    "compiled/local-autoloads.el" \
    $(find . \( -path '*/tests' -o -path '*/testing' -o -path '*/test' -o -name 'scripts' -o -name '.cask' -o -name '.git' \) -prune -o -type f -name '*.el' -print0 | xargs -0 grep -l ';;;###autoload' | xargs dirname | sort | uniq | sed 's,^\./,,')

inform "Recompiling"

n="1"
if [[ -e /proc/cpuinfo ]]; then
    n="$(awk '/processor/' /proc/cpuinfo | wc -l)"
fi
if [[ "$n" -gt 5 ]]; then
    n="5"
fi

# Either 't' or 'nil'
native_comp="$(native-comp-available)"

if [[ "$native_comp" = "t" ]]; then
    # With native compilation is enabled all loaded .elc files will automatically
    # get compiled into .eln. When multiple processes do this, race condition may
    # occur and all recompilation fails.
    cfg="$(mktemp "$TMPDIR/config.elXXXXX")"

    if [[ ! -d "$emacs_dir/compiled/eln" ]]; then
        mkdir "$emacs_dir/compiled/eln"
    fi

    echo "CONFIG = $cfg"

    # # Generate config and native-compile trampolines
    # "$emacs" -Q --batch --load src/recompile.el --eval "(recompile-main \"$emacs_dir\" 0 1 nil \"$cfg\")"
    #
    # ( seq 0 "$((n - 1))" | xargs --replace=INPUT --max-args=1 -P "$n" --verbose "$emacs" -Q --batch --load src/recompile.el --eval "(recompile-main \"$emacs_dir\" INPUT $n nil nil)" && \
    #       find . -type f -name '*.elc' -print | xargs -n 1 -P "$n" "$emacs" --batch -l "$cfg" -f batch-native-compile
    # ) && rm "$cfg" || rm "$cfg"


    # Preload to native-compile trampolines
    "$emacs" -Q --batch --load src/recompile.el --eval "(recompile-main \"$emacs_dir\" 0 1 nil \"$cfg\")"

    seq 0 "$((n - 1))" | xargs --replace=INPUT --max-args=1 -P "$n" --verbose "$emacs" -Q --batch --load src/recompile.el --eval "(recompile-main \"$emacs_dir\" INPUT $n nil nil)" && \
    seq 0 "$((n - 1))" | xargs --replace=INPUT --max-args=1 -P "$n" --verbose "$emacs" -Q --batch --load src/recompile.el --eval "(recompile-main \"$emacs_dir\" INPUT $n t nil)"


else
    seq 0 "$((n - 1))" | xargs --replace=INPUT --max-args=1 -P "$n" --verbose "$emacs" -Q --batch --load src/recompile.el --eval "(recompile-main \"$emacs_dir\" INPUT $n nil nil)"
fi

exit 0

