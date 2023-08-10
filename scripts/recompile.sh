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
   "src/local-autoloads.el" \
   "third-party/auctex/preview.el" \
   "third-party/auctex/tex-site.el" \
   "third-party/clojure-mode/clojure-mode-autoloads.el" \
   "third-party/smartparens/smartparens-autoloads.el"  \
   "third-party/sml-mode/sml-mode-autoloads.el" \
   "third-party/flycheck/flycheck-autoloads.el"

inform "Removing old *.elc files"
find -O3 . \( -name '*.elc' -o -name '*.eln' \) -delete
rm -frv "$emacs_dir/eln-cache"


tex_site_el="$emacs_dir/third-party/auctex/tex-site.el"
if [[ ! -f "${tex_site_el}" ]]; then
    tex_site_source="${tex_site_el}.in"
    if [[ ! -f "${tex_site_source}" ]]; then
        fatal "Cannot find source for tex-site.el: ${tex_site_source}"
    fi
    inform "Generating ${tex_site_el} from ${tex_site_source}"
    sed -r \
        -e 's,@lisppackage(lisp|data)dir@,(file-name-directory load-file-name),' \
        -e 's,@lispautodir@,temporary-file-directory,' \
        -e 's,@AUCTEXVERSION@,latest,' \
        -e 's,@AUCTEXDATE@,latest,' \
        <"${tex_site_source}" \
        >"${tex_site_el}"
    echo -e "\n\n(provide 'tex-site)" >>"${tex_site_el}"
fi

preview_el="$emacs_dir/third-party/auctex/preview.el"
if [[ ! -f "${preview_el}" ]]; then
    preview_source="${preview_el}.in"
    if [[ ! -f "${preview_source}" ]]; then
        fatal "Cannot find source for preview.el: ${preview_source}"
    fi
    inform "Generating ${preview_el} from ${preview_source}"
    sed -r \
        -e 's/@PREVIEWDATE@/latest/' \
        -e 's/@PREVIEWVERSION@/latest/' \
        <"${preview_source}" \
        >"${preview_el}"
fi


inform "Generating src/local-autoloads.el"
update-dir-autoloads \
    "src/local-autoloads.el" \
    $(find . \( -path '*/tests' -o -path '*/testing' -o -path '*/test' -o -name 'scripts' -o -name '.cask' -o -name '.git' \) -prune -o -type f -name '*.el' -print0 | xargs -0 grep -l ';;;###autoload' | xargs dirname | sort | uniq | sed 's,^\./,,')

mkdir -p "${emacs_dir}/prog-data"
mkdir -p "${emacs_dir}/resources"
# for fresh emacsen
mkdir -p "${emacs_dir}/resources/themes"

if [[ ! -f "$emacs_dir/prog-data/persistent-store" ]]; then
    echo "()" >"$emacs_dir/prog-data/persistent-store"
fi

# if [[ ! -f "$emacs_dir/third-party/auctex/preview-latex.el" ]]; then
#     inform "Generating $emacs_dir/third-party/auctex/preview-latex.el"
#     cat <<\QEOF >"$emacs_dir/third-party/auctex/preview-latex.el"
# foobar
# QEOF
#
# fi

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

    echo "CONFIG = $cfg"

    # Generate config and native-compile trampolines
    "$emacs" -Q --batch --load src/recompile.el --eval "(recompile-main \"$emacs_dir\" 0 1 nil \"$cfg\")"

    ( seq 0 "$((n - 1))" | xargs --replace=INPUT --max-args=1 -P "$n" --verbose "$emacs" -Q --batch --load src/recompile.el --eval "(recompile-main \"$emacs_dir\" INPUT $n nil nil)";
      find . -type f -name '*.elc' -print | xargs -n 1 -P "$n" "$emacs" --batch -l "$cfg" -f batch-native-compile;
      rm "$cfg"
    ) || rm "$cfg"

    # seq 0 "$((n - 1))" | xargs --replace=INPUT --max-args=1 -P "$n" --verbose "$emacs" -Q --batch --load src/recompile.el --eval "(recompile-main \"$emacs_dir\" INPUT $n t nil)"
else
    seq 0 "$((n - 1))" | xargs --replace=INPUT --max-args=1 -P "$n" --verbose "$emacs" -Q --batch --load src/recompile.el --eval "(recompile-main \"$emacs_dir\" INPUT $n nil nil)"
fi

exit 0

