#!/bin/bash
#
# File: recompile.sh
#
# Created: Tuesday, 11 September 2012
#

emacs_dir=${1:-"${EMACS_ROOT}"}

if [[ ! -d "$emacs_dir" ]]; then
    echo "Emacs directory is not configured properly: either set EMACS_ROOT environment variable or pass directory to this script"
    exit 1
fi

function inform {
    local msg="$1"
    echo "[$msg]"
}

function fatal {
    local msg="$1"
    echo "[$msg]" >&2
    exit 1
}

function update-dir-autoloads {
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
    read -r -d '' emacs_cmd <<EOF
(progn
  ;; Completely disable local variables because they cause much
  ;; trouble when files have invalid local variable entries.
  (defun hack-local-variables (&optional ignored) nil)
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
    emacs --batch --eval "$emacs_cmd"
}

inform "Removing generated autoload el files"
rm -f \
   "src/local-autoloads.el" \
   "third-party/auctex/preview.el" \
   "third-party/auctex/tex-site.el" \
   "third-party/clojure-mode/clojure-mode-autoloads.el" \
   "third-party/smartparens/smartparens-autoloads.el"  \
   "third-party/sml-mode/sml-mode-autoloads.el" \
   "third-party/flycheck/flycheck-autoloads.el" \
   "third-party/elm-mode/elm-mode-autoloads.el"

inform "Removing old *.elc files"
find -O3 . -name '*.elc' -delete


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
    "src" \
    "src/eproj" \
    "src/haskell" \
    "src/latex" \
    "src/lisp" \
    "src/python" \
    "src/shell" \
    "third-party/auctex" \
    "third-party/button-lock" \
    "third-party/clojure-mode" \
    "third-party/company-statistics" \
    "third-party/el-patch" \
    "third-party/elm-mode" \
    "third-party/flycheck" \
    "third-party/flycheck-elm" \
    "third-party/flycheck-liquidhs" \
    "third-party/groovy-mode" \
    "third-party/haskell-mode" \
    "third-party/intero" \
    "third-party/liquid-types" \
    "third-party/misc-modes" \
    "third-party/pkg-info" \
    "third-party/popup-el" \
    "third-party/pos-tip" \
    "third-party/rust-mode" \
    "third-party/smartparens" \
    "third-party/sml-mode" \
    "third-party/toml-mode" \
    "third-party/tuareg" \
    "third-party/yafolding.el"

if which make >/dev/null; then
    inform "Building autoloads in third-party/org-mode"
    pushd "third-party/org-mode"
    make autoloads
    popd
else
    inform "warning: 'make' not found, not updating autoloads" >&2
fi

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
emacs --batch --load src/recompile.el --eval "(recompile-main \"$emacs_dir\")"

exit 0

