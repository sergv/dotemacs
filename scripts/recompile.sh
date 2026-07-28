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
export EMACS_DEBUG=0
emacs="${EMACS:-emacs}"
emacs_dir=${1:-"${EMACS_ROOT}"}

if [[ -z "${2:-}" ]]; then
    artifacts_dir="${emacs_dir}"
    zipped_el_dest="nil"
else
    artifacts_dir=${2:-"${emacs_dir}"}
    zipped_el_dest="\"${artifacts_dir}\""
fi

compilation_dest="$artifacts_dir/compiled"

source "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/utils.sh"

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

function generate-autoloads() {
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
        make-backup-files nil
        backup-inhibited t
        autoload-compute-prefixes nil)
  ;; Eliminate all doc strings from autoloads file
  ;; (defun help-add-fundoc-usage (doc args) nil)
  (loaddefs-generate (list ${dirs[*]}) "$name" nil "(defvar el-patch-features nil)"))
EOF
    "$emacs" --batch --eval "$emacs_cmd" #>/dev/null 2>&1
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
generate-autoloads \
    "$compilation_dest/local-autoloads.el" \
    $(gen-el-files "-print0" | xargs -0 grep -l ';;;###autoload' | xargs dirname | sort | uniq | sed 's,^\./,,')

inform "Recompiling"

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

  (defconst +recompile--zipped-el-dest+ $zipped_el_dest)

  (setf treesit-extra-load-path '("$artifacts_dir/lib"))

  (setf cl--optimize-speed 3
        cl--optimize-safety 0
        ;; byte-compile-error-on-warn t
        compilation-safety 0
        jka-compr-verbose nil)

  (setf with-editor-emacsclient-executable nil
        byte-compile-dest-file-function
        (lambda (path)
          (let* ((filename (file-name-nondirectory path))
                   (dir (if (member filename '("init.el" "early-init.el"))
                            ""
                          "compiled/elc/")))
              (concat "${artifacts_dir}/" dir filename "c"))))

  (defun recompile--write-zipped-el (zipped-el-dest emacs-dir file)
    (let* ((dest-base
            (concat zipped-el-dest (strip-string-prefix (directory-file-name emacs-dir) file)))
           (dest (concat dest-base ".gz")))
      (with-temp-buffer
        (if (zerop
             (call-process "gzip"
                           file
                           (current-buffer)
                           nil
                           "--best"
                           "--stdout"))
            (progn
              (make-directory (file-name-directory dest) t)
              (jka-compr-run-real-handler
               'write-region
               (list (point-min) (point-max) dest)))
          (error "Failed to compress source ‘%s’ to ‘%s’:\n%s" file dest (buffer-substring-no-properties (point-min) (point-max)))))))

  (load "bytecomp" nil t))
EOF

define compile_loop <<'EOF'
(dolist (file command-line-args-left)

  (let ((should-report-warnings? nil))
    (dolist (dir '("src/" "third-party/dante/" "third-party/misc-modes/revive-minimal.el"))
      (setf should-report-warnings?
            (or should-report-warnings?
                (string-prefix-p (concat +emacs-config-path+ "/" dir) file))))
    (if should-report-warnings?
        (setq byte-compile-warnings '(not docstrings-wide docstrings lexical)
              bytecomp--inhibit-lexical-cookie-warning nil)
      (setq byte-compile-warnings nil
            bytecomp--inhibit-lexical-cookie-warning t)))

  (if (batch-byte-compile-file file)
      (when (stringp +recompile--zipped-el-dest+)
        (recompile--write-zipped-el +recompile--zipped-el-dest+ +emacs-config-path+ file))
    ;; (error "Compilation errors in %s" file)
  ))
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

    # (condition-case err
    #     (let ((no-native-compile nil)
    #           (byte-native-compiling t)
    #           (byte-native-qualities nil)
    #           ;; Batch compilation has memory leak thanks to libgccjit.
    #           (comp-running-batch-compilation nil)
    #           (native-comp-debug 0)
    #           ;; (native-comp-compiler-options '("-O2"))
    #           ;; (native-comp-driver-options '("-march=native"))
    #           )
    #       (native-compile file
    #                       (comp-el-to-eln-filename file)))
    #   (error
    #    (message "[recompile.el] %s failed to native-compile %s: %s" k file (cdr err))))

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
        # Marginally more checking but three times slower.
        # xargs -0 -P "$jobs" -n 1 \
        xargs -0 -P "$jobs" -n 5 \
              "$emacs" -Q --batch \
              -L "$artifacts_dir/lib" \
              "${load_path[@]}" \
              --eval "$eval_prelude" \
              -l "$compilation_dest/local-autoloads.el.gz" \
              --eval "$compile_loop"
    if [ $? -ne 0 ]; then
        echo "Byte compilation failed" >&2
        exit 1
    fi
    # todo: use zipped_el_dest
fi

exit 0

