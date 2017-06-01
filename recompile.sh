#!/bin/bash
#
# File: recompile.sh
#
# Created: Tuesday, 11 September 2012
#

if type readlink >/dev/null 2>&1; then
    EMACS_DIR=$(dirname $(readlink -f "$0"))
else
    pushd "$(dirname "$0")" >/dev/null
    EMACS_DIR=$(pwd)
    popd >/dev/null
fi
emacs_dir=${1:-"${EMACS_DIR}"}

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

rm -f \
   "src/local-autoloads.el" \
   "third-party/clojure-mode/clojure-mode-autoloads.el" \
   "third-party/smartparens/smartparens-autoloads.el"  \
   "third-party/sml-mode/sml-mode-autoloads.el" \
   "third-party/flycheck/flycheck-autoloads.el" \
   "third-party/elm-mode/elm-mode-autoloads.el"

update-dir-autoloads \
    "src/local-autoloads.el" \
    "src" \
    "src/eproj" \
    "src/haskell" \
    "src/latex" \
    "src/lisp" \
    "src/python" \
    "src/shell" \
    "third-party/clojure-mode" \
    "third-party/elm-mode" \
    "third-party/flycheck" \
    "third-party/flycheck-elm" \
    "third-party/haskell-mode" \
    "third-party/misc-modes" \
    "third-party/pkg-info" \
    "third-party/smartparens" \
    "third-party/sml-mode" \
    "third-party/tuareg" \
    "third-party/yafolding.el"

if which make >/dev/null; then
    pushd "third-party/org-mode"
    make autoloads
    popd
else
    echo "warning: 'make' not found, not updating autoloads" >&2
fi

mkdir -p "${emacs_dir}/prog-data"
# for fresh emacsen
mkdir -p "${emacs_dir}/prog-data/themes"

if [[ ! -f "$emacs_dir/prog-data/persistent-store" ]]; then
    echo "()" >"$emacs_dir/prog-data/persistent-store"
fi

if [[ ! -f "$emacs_dir/third-party/auctex/tex-site.el" ]]; then
    cat <<\QEOF >"$emacs_dir/third-party/auctex/tex-site.el"
;;; tex-site.el - Site specific variables.  Don't edit.

;; Copyright (C) 2005 Free Software Foundation, Inc.
;;
;; completely rewritten.

;; Author: David Kastrup <dak@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file contains startup code, autoloads and variables adapted to
;; the local site configuration.  It is generated and placed by the
;; installation procedure and should not be edited by hand, nor moved
;; to a different place, as some settings may be established relative
;; to the file.

;; All user customization should be done with
;; M-x customize-variable RET

;;; Code:

(if (< emacs-major-version 21)
  (error "AUCTeX requires Emacs 21 or later"))

;; Define here in order for `M-x customize-group <RET> AUCTeX <RET>'
;; to work if the main AUCTeX files are not loaded yet.
(defgroup AUCTeX nil
  "A (La)TeX environment."
  :tag "AUCTeX"
  :link '(custom-manual "(auctex)Top")
  :link '(url-link :tag "Home Page" "http://www.gnu.org/software/auctex/")
  :prefix "TeX-"
  :group 'tex
  :load "tex" :load "latex" :load "tex-style")

(defvar TeX-lisp-directory
  (expand-file-name "auctex" (file-name-directory load-file-name))
  "The directory where most of the AUCTeX lisp files are located.
For the location of lisp files associated with
styles, see the variables TeX-style-* (hand-generated lisp) and
TeX-auto-* (automatically generated lisp).")

(add-to-list 'load-path TeX-lisp-directory)

(defvar TeX-data-directory
  (expand-file-name "auctex" (file-name-directory load-file-name))
  "The directory where the AUCTeX non-Lisp data is located.")

(defcustom TeX-auto-global
    "/usr/local/var/auctex"
  "*Directory containing automatically generated information.
Must end with a directory separator.

For storing automatic extracted information about the TeX macros
shared by all users of a site."
  :group 'TeX-file
  :type 'directory)

(defconst TeX-mode-alist
  '((tex-mode . tex-mode)
    (plain-tex-mode . tex-mode)
    (texinfo-mode . texinfo)
    (latex-mode . tex-mode)
    (doctex-mode . tex-mode))
  "Alist of built-in TeX modes and their load files.")

(defalias 'TeX-load-hack 'ignore)

(add-hook 'tex-site-unload-hook
	  (lambda ()
	    (let ((list after-load-alist))
	      (while list
		;; Adapted copy of the definition of `assq-delete-all'
		;; from Emacs 21 as substitute for
		;; `(assq-delete-all'TeX-modes-set (car list))' which
		;; fails on non-list elements in Emacs 21.
		(let* ((alist (car list))
		       (tail alist)
		       (key 'TeX-modes-set))
		  (while tail
		    (if (and (consp (car tail))
			     (eq (car (car tail)) key))
			(setq alist (delq (car tail) alist)))
		    (setq tail (cdr tail))))
		(setq list (cdr list))))
	    (setq load-path (delq TeX-lisp-directory load-path))))

(defun TeX-modes-set (var value &optional update)
  "Set VAR (which should be `TeX-modes') to VALUE.

This places either the standard or the AUCTeX versions of
functions into the respective function cell of the mode.
If UPDATE is set, a previously saved value for
the non-AUCTeX function gets overwritten with the current
definition."
  (custom-set-default var value)
  (let ((list TeX-mode-alist) elt)
    (while list
      (setq elt (car (pop list)))
      (let ((dst (intern (concat "TeX-" (symbol-name elt)))))
        (if (fboundp 'advice-add)
            (if (memq elt value)
                (advice-add elt :override dst)
              (advice-remove elt dst))
          (when (or update (null (get elt 'tex-saved)))
            (when (fboundp elt)
              (put elt 'tex-saved (symbol-function elt))))
          (defalias elt
            (if (memq elt value)
                dst
              (get elt 'tex-saved))))))))

(defcustom TeX-modes
  (mapcar 'car TeX-mode-alist)
  "List of modes provided by AUCTeX.

This variable can't be set normally; use customize for that, or
set it with `TeX-modes-set'."
  :type (cons 'set
	      (mapcar (lambda(x) (list 'const (car x))) TeX-mode-alist))
  :set 'TeX-modes-set
  :group 'AUCTeX
  :initialize(lambda (var value)
	       (custom-initialize-reset var value)
	       (unless (fboundp 'advice-add)
		 (let ((list TeX-mode-alist))
		   (while list
		     (eval-after-load (cdar list)
		       `(TeX-modes-set ',var ,var t))
		     (setq list (cdr list)))))) )

(defconst AUCTeX-version "2016-03-04"
    "AUCTeX version.
If not a regular release, the date of the last change.")

(defconst AUCTeX-date "2016-03-04"
  "AUCTeX release date using the ISO 8601 format, yyyy-mm-dd.")

;; Store bibitems when saving a BibTeX buffer
(add-hook 'bibtex-mode-hook 'BibTeX-auto-store)

;;; auto-loads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "bib-cite" "bib-cite.el" (22239 10706 15021
;;;;;;  495000))
;;; Generated autoloads from bib-cite.el

(autoload 'bib-cite-minor-mode "bib-cite" "\
Toggle bib-cite mode.
When bib-cite mode is enabled, citations, labels and refs are highlighted
when the mouse is over them.  Clicking on these highlights with [mouse-2]
runs bib-find, and [mouse-3] runs bib-display.

\(fn ARG)" t nil)

(autoload 'turn-on-bib-cite "bib-cite" "\
Unconditionally turn on Bib Cite mode.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "context" "context.el" (22239 10706 15021 495000))
;;; Generated autoloads from context.el

(defalias 'ConTeXt-mode 'context-mode)

(autoload 'context-mode "context" "\
Major mode in AUCTeX for editing ConTeXt files.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of ConTeXt-mode-hook.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "context-en" "context-en.el" (22239 10706 15021
;;;;;;  495000))
;;; Generated autoloads from context-en.el

(autoload 'context-en-mode "context-en" "\
Major mode for editing files for ConTeXt using its english interface.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of TeX-mode-hook, and then the value
of context-mode-hook.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "context-nl" "context-nl.el" (22239 10706 15021
;;;;;;  495000))
;;; Generated autoloads from context-nl.el

(autoload 'context-nl-mode "context-nl" "\
Major mode for editing files for ConTeXt using its dutch interface.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of TeX-mode-hook, and then the value
of context-mode-hook.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "font-latex" "font-latex.el" (22239 10706 19021
;;;;;;  413000))
;;; Generated autoloads from font-latex.el

(autoload 'font-latex-setup "font-latex" "\
Setup this buffer for LaTeX font-lock.  Usually called from a hook.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "latex" "latex.el" (22239 10706 19021 413000))
;;; Generated autoloads from latex.el

(autoload 'BibTeX-auto-store "latex" "\
This function should be called from `bibtex-mode-hook'.
It will setup BibTeX to store keys in an auto file.

\(fn)" nil nil)

(add-to-list 'auto-mode-alist '("\\.drv\\'" . latex-mode))

(add-to-list 'auto-mode-alist '("\\.hva\\'" . latex-mode))

(autoload 'TeX-latex-mode "latex" "\
Major mode in AUCTeX for editing LaTeX files.
See info under AUCTeX for full documentation.

Special commands:
\\{LaTeX-mode-map}

Entering LaTeX mode calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `LaTeX-mode-hook'.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.dtx\\'" . doctex-mode))

(autoload 'docTeX-mode "latex" "\
Major mode in AUCTeX for editing .dtx files derived from `LaTeX-mode'.
Runs `LaTeX-mode', sets a few variables and
runs the hooks in `docTeX-mode-hook'.

\(fn)" t nil)

(defalias 'TeX-doctex-mode 'docTeX-mode)

;;;***

;;;### (autoloads nil "multi-prompt" "multi-prompt.el" (22239 10706
;;;;;;  23021 331000))
;;; Generated autoloads from multi-prompt.el

(autoload 'multi-prompt "multi-prompt" "\
Completing prompt for a list of strings.
The first argument SEPARATOR should be the string (of length 1) to
separate the elements in the list.  The second argument UNIQUE should
be non-nil, if each element must be unique.  The remaining elements
are the arguments to `completing-read'.  See that.

\(fn SEPARATOR UNIQUE PROMPT TABLE &optional MP-PREDICATE REQUIRE-MATCH INITIAL HISTORY)" nil nil)

(autoload 'multi-prompt-key-value "multi-prompt" "\
Read multiple strings, with completion and key=value support.
PROMPT is a string to prompt with, usually ending with a colon
and a space.  TABLE is an alist.  The car of each element should
be a string representing a key and the optional cdr should be a
list with strings to be used as values for the key.

See the documentation for `completing-read' for details on the
other arguments: PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST,
DEF, and INHERIT-INPUT-METHOD.

The return value is the string as entered in the minibuffer.

\(fn PROMPT TABLE &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil)

;;;***

;;;### (autoloads nil "plain-tex" "plain-tex.el" (22239 10706 23021
;;;;;;  331000))
;;; Generated autoloads from plain-tex.el

(autoload 'TeX-plain-tex-mode "plain-tex" "\
Major mode in AUCTeX for editing plain TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{plain-TeX-mode-map}

Entering `plain-tex-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of plain-TeX-mode-hook.

\(fn)" t nil)

(autoload 'ams-tex-mode "plain-tex" "\
Major mode in AUCTeX for editing AmS-TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{AmSTeX-mode-map}

Entering AmS-tex-mode calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `AmS-TeX-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "tex" "tex.el" (22239 10706 31021 166000))
;;; Generated autoloads from tex.el

(defalias 'TeX-assoc-string (symbol-function (if (featurep 'xemacs) 'assoc 'assoc-string)))

(autoload 'TeX-tex-mode "tex" "\
Major mode in AUCTeX for editing TeX or LaTeX files.
Tries to guess whether this file is for plain TeX or LaTeX.

The algorithm is as follows:

   1) if the file is empty or `TeX-force-default-mode' is not set to nil,
      `TeX-default-mode' is chosen
   2) If \\documentstyle or \\begin{, \\section{, \\part{ or \\chapter{ is
      found, `latex-mode' is selected.
   3) Otherwise, use `plain-tex-mode'

\(fn)" t nil)

(autoload 'TeX-auto-generate "tex" "\
Generate style file for TEX and store it in AUTO.
If TEX is a directory, generate style files for all files in the directory.

\(fn TEX AUTO)" t nil)

(autoload 'TeX-auto-generate-global "tex" "\
Create global auto directory for global TeX macro definitions.

\(fn)" t nil)

(autoload 'TeX-submit-bug-report "tex" "\
Submit a bug report on AUCTeX via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your AUCTeX version and AUCTeX configuration.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "tex-bar" "tex-bar.el" (22239 10706 31021 166000))
;;; Generated autoloads from tex-bar.el

(autoload 'TeX-install-toolbar "tex-bar" "\
Install toolbar buttons for TeX mode.

\(fn)" t nil)

(autoload 'LaTeX-install-toolbar "tex-bar" "\
Install toolbar buttons for LaTeX mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "tex-fold" "tex-fold.el" (22239 10706 31021
;;;;;;  166000))
;;; Generated autoloads from tex-fold.el
 (autoload 'TeX-fold-mode "tex-fold" "Minor mode for hiding and revealing macros and environments." t)

(defalias 'tex-fold-mode 'TeX-fold-mode)

;;;***

;;;### (autoloads nil "tex-font" "tex-font.el" (22239 10706 31021
;;;;;;  166000))
;;; Generated autoloads from tex-font.el

(autoload 'tex-font-setup "tex-font" "\
Setup font lock support for TeX.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "tex-info" "tex-info.el" (22239 10706 31021
;;;;;;  166000))
;;; Generated autoloads from tex-info.el

(defalias 'Texinfo-mode 'texinfo-mode)

(autoload 'TeX-texinfo-mode "tex-info" "\
Major mode in AUCTeX for editing Texinfo files.

Special commands:
\\{Texinfo-mode-map}

Entering Texinfo mode calls the value of `text-mode-hook'  and then the
value of `Texinfo-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "tex-jp" "tex-jp.el" (22239 10706 31021 166000))
;;; Generated autoloads from tex-jp.el

(autoload 'japanese-plain-tex-mode "tex-jp" "\
Major mode in AUCTeX for editing Japanese plain TeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-plain-tex-mode'.

\(fn)" t nil)

(autoload 'japanese-latex-mode "tex-jp" "\
Major mode in AUCTeX for editing Japanese LaTeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-latex-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "texmathp" "texmathp.el" (22239 10706 31021
;;;;;;  166000))
;;; Generated autoloads from texmathp.el

(autoload 'texmathp "texmathp" "\
Determine if point is inside (La)TeX math mode.
Returns t or nil.  Additional info is placed into `texmathp-why'.
The functions assumes that you have (almost) syntactically correct (La)TeX in
the buffer.
See the variable `texmathp-tex-commands' about which commands are checked.

\(fn)" t nil)

(autoload 'texmathp-match-switch "texmathp" "\
Search backward for any of the math switches.
Limit searched to BOUND.

\(fn BOUND)" nil nil)

;;;***

;;;### (autoloads nil "toolbar-x" "toolbar-x.el" (22239 10706 35021
;;;;;;  84000))
;;; Generated autoloads from toolbar-x.el
 (autoload 'toolbarx-install-toolbar "toolbar-x")

;;;***

(provide 'auto-loads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-loads.el ends here
(provide 'tex-site)
;;; tex-site.el ends here
QEOF

fi

if [[ ! -f "$emacs_dir/third-party/auctex/preview-latex.el" ]]; then
    cat <<\QEOF >"$emacs_dir/third-party/auctex/preview-latex.el"
;;; preview-latex.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "preview" "preview.el" (22239 13945 173053
;;;;;;  96000))
;;; Generated autoloads from preview.el

(autoload 'preview-install-styles "preview" "\
Installs the TeX style files into a permanent location.
This must be in the TeX search path.  If FORCE-OVERWRITE is greater
than 1, files will get overwritten without query, if it is less
than 1 or nil, the operation will fail.  The default of 1 for interactive
use will query.

Similarly FORCE-SAVE can be used for saving
`preview-TeX-style-dir' to record the fact that the uninstalled
files are no longer needed in the search path.

\(fn DIR &optional FORCE-OVERWRITE FORCE-SAVE)" t nil)

(autoload 'LaTeX-preview-setup "preview" "\
Hook function for embedding the preview package into AUCTeX.
This is called by `LaTeX-mode-hook' and changes AUCTeX variables
to add the preview functionality.

\(fn)" nil nil)

(autoload 'preview-report-bug "preview" "\
Report a bug in the preview-latex package.

\(fn)" t nil)

;;;***

(provide 'preview-latex)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; preview-latex.el ends here
QEOF

fi

# [[ -f "$emacs_dir/src/user-info.el" ]] || touch "$emacs_dir/src/user-info.el"
# [[ -f "$emacs_dir/src/machine-specific-setup.el" ]] || touch "$emacs_dir/src/machine-specific-setup.el"

emacs --batch --load src/recompile.el --eval "(recompile-main \"$emacs_dir\")"

exit 0

