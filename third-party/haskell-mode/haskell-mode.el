;;; haskell-mode.el --- A Haskell editing mode    -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2003, 2004, 2005, 2006, 2007, 2008, 2016
;;             Free Software Foundation, Inc

;; Copyright © 1992, 1997-1998  Simon Marlow, Graeme E Moss, and Tommy Thorn

;; Author:  1992      Simon Marlow
;;          1997-1998 Graeme E Moss <gem@cs.york.ac.uk> and
;;                    Tommy Thorn <thorn@irisa.fr>,
;;          2001-2002 Reuben Thomas (>=v1.4)
;;          2003      Dave Love <fx@gnu.org>
;;          2016      Arthur Fayzrakhmanov
;; Keywords: faces files Haskell
;; URL: https://github.com/haskell/haskell-mode

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing Haskell (the functional programming
;; language, see URL `http://www.haskell.org') in Emacs.
;;
;; Some of its major features include:
;;
;;  - syntax highlighting (font lock),
;;
;;  - automatic indentation,
;;
;;  - on-the-fly documentation,
;;
;;  - interaction with inferior GHCi/Hugs instance,
;;
;;  - scans declarations and places them in a menu.
;;
;; See URL `https://github.com/haskell/haskell-mode' and/or
;; Info node `(haskell-mode)Introduction' for more information.
;;
;; Use `M-x haskell-mode-view-news` (after Haskell Mode is installed)
;; to show information on recent changes in Haskell Mode.

;;; Change Log:

;; This mode is based on an editing mode by Simon Marlow 11/1/92
;; and heavily modified by Graeme E Moss and Tommy Thorn 7/11/98.
;;
;; Version 1.5:
;;   Added autoload for haskell-indentation
;;
;; Version 1.43:
;;   Various tweaks to doc strings and customization support from
;;   Ville Skyttä <scop@xemacs.org>.
;;
;; Version 1.42:
;;   Added autoload for GHCi inferior mode (thanks to Scott
;;   Williams for the bug report and fix).
;;
;; Version 1.41:
;;   Improved packaging, and made a couple more variables
;;   interactively settable.
;;
;; Version 1.4:
;;   Added GHCi mode from Chris Webb, and tidied up a little.
;;
;; Version 1.3:
;;   The literate or non-literate style of a buffer is now indicated
;;   by just the variable haskell-literate: nil, `bird', or `tex'.
;;   For literate buffers with ambiguous style, the value of
;;   haskell-literate-default is used.
;;
;; Version 1.2:
;;   Separated off font locking, declaration scanning and simple
;;   indentation, and made them separate modules.  Modules can be
;;   added easily now.  Support for modules haskell-doc,
;;   haskell-indent, and haskell-hugs.  Literate and non-literate
;;   modes integrated into one mode, and literate buffer indicated by
;;   value of haskell-literate(-bird-style).
;;
;; Version 1.1:
;;   Added support for declaration scanning under XEmacs via
;;   func-menu.  Moved operators to level two fontification.
;;
;; Version 1.0:
;;   Added a nice indention support from Heribert Schuetz
;;   <Heribert.Schuetz@informatik.uni-muenchen.de>:
;;
;;     I have just hacked an Emacs Lisp function which you might prefer
;;     to `indent-relative' in haskell-mode.el.  See below.  It is not
;;     really Haskell-specific because it does not take into account
;;     keywords like `do', `of', and `let' (where the layout rule
;;     applies), but I already find it useful.
;;
;;   Cleaned up the imenu support.  Added support for literate scripts.
;;
;; Version 0.103 [HWL]:
;;   From Hans Wolfgang Loidl <hwloidl@dcs.gla.ac.uk>:
;;
;;   I (HWL) added imenu support by copying the appropriate functions
;;   from hugs-mode.  A menu-bar item "Declarations" is now added in
;;   haskell mode.  The new code, however, needs some clean-up.
;;
;; Version 0.102:
;;
;;   Moved C-c C-c key binding to comment-region.  Leave M-g M-g to do
;;   the work.  comment-start-skip is changed to comply with comment-start.
;;
;; Version 0.101:
;;
;;   Altered indent-line-function to indent-relative.
;;
;; Version 0.100:
;;
;;   First official release.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'current-column-fixed)

(require 'haskell-customize)
(require 'ansi-color)
(require 'dabbrev)
(require 'compile)
(require 'etags)
(require 'flymake)
(require 'flymake-proc)
(require 'outline)
(require 'cl-lib)
(require 'haskell-ghc-support)
(require 'haskell-complete-module)
(require 'haskell-align-imports)
(require 'haskell-lexeme)
(require 'haskell-sort-imports)
(require 'haskell-string)
(require 'haskell-syntax-table)
(require 'haskell-indentation)
(require 'haskell-font-lock)
(require 'haskell-cabal)

;; All functions/variables start with `(literate-)haskell-'.

;; Version of mode.
(defconst haskell-version "16.2-git"
  "The release version of `haskell-mode'.")

;;;###autoload
(defun haskell-version (&optional here)
  "Show the `haskell-mode` version in the echo area.
With prefix argument HERE, insert it at point."
  (interactive "P")
  (let* ((haskell-mode-dir (ignore-errors
                             (file-name-directory (or (locate-library "haskell-mode") ""))))
         (version (format "haskell-mode version %s (%s)"
                           haskell-version
                           haskell-mode-dir)))
    (if here
        (insert version)
      (message "%s" version))))

;;;###autoload
(defun haskell-mode-view-news ()
  "Display information on recent changes to haskell-mode."
  (interactive)
  (with-current-buffer (find-file-read-only (expand-file-name "NEWS" haskell-mode-pkg-base-dir))
    (goto-char (point-min))
    (outline-hide-sublevels 1)
    (outline-next-visible-heading 1)
    (outline-show-subtree)))

;; Are we looking at a literate script?
(defvar-local haskell-literate nil
  "If not nil, the current buffer contains a literate Haskell script.
Possible values are: `bird' and `tex', for Bird-style and LaTeX-style
literate scripts respectively.  Set by `haskell-mode' and
`haskell-literate-mode'.  For an ambiguous literate buffer -- i.e. does
not contain either \"\\begin{code}\" or \"\\end{code}\" on a line on
its own, nor does it contain \">\" at the start of a line -- the value
of `haskell-literate-default' is used.")
(put 'haskell-literate 'safe-local-variable 'symbolp)

;; Default literate style for ambiguous literate buffers.
(defcustom haskell-literate-default 'bird
  "Default value for `haskell-literate'.
Used if the style of a literate buffer is ambiguous.  This variable should
be set to the preferred literate style."
  :group 'haskell
  :type '(choice (const bird) (const tex) (const nil)))

(defvar haskell-mode-map
  (let ((map (make-sparse-keymap)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Editing-specific commands
    (define-key map (kbd "C-c C-,") 'haskell-mode-format-imports)
    (define-key map [remap delete-indentation] 'haskell-delete-indentation)
    (define-key map (kbd "C-c C-l") 'haskell-mode-enable-process-minor-mode)
    (define-key map (kbd "C-c C-b") 'haskell-mode-enable-process-minor-mode)
    (define-key map (kbd "C-c C-v") 'haskell-mode-enable-process-minor-mode)
    (define-key map (kbd "C-c C-t") 'haskell-mode-enable-process-minor-mode)
    (define-key map (kbd "C-c C-i") 'haskell-mode-enable-process-minor-mode)
    (define-key map (kbd "C-c C-s") 'haskell-mode-toggle-scc-at-point)
    map)
  "Keymap used in `haskell-mode'.")

(defun haskell-mode-enable-process-minor-mode ()
  "Tell the user to choose a minor mode for process interaction."
  (interactive)
  (error "Run `C-h f haskell-mode` for instruction how to setup a Haskell interaction mode."))

(easy-menu-define haskell-mode-menu haskell-mode-map
  "Menu for the Haskell major mode."
  ;; Suggestions from Pupeno <pupeno@pupeno.com>:
  ;; - choose the underlying interpreter
  ;; - look up docs
  `("Haskell"
    ["Indent line" indent-according-to-mode]
    ["(Un)Comment region" comment-region mark-active]
    "---"
    ["Load tidy core" ghc-core-create-core]
    "---"
    ["Customize" (customize-group 'haskell)]
    ))

(defun haskell-syntax-propertize (begin end)
  (save-excursion
    (when haskell-literate
      (goto-char begin)
      ;; Algorithm (first matching rule wins):
      ;; - current line is latex code if previous non-empty line was
      ;;   latex code or was \begin{code} and current line is not
      ;;   \end{code}
      ;; - current line is bird code if it starts with >
      ;; - else literate comment
      (let ((previous-line-latex-code
             (catch 'return
               (save-excursion
                 (when (= (forward-line -1) 0)
                   (while (looking-at-p "^[\t ]*$")
                     (unless (= (forward-line -1) 0)
                       (throw 'return nil)))
                   (or (and (not (equal (eval-when-compile (string-to-syntax "<")) (syntax-after (point))))
                            (not (looking-at-p "^>")))
                       (looking-at-p "^\\\\begin{code}[\t ]*$")))))))
        (while (< (point) end)
          (unless (looking-at-p "^[\t ]*$")
            (if previous-line-latex-code
                (when (looking-at-p "^\\\\end{code}[\t ]*$")
                  (put-text-property (point) (1+ (point)) 'syntax-table (eval-when-compile (string-to-syntax "<")))
                  (setq previous-line-latex-code nil)
                  ;; continue latex-code
                  )
              (if (looking-at-p "^>")
                  ;; this is a whitespace
                  (put-text-property (point) (1+ (point)) 'syntax-table (eval-when-compile (string-to-syntax "-")))
                ;; this is a literate comment
                (progn
                  (put-text-property (point) (1+ (point)) 'syntax-table (eval-when-compile (string-to-syntax "<")))
                  (when (looking-at-p "^\\\\begin{code}[\t ]*$")
                    (setq previous-line-latex-code t))))))
          (forward-line 1))))

    (goto-char begin)
    (let ((case-fold-search nil)
          (ppss (syntax-ppss)))
      ;; If inside a comment
      (when (nth 4 ppss)
        ;; go to the end of a comment, there is nothing to see inside
        ;; a comment so we might as well just skip over it
        ;; immediately
        (setq ppss (parse-partial-sexp (point) (point-max) nil nil ppss
                                       'syntax-table)))
      ;; character address of start of comment or string
      (when (nth 8 ppss)
        ;; go to the beginning of a comment or string
        (goto-char (nth 8 ppss))
        (when (eq ?| (nth 3 ppss))
          ;; if this is a quasi quote we need to backtrack even more
          ;; to the opening bracket
          (skip-chars-backward "^[")
          (goto-char (1- (point)))))

      (while (< (point) end)
        (let
            ((token-kind (haskell-lexeme-looking-at-token-raw)))

          (pcase token-kind
           (`qsymid
            (let ((lexeme (haskell-lexeme-classify-by-first-char (char-after (match-beginning 1)))))
              (when (or (eq lexeme 'varsym)
                        (eq lexeme 'consym))
                ;; we have to neutralize potential comments here
                (put-text-property (match-beginning 1) (match-end 1) 'syntax-table (eval-when-compile (string-to-syntax "."))))))
           (`number
            (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (eval-when-compile (string-to-syntax "w"))))
           (`char
            (save-excursion
              (goto-char (match-beginning 2))
              (let ((limit (match-end 2)))
                (save-match-data
                  (while (re-search-forward "\"" limit t)
                    (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (eval-when-compile (string-to-syntax "."))))))
              ;; Place a generic string delimeter only when an open
              ;; quote is closed by end-of-line Emacs acts strangely
              ;; when a generic delimiter is not closed so in case
              ;; string ends at the end of the buffer we will use
              ;; plain string
              (if (and (not (match-beginning 3))
                       (not (eq (match-end 2) (point-max))))
                  (progn
                    (put-text-property (match-beginning 1) (match-end 1) 'syntax-table (eval-when-compile (string-to-syntax "|")))
                    (put-text-property (match-end 2 ) (1+ (match-end 2)) 'syntax-table (eval-when-compile (string-to-syntax "|"))))
                (put-text-property (match-beginning 1) (match-end 1) 'syntax-table (eval-when-compile (string-to-syntax "\"")))
                (unless (eq (match-end 2) (point-max))
                  (put-text-property (match-end 2 ) (1+ (match-end 2)) 'syntax-table (eval-when-compile (string-to-syntax "\"")))))))
           (`string
            (save-excursion
              (goto-char (match-beginning 2))
              (let ((limit (match-end 2)))
                (save-match-data
                  (while (re-search-forward "\"" limit t)
                    (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (eval-when-compile (string-to-syntax "."))))))
              ;; Place a generic string delimeter only when an open
              ;; quote is closed by end-of-line Emacs acts strangely
              ;; when a generic delimiter is not closed so in case
              ;; string ends at the end of the buffer we will use
              ;; plain string
              (unless (or (match-beginning 3)
                          (eq (match-end 2) (point-max)))
                (put-text-property (match-beginning 1) (match-end 1) 'syntax-table (eval-when-compile (string-to-syntax "|")))
                (put-text-property (match-end 2 ) (1+ (match-end 2)) 'syntax-table (eval-when-compile (string-to-syntax "|"))))))
           (`template-haskell-quasi-quote
            (put-text-property (match-beginning 2) (match-end 2) 'syntax-table (eval-when-compile (string-to-syntax "\"")))
            (when (match-beginning 4)
              (put-text-property (match-beginning 4) (match-end 4) 'syntax-table (eval-when-compile (string-to-syntax "\""))))
            (save-excursion
              (goto-char (match-beginning 3))
              (let ((limit (match-end 3)))
                (save-match-data
                  (while (search-forward "\"" limit t)
                    (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (eval-when-compile (string-to-syntax ".")))))))))
          (if token-kind
              (goto-char (match-end 0))
            (goto-char end)))))))

(defun haskell-ident-at-point ()
  "Return the identifier near point going backward or nil if none found.
May return a qualified name."
  (let ((reg (haskell-ident-pos-at-point)))
    (when reg
      (buffer-substring-no-properties (car reg) (cdr reg)))))

(defun haskell-spanable-pos-at-point ()
  "Like `haskell-ident-pos-at-point', but includes any surrounding backticks."
  (save-excursion
    (let ((pos (haskell-ident-pos-at-point)))
      (when pos
        (cl-destructuring-bind (start . end) pos
          (if (and (eq ?` (char-before start))
                   (eq ?` (char-after end)))
              (cons (- start 1) (+ end 1))
            (cons start end)))))))

(defun haskell-ident-pos-at-point ()
  "Return the span of the identifier near point going backward.
Returns nil if no identifier found or point is inside string or
comment.  May return a qualified name."
  (unless (nth 8 (syntax-ppss))
    ;; Do not handle comments and strings
    (let (start end)
      ;; Initial point position is non-deterministic, it may occur anywhere
      ;; inside identifier span, so the approach is:
      ;; - first try go left and find left boundary
      ;; - then try go right and find right boundary
      ;;
      ;; In both cases assume the longest path, e.g. when going left take into
      ;; account than point may occur at the end of identifier, when going right
      ;; take into account that point may occur at the beginning of identifier.
      ;;
      ;; We should handle `.` character very careful because it is heavily
      ;; overloaded.  Examples of possible cases:
      ;; Control.Monad.>>=  -- delimiter
      ;; Control.Monad.when -- delimiter
      ;; Data.Aeson..:      -- delimiter and operator symbol
      ;; concat.map         -- composition function
      ;; .?                 -- operator symbol
      (save-excursion
        ;; First, skip whitespace if we're on it, moving point to last
        ;; identifier char.  That way, if we're at "map ", we'll see the word
        ;; "map".
        (when (and (eolp)
                   (not (bolp)))
          (backward-char))
        (when (and (not (eobp))
                   (eq (char-syntax (char-after)) ? ))
          (skip-chars-backward " \t")
          (backward-char))
        ;; Now let's try to go left.
        (save-excursion
          (if (not (haskell-mode--looking-at-varsym))
              ;; Looking at non-operator char, this is quite simple
              (progn
                (skip-syntax-backward "w_")
                ;; Remember position
                (setq start (point)))
            ;; Looking at operator char.
            (while (and (not (bobp))
                        (haskell-mode--looking-at-varsym))
              ;; skip all operator chars backward
              (setq start (point))
              (backward-char))
            ;; Extra check for case when reached beginning of the buffer.
            (when (haskell-mode--looking-at-varsym)
              (setq start (point))))
          ;; Slurp qualification part if present.  If identifier is qualified in
          ;; case of non-operator point will stop before `.` dot, but in case of
          ;; operator it will stand at `.` delimiting dot.  So if we're looking
          ;; at `.` let's step one char forward and try to get qualification
          ;; part.
          (goto-char start)
          (when (looking-at-p (rx "."))
            (forward-char))
          (if-let (pos (haskell-mode--skip-qualification-backward))
              (setq start pos)))
        ;; Finally, let's try to go right.
        (save-excursion
          ;; Try to slurp qualification part first.
          (skip-syntax-forward "w_")
          (setq end (point))
          (while (and (looking-at-p (rx "." upper))
                      (not (zerop (progn (forward-char)
                                         (skip-syntax-forward "w_")))))
            (setq end (point)))
          ;; If point was at non-operator we already done, otherwise we need an
          ;; extra check.
          (while (haskell-mode--looking-at-varsym)
            (forward-char)
            (setq end (point))))
        (unless (= start end)
          (cons start end))))))

(defun haskell-mode--looking-at-varsym ()
  "Return t when point stands at operator symbol."
  (unless (eobp)
    (let ((lex (haskell-lexeme-classify-by-first-char (char-after))))
      (or (eq lex 'varsym)
          (eq lex 'consym)))))

(defun haskell-mode--skip-qualification-backward ()
  "Skip qualified part of identifier backward.
Expects point stands *after* delimiting dot.
Returns beginning position of qualified part or nil if no qualified part found."
  (unless (and (bobp)
               (looking-at-p (rx bol)))
    (let ((case-fold-search nil)
          pos)
      (while (and (eq (char-before) ?.)
                  (progn (backward-char)
                         (not (zerop (skip-syntax-backward "w'"))))
                  (skip-syntax-forward "'")
                  (looking-at-p "[[:upper:]]"))
        (setq pos (point)))
      pos)))

(defun haskell-delete-indentation (&optional arg)
  "Like `delete-indentation' but ignoring Bird-style \">\".
Prefix ARG is handled as per `delete-indentation'."
  (interactive "*P")
  (let ((fill-prefix (or fill-prefix (if (eq haskell-literate 'bird) ">"))))
    (delete-indentation arg)))

(defvar electric-pair-inhibit-predicate)
(declare-function electric-pair-default-inhibit "elec-pair")
(defun haskell-mode--inhibit-bracket-inside-comment-or-default (ch)
  "An `electric-pair-mode' inhibit function for character CH."
  (or (nth 4 (syntax-ppss))
      (funcall #'electric-pair-default-inhibit ch)))

;; The main mode functions
;;;###autoload
(define-derived-mode haskell-mode prog-mode "Haskell"
  "Major mode for editing Haskell programs.

\\<haskell-mode-map>

Literate Haskell scripts are supported via `haskell-literate-mode'.
The variable `haskell-literate' indicates the style of the script in the
current buffer.  See the documentation on this variable for more details.

Use `haskell-version' to find out what version of Haskell mode you are
currently using.

Additional Haskell mode modules can be hooked in via `haskell-mode-hook'.

Indentation modes:

    `haskell-indentation-mode', Kristof Bastiaensen, Gergely Risko
      Intelligent semi-automatic indentation Mk2

    `haskell-indent-mode', Guy Lapalme
      Intelligent semi-automatic indentation.

Interaction modes:

    `interactive-haskell-mode'
      Interact with per-project GHCi processes through a REPL and
      directory-aware sessions.

Other modes:

    `haskell-decl-scan-mode', Graeme E Moss
      Scans top-level declarations, and places them in a menu.

    `haskell-doc-mode', Hans-Wolfgang Loidl
      Echoes types of functions or syntax of keywords when the cursor is idle.

To activate a minor-mode, simply run the interactive command. For
example, `M-x haskell-doc-mode'. Run it again to disable it.

To enable a mode for every haskell-mode buffer, add a hook in
your Emacs configuration. To do that you can customize
`haskell-mode-hook' or add lines to your .emacs file. For
example, to enable `interactive-haskell-mode', use the following:

    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

Minor modes that work well with `haskell-mode':

- `smerge-mode': show and work with diff3 conflict markers used
  by git, svn and other version control systems."
  :group 'haskell
  (when (version< emacs-version "25.1")
    (error "haskell-mode requires at least Emacs 25.1"))

  ;; paragraph-{start,separate} should treat comments as paragraphs as well.
  (setq-local paragraph-start (concat " *{-\\([^#]\\|$\\)\\| *-- |\\|" page-delimiter))
  (setq-local paragraph-separate (concat " *$\\| *\\({-\\([^#]\\|$\\)\\|\\([^#]\\|^\\)-}\\) *$\\|" page-delimiter))
  (setq-local fill-paragraph-function 'haskell-fill-paragraph)
  ;; (setq-local adaptive-fill-function 'haskell-adaptive-fill)
  (setq-local comment-start "--")
  (setq-local comment-padding 1)
  (setq-local comment-start-skip "[-{]-[ \t]*")
  (setq-local comment-end "")
  (setq-local comment-end-skip "[ \t]*\\(-}\\|\\s>\\)")
  (setq-local forward-sexp-function #'haskell-forward-sexp)
  (setq-local parse-sexp-ignore-comments nil)
  (setq-local syntax-propertize-function #'haskell-syntax-propertize)

  ;; Set things up for imenu.
  (setq-local imenu-create-index-function 'haskell-ds-create-imenu-index)
  ;; Set things up for font-lock.
  (setq-local font-lock-defaults
              '((haskell-font-lock-keywords)
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . haskell-syntactic-face-function)
                ;; Get help from font-lock-syntactic-keywords.
                (parse-sexp-lookup-properties . t)
                (font-lock-extra-managed-props . (composition haskell-type))))
  ;; Preprocessor definitions can have backslash continuations
  (setq-local font-lock-multiline t)
  ;; Haskell's layout rules mean that TABs have to be handled with extra care.
  ;; The safer option is to avoid TABs.  The second best is to make sure
  ;; TABs stops are 8 chars apart, as mandated by the Haskell Report.  --Stef
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 8)
  (setq-local comment-auto-fill-only-comments t)
  ;; Haskell is not generally suitable for electric indentation, since
  ;; there is no unambiguously correct indent level for any given line.
  (when (boundp 'electric-indent-inhibit)
    (setq electric-indent-inhibit t))

  ;; dynamic abbrev support: recognize Haskell identifiers
  ;; Haskell is case-sensitive language
  (setq-local dabbrev-case-fold-search nil)
  (setq-local dabbrev-case-distinction nil)
  (setq-local dabbrev-case-replace nil)
  (setq-local dabbrev-abbrev-char-regexp "\\sw\\|[.]")
  (setq haskell-literate nil)
  (add-hook 'before-save-hook 'haskell-mode-before-save-handler nil t)
  ;; provide non-interactive completion function
  (add-hook 'completion-at-point-functions
            'haskell-completions-completion-at-point
            nil
            t)

  ;; Avoid Emacs 25 bug with electric-pair inside comments
  (when (eq 25 emacs-major-version)
    (setq-local electric-pair-inhibit-predicate 'haskell-mode--inhibit-bracket-inside-comment-or-default))

  (haskell-indentation-mode))

(defcustom haskell-mode-hook '(haskell-indentation-mode interactive-haskell-mode)
  "List of functions to run after `haskell-mode' is enabled.

Use to enable minor modes coming with `haskell-mode' or run an
arbitrary function.

Note that  `haskell-indentation-mode' and `haskell-indent-mode' should not be
run at the same time."
  :group 'haskell
  :type 'hook
  :options '(capitalized-words-mode
             flyspell-prog-mode
             haskell-decl-scan-mode
             haskell-indent-mode
             haskell-indentation-mode
             imenu-add-menubar-index
             interactive-haskell-mode
             turn-on-haskell-unicode-input-method))

(defun haskell-fill-paragraph (justify)
  (save-excursion
    ;; Fill paragraph should only work in comments.
    ;; The -- comments are handled properly by default
    ;; The {- -} comments need some extra love.
    (let* ((syntax-values (syntax-ppss))
           (comment-num (nth 4 syntax-values)))
      (cond
       ((eq t comment-num)
        ;; standard fill works wonders inside a non-nested comment
        (fill-comment-paragraph justify))

       ((integerp comment-num)
        ;; we are in a nested comment. lets narrow to comment content
        ;; and use plain paragraph fill for that
        (let* ((comment-start-point (nth 8 syntax-values))
               (comment-end-point
                (save-excursion
                  (goto-char comment-start-point)
                  (forward-sexp)
                  ;; Find end of any comment even if forward-sexp
                  ;; fails to find the right braces.
                  (backward-char 3)
                  (re-search-forward "[ \t]?-}" nil t)
                  (match-beginning 0)))
               (fill-start (+ 2 comment-start-point))
               (fill-end comment-end-point)
               (fill-paragraph-handle-comment nil))
          (save-restriction
            (narrow-to-region fill-start fill-end)
            (fill-paragraph justify)
            ;; If no filling happens, whatever called us should not
            ;; continue with standard text filling, so return t
            t)))
       ((eolp)
        ;; do nothing outside of a comment
        t)
       (t
        ;; go to end of line and try again
        (end-of-line)
        (haskell-fill-paragraph justify))))))


;; (defun haskell-adaptive-fill ()
;;   ;; We want to use "--  " as the prefix of "-- |", etc.
;;   (let* ((line-end (save-excursion (end-of-line) (point)))
;;          (line-start (point)))
;;     (save-excursion
;;       (unless (in-comment)
;;         ;; Try to find the start of a comment. We only fill comments.
;;         (search-forward-regexp comment-start-skip line-end t))
;;       (when (in-comment)
;;         (let ();(prefix-start (point)))
;;           (skip-syntax-forward "^w")
;;           (make-string (- (point) line-start) ?\s))))))

(defvar haskell-paredit-fixed-syntax-table
  (let ((tbl (copy-syntax-table haskell-mode-syntax-table)))
    (modify-syntax-entry ?` "." tbl)
    tbl)
  "Special syntax table for Haskell makes paredit work with `...`
blocks. Ultimately paredit will use ‘scan-sexps’ which behaves
funny when there are characters with $ syntax around.")

;;;###autoload
(defun haskell-forward-sexp-no-pairing (&optional arg)
  (with-syntax-table haskell-paredit-fixed-syntax-table
    (haskell-forward-sexp arg)))

;;;###autoload
(defun haskell-forward-sexp (&optional arg)
  "Haskell specific version of `forward-sexp'.

Move forward across one balanced expression (sexp).  With ARG, do
it that many times.  Negative arg -N means move backward across N
balanced expressions.  This command assumes point is not in a
string or comment.

If unable to move over a sexp, signal `scan-error' with three
arguments: a message, the start of the obstacle (a parenthesis or
list marker of some kind), and end of the obstacle."
  (interactive "^p")
  ;; If we’re not derived then lexeme parsing doesn’t work.
  (cl-assert (or (derived-mode-p 'haskell-mode) (derived-mode-p 'haskell-ts-mode))
             nil
             "The ‘haskell-forward-sexp’ function doesn’t work in modes not derived from ‘haskell-mode’ or ‘haskell-ts-mode’. Current mode: %s"
             major-mode)
  (or arg (setq arg 1))
  (if (< arg 0)
      (while (< arg 0)
        (skip-syntax-backward "->")
        ;; Navigate backwards using plain `backward-sexp', assume that it
        ;; skipped over at least one Haskell expression, and jump forward until
        ;; last possible point before the starting position. If applicable,
        ;; `scan-error' is signalled by `backward-sexp'.
        (let ((end (point))
              (forward-sexp-function nil))
          (backward-sexp)
          (let ((cur (point)))
            (while (< (point) end)
              (setf cur (point))
              (haskell-forward-sexp)
              (skip-syntax-forward "->"))
            (goto-char cur)))
        (cl-incf arg))
    (save-match-data
      (let ((case-fold-search nil)
            (continue t))
        (while (and (> arg 0)
                    continue)
          (if-let (token-kind (haskell-lexeme-looking-at-token-raw))
              (cond
                ((or (eq token-kind 'comment)
                     (eq token-kind 'nested-comment))
                 (goto-char (match-end 0))
                 (unless parse-sexp-ignore-comments
                   (cl-decf arg)))
                ((eq token-kind 'special)
                 (let ((str (char-after (match-beginning 0))))
                   (cond ((memq str '(?\( ?\[ ?\{))
                          (goto-char (or (scan-sexps (point) 1)
                                         (buffer-end 1))))
                         ((memq str '(?\) ?\] ?\}))
                          (signal 'scan-error (list "Containing expression ends prematurely."
                                                    (match-beginning 0)
                                                    (match-end 0))))
                         (t (goto-char (match-end 0)))))
                 (cl-decf arg))
                (t
                 (goto-char (match-end 0))
                 (cl-decf arg)))
            (setf continue nil)))))))

;;;###autoload
(define-derived-mode haskell-literate-mode haskell-mode "LitHaskell"
  "As `haskell-mode' but for literate scripts."
  (setq haskell-literate
        (save-excursion
          (goto-char (point-min))
          (cond
           ((re-search-forward "^\\\\\\(begin\\|end\\){code}$" nil t) 'tex)
           ((re-search-forward "^>" nil t) 'bird)
           (t haskell-literate-default))))
  (if (eq haskell-literate 'bird)
      ;; fill-comment-paragraph isn't much use there, and even gets confused
      ;; by the syntax-table text-properties we add to mark the first char
      ;; of each line as a comment-starter.
      (setq-local fill-paragraph-handle-comment nil))
  (setq-local mode-line-process '("/" (:eval (symbol-name haskell-literate)))))

;;;###autoload
(define-obsolete-function-alias 'literate-haskell-mode 'haskell-literate-mode "2020-04")

;;;###autoload
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))


(defcustom haskell-check-command "hlint"
  "*Command used to check a Haskell file."
  :group 'haskell
  :type '(choice (const "hlint")
                 (const "ghc -fno-code")
                 (string :tag "Other command")))

(defvar haskell-saved-check-command nil
  "Internal use.")

;; Like Python.  Should be abstracted, sigh.
(defun haskell-check (command)
  "Check a Haskell file (default current buffer's file).
Runs COMMAND, a shell command, as if by `compile'.
See `haskell-check-command' for the default."
  (interactive
   (list (read-string "Checker command: "
                      (or haskell-saved-check-command
                          (concat haskell-check-command " "
                                  (let ((name (buffer-file-name)))
                                    (if name
                                        (file-name-nondirectory name))))))))
  (setq haskell-saved-check-command command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compilation-start command))

;; This function was renamed and deprecated, but we want clean
;; byte compilation in all versions.
(defalias 'haskell-flymake-create-temp-buffer-copy
  (if (fboundp 'flymake-proc-init-create-temp-buffer-copy)
      'flymake-proc-init-create-temp-buffer-copy
    'flymake-init-create-temp-buffer-copy))

(defun haskell-flymake-init ()
  "Flymake init function for Haskell."
  (when haskell-saved-check-command
    (let ((checker-elts (split-string haskell-saved-check-command)))
      (list (car checker-elts)
            (append (cdr checker-elts)
                    (list (haskell-flymake-create-temp-buffer-copy
                           'flymake-create-temp-inplace)))))))

(add-to-list 'flymake-proc-allowed-file-name-masks '("\\.l?hs\\'" haskell-flymake-init))

(defun haskell-mode-format-imports ()
  "Format the imports by aligning and sorting them."
  (interactive)
  (let ((col (current-column-fixed)))
    (haskell-sort-imports)
    (haskell-align-imports)
    (move-to-column col)))

(declare-function haskell-mode-stylish-buffer "haskell-commands")

(defun haskell-mode-before-save-handler ()
  "Function that will be called before buffer's saving."
  (when haskell-stylish-on-save
    (ignore-errors (haskell-mode-stylish-buffer))))

;; From Bryan O'Sullivan's blog:
;; http://www.serpentine.com/blog/2007/10/09/using-emacs-to-insert-scc-annotations-in-haskell-code/
(defun haskell-mode-try-insert-scc-at-point ()
  "Try to insert an SCC annotation at point.  Return true if
successful, nil otherwise."
  (if (or (looking-at-p "\\b\\|[ \t]\\|$")
          ;; Allow SCC if point is on a non-letter with whitespace to the left
          (and (not (bolp))
               (save-excursion
                 (forward-char -1)
                 (looking-at-p "[ \t]"))))
      (let ((space-at-point (looking-at-p "[ \t]")))
        (unless (and (not (bolp)) (save-excursion
                                    (forward-char -1)
                                    (looking-at-p "[ \t]")))
          (insert " "))
        (insert "{-# SCC \"\" #-}")
        (unless space-at-point
          (insert " "))
        (forward-char (if space-at-point -5 -6))
        t )))

(defun haskell-mode-insert-scc-at-point ()
  "Insert an SCC annotation at point."
  (interactive)
  (unless (haskell-mode-try-insert-scc-at-point)
    (error "Not over an area of whitespace")))

(make-obsolete
 'haskell-mode-insert-scc-at-point
 'haskell-mode-toggle-scc-at-point
 "2015-11-11")

(defun haskell-mode-try-kill-scc-at-point ()
  "Try to kill an SCC annotation at point.  Return true if
successful, nil otherwise."
  (save-excursion
    (let ((old-point (point))
          (scc "\\({-#[ \t]*SCC \"[^\"]*\"[ \t]*#-}\\)[ \t]*"))
      (while (not (or (looking-at scc) (bolp)))
        (forward-char -1))
      (if (and (looking-at scc)
               (<= (match-beginning 1) old-point)
               (> (match-end 1) old-point))
          (progn (kill-region (match-beginning 0) (match-end 0))
                 t)))))

;; Also Bryan O'Sullivan's.
(defun haskell-mode-kill-scc-at-point ()
  "Kill the SCC annotation at point."
  (interactive)
  (unless (haskell-mode-try-kill-scc-at-point)
    (error "No SCC at point")))

(make-obsolete
 'haskell-mode-kill-scc-at-point
 'haskell-mode-toggle-scc-at-point
 "2015-11-11")

(defun haskell-mode-toggle-scc-at-point ()
  "If point is in an SCC annotation, kill the annotation.  Otherwise, try to insert a new annotation."
  (interactive)
  (unless (haskell-mode-try-kill-scc-at-point)
    (unless (haskell-mode-try-insert-scc-at-point)
      (error "Could not insert or remove SCC"))))

(defun haskell-guess-module-name-from-file-name (file-name)
  "Guess the module name from FILE-NAME.

Based on given FILE-NAME this function tries to find path
components that look like module identifiers and composes full
module path using this information. For example:

    /Abc/Def/Xyz.lhs => Abc.Def.Xyz
    /Ab-c/Def/Xyz.lhs => Def.Xyz
    src/Abc/Def/Xyz.hs => Abc.Def.Xyz
    c:\\src\\Abc\\Def\\Xyz.hs => Abc.Def.Xyz
    nonmodule.txt => nil

This function usually will be used with `buffer-file-name':

    (haskell-guess-module-name-from-file-name (buffer-file-name))"

  (let* ((file-name-sans-ext (file-name-sans-extension file-name))
         (components (cl-loop for part
                             in (reverse (split-string file-name-sans-ext "/"))
                             while (let ((case-fold-search nil))
                                     (string-match (concat "^" haskell-lexeme-modid "$") part))
                             collect part)))
    (when components
      (mapconcat 'identity (reverse components) "."))))

(defun haskell-guess-module-name ()
  "Guess the current module name of the buffer.
Uses `haskell-guess-module-name-from-file-name'."
  (haskell-guess-module-name-from-file-name (buffer-file-name)))

(defvar haskell-auto-insert-module-format-string
  "-- | \n\nmodule %s where\n\n"
  "Template string that will be inserted in new haskell buffers via `haskell-auto-insert-module-template'.")

(defun haskell-auto-insert-module-template ()
  "Insert a module template for the newly created buffer."
  (interactive)
  (when (and (= (point-min)
                (point-max))
             (buffer-file-name))
    (insert (format haskell-auto-insert-module-format-string (haskell-guess-module-name-from-file-name (buffer-file-name))))
    (goto-char (point-min))
    (end-of-line)))

;; Provide ourselves:
(provide 'haskell-mode)
;;; haskell-mode.el ends here
