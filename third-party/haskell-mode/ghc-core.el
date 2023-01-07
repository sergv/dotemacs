;;; ghc-core.el --- Syntax highlighting module for GHC Core -*- lexical-binding: t -*-

;; Copyright (C) 2010  Johan Tibell

;; Author: Johan Tibell <johan.tibell@gmail.com>

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Purpose:
;;
;; To make it easier to read GHC Core output by providing highlighting
;; and removal of commonly ignored annotations.

;;; Code:
(require 'haskell-mode)
(require 'haskell-font-lock)

(require 'haskell-regexen)
(require 'haskell-smart-operators-mode)

(defun ghc-core-clean-region (start end)
  "Remove commonly ignored annotations and namespace prefixes
in the region between START and END."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward-regexp "GHC\.[^\.]*\." nil t)
      (replace-match "" nil t))
    (goto-char (point-min))
    (while (flush-lines "^ *GblId *$" nil))
    (goto-char (point-min))
    (while (flush-lines "^ *LclId *$" nil))
    (goto-char (point-min))
    (while (flush-lines (concat "^ *\\[\\(?:Arity [0-9]+\\|NoCafRefs\\|"
                                "Str: DmdType\\|Worker \\)"
                                "\\([^]]*\\n?\\).*\\] *$") nil))
    (goto-char (point-min))
    (while (search-forward "Main." nil t) (replace-match "" nil t))))

(defun ghc-core-clean-buffer ()
  "Remove commonly ignored annotations and namespace prefixes
in the current buffer."
  (interactive)
  (ghc-core-clean-region (point-min) (point-max)))

(defvar ghc-core-mode-syntax-table
  (let ((tbl (copy-syntax-table haskell-mode-syntax-table)))
    (modify-syntax-entry ?- "_ 12" tbl)
    (modify-syntax-entry ?$ "_" tbl)
    (modify-syntax-entry ?# "_" tbl)
    (modify-syntax-entry ?. "_" tbl)
    tbl))

(defvar ghc-core-font-lock-keywords
  `(
    (,(eval-when-compile
        (concat "`\\(?:" haskell-regexen/core/opt-q/varid-or-conid "\\)`"))
     0 'haskell-operator-face)
    ("\\[\\]" 0 'haskell-constructor-face)
    (,(eval-when-compile (regexp-opt haskell-font-lock-keywords)) 0 'haskell-keyword-face)

    ;; ("(\\(,*\\|->\\))" 0 'haskell-constructor-face)

    ;; (,haskell-regexen/core/opt-q/varid
    ;;  (0 'default)
    ;;  ;; (1 'font-lock-variable-name-face)
    ;;  ;; (2 'font-lock-variable-name-face)
    ;;  )

    (,haskell-regexen/core/opt-q/conid
     (1 'default)
     (2 'haskell-constructor-face))
    (,haskell-regexen/core/opt-q/operator 0 'haskell-operator-face)
    (,(rx-let ((sign (or "-" "+"))
               (digits (regex "[0-9]")))
        (rx symbol-start
            (? sign)
            (+ digits)
            (? "."
               (+ digits))
            (? (or "e" "E")
               (? sign)
               (+ digits))
            (? (** 1 2 "#"))
            symbol-end))
     (0 'font-lock-constant-face))))

;;;###autoload
(define-derived-mode ghc-core-mode prog-mode "GHC-Core"
  "Major mode for GHC Core files."
  (setq-local comment-start "--+"
              comment-start-skip "--+ *"

              indent-tabs-mode nil
              font-lock-defaults
              '(ghc-core-font-lock-keywords
                nil ;; perform syntactic fontification (e.g. strings, comments)
                )))

(provide 'ghc-core)
;;; ghc-core.el ends here
