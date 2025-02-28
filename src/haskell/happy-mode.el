;; happy-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 12 May 2014
;; Description:
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; Commentary:
;; This mode is loosely-based on old bison-mode http://ftp.sunet.se/pub/gnu/emacs-lisp/incoming/bison-mode.el
;; that's not part of emacs.
;;
;; Requirements:
;; This mode depends on of poly-mode.

(eval-when-compile
  (require 'cl-lib))

(require 'current-column-fixed)

(require 'haskell-mode)
(require 'cc-mode)

(require 'polymode)
(require 'alex-happy-utils)

;;;; happy mode

(defconst happy-colon-column 16 "\
*The column in which to place a colon separating a token from its definition.")

(defvar happy-grammar-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap ";"                     #'electric-happy-semi)
    (define-key keymap ":"                     #'electric-happy-colon)
    (define-key keymap "|"                     #'electric-happy-colon)
    (define-key keymap (kbd "<backspace>")     #'backward-delete-char-untabify)
    (define-key keymap (kbd "<tab>")           #'happy-indent-command)
    (define-key keymap (kbd "S-<tab>")         #'happy-dedent-command)
    (define-key keymap (kbd "S-<iso-lefttab>") #'happy-dedent-command)
    keymap)
  "Keymap used in happy mode.")

(defvar happy-grammar-mode-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?\{ "(}  " tbl)
    (modify-syntax-entry ?\} "){  " tbl)
    (modify-syntax-entry ?\\ "\\  " tbl)
    (modify-syntax-entry ?\' "."    tbl)
    (modify-syntax-entry ?\" "\""   tbl)
    (modify-syntax-entry ?\: "."    tbl)
    (modify-syntax-entry ?\; "."    tbl)
    (modify-syntax-entry ?-  ". 12" tbl)
    (modify-syntax-entry ?\n ">"    tbl)
    tbl)
  "Syntax table in use in happy-mode buffers.")

(defconst happy-mode-rule-start-regexp
  (rx-let ((ws (* (any ?\s ?\t)))
           (ws-nl (* (any ?\s ?\t ?\n ?\r))))
    (rx bol
        ws
        (+ (or (syntax word)
               (syntax symbol)))
        ws
        (? "::"
           ws
           "{"
           (* (not (any ?\})))
           "}"
           ws-nl)
        ":")))

(defconst happy-mode-rule-start-or-body-regexp
  "^[ \t]*\\(?:\\(?:\\s_\\|\\sw\\)+[ \t]*:\\||\\)")

(defvar happy-grammar-mode-font-lock-keywords
  `((,(rx (or "%name"
              "%tokentype"
              "%error"
              "%token"
              "%%"
              "%left"
              "%right"
              "%nonassoc"
              "%monad"
              "%lexer"
              "%attribute"
              "%attributetype"
              "%partial"
              "%importedidentity"
              "%expect"
              "%prec"))
     (0 'font-lock-keyword-face))
    (,(rx bol
          (group
           (regexp "[a-z0-9_]")
           (* (regexp "[a-zA-Z0-9_]")))
          (* (any ?\s ?\t ?\n ?\r))
          ":")
     (1 'font-lock-function-name-face))
    (,(rx bow
          (group
           (+ (regexp "[A-Z0-9_]")))
          eow)
     (0 'font-lock-constant-face))
    (,(rx (or ":"
              "|"
              ";"
              (seq "'"
                   (+ (not (any ?\n ?\')))
                   "'")))
     (0 'font-lock-negation-char-face))
    (,(rx "$" (or (+ digit) "$"))
     (0 'font-lock-variable-name-face)))
  "Highlight definitions of happy distinctive constructs for font-lock.")

(define-derived-mode happy-grammar-mode prog-mode "Happy"
  "Major mode for editing Happy files."
  (setq-local font-lock-defaults
              '(happy-grammar-mode-font-lock-keywords
                nil ;; perform syntactic fontification
                nil ;; do not ignore case
                nil ;; no special syntax provided
                ))

  (setq-local paragraph-start (concat "^$\\|" page-delimiter)
              paragraph-separate paragraph-start
              paragraph-ignore-fill-prefix t
              indent-line-function 'happy-indent-line
              require-final-newline t
              comment-start "--"
              comment-end ""
              comment-column 32
              comment-start-skip "--+ *"
              parse-sexp-ignore-comments t
              ;; selective-display t
              ;; selective-display-ellipses t
              )

  (make-local-variable 'block-indent-level)
  (make-local-variable 'auto-fill-hook)

  (setq-local parse-sexp-lookup-properties t))

(defun happy-in-literal-context? (start end)
  "Check whether end is in literal context."
  (let* ((state (parse-partial-sexp start end))
         (inside-string? (nth 3 state))
         (inside-comment? (nth 4 state))
         (following-quote-character? (nth 5 state)))
    (or inside-string?
        inside-comment?
        following-quote-character?)))

(defun electric-happy-colon (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (cond ((happy-in-literal-context? (save-excursion
                                      (save-match-data
                                        (if (re-search-backward
                                             "^[ \t]*\\(\\s_\\|\\sw\\)+[ \t]*:"
                                             nil
                                             t)
                                          (- (match-end 0) 1)
                                          (point-min))))
                                    (point))
         (self-insert-command (prefix-numeric-value arg)))
        ((and (not arg)
              (eolp))
         (happy-indent-line)
         (and c-auto-newline
              (eq last-command-event ?\|)
              (save-excursion
                (beginning-of-line)
                (not (looking-at-p "[ \t]*$")))
              (newline))
         (delete-horizontal-space)
         (happy-mode-indent-to! happy-colon-column)
         (insert last-command-event)
         (insert " "))
        (t
         (self-insert-command (prefix-numeric-value arg)))))

(defun electric-happy-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (electric-happy-terminator arg))

;; (defun electric-happy-per (arg)
;;   "Insert character and correct line's indentation."
;;   (interactive "P")
;;   (let ((state (parse-partial-sexp
;;                 (save-excursion
;;                   (save-match-data
;;                     (if (re-search-backward
;;                          "^[ \t]*\\(\\s_\\|\\sw\\)+[ \t]*:"
;;                          nil t)
;;                       (- (match-end 0) 1)
;;                       (point-min))))
;;                 (point))))
;;     (if (and (not arg)
;;              (eolp)
;;              (not (eq (preceding-char) ?%))
;;              (not (or (nth 3 state) (nth 4 state) (nth 5 state))))
;;       (if (not (save-excursion
;;                  (skip-chars-backward " \t")
;;                  (bolp)))
;;         (indent-to bison-percent-column)
;;         (delete-region (save-excursion
;;                          (beginning-of-line)
;;                          (point))
;;                        (point))))
;;     (self-insert-command (prefix-numeric-value arg))))

(defun electric-happy-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if (happy-in-literal-context? (save-excursion
                                   (save-match-data
                                     (if (re-search-backward
                                          "^[ \t]*\\(\\s_\\|\\sw\\)+[ \t]*:"
                                          nil
                                          t)
                                       (- (match-end 0) 1)
                                       (point-min))))
                                 (point))
    (self-insert-command (prefix-numeric-value arg))
    (if (and (not arg) (eolp)
             (not (save-excursion
                    (beginning-of-line)
                    (skip-chars-forward " \t")
                    (= (following-char) ?%))))
      (progn
        (and c-auto-newline
             (progn
               (if (save-excursion
                     (beginning-of-line)
                     (not (looking-at-p "[ \t]*$")))
                 (newline))
               (happy-indent-line)
               (backward-delete-char-untabify 2)))
        (insert last-command-event)
        (happy-indent-line)
        (and c-auto-newline
             (progn
               (newline)
               (setq insertpos (- (point) 2))
               (happy-indent-line)))
        (save-excursion
          (if insertpos (goto-char (1+ insertpos)))
          (delete-char -1))))
    (if insertpos
      (save-excursion
        (goto-char insertpos)
        (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun happy-indent-command (&optional whole-exp)
  "Indent current line as Happy (Bison) code, or in some cases insert a tab character.
If c-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  (interactive "P")
  (if whole-exp
    (let ((shift-amount (or (happy-indent-line)
                            0))
          beg end)
      (save-excursion
        (save-match-data
          (if c-tab-always-indent
            (beginning-of-line))
          (setq beg (point))
          (re-search-forward ";\\|^%%" nil t)
          (when (save-excursion
                  (beginning-of-line)
                  (looking-at-p "%%"))
            (forward-line -1)
            (end-of-line))
          (setq end (point))
          (goto-char beg)
          (forward-line 1)
          (setq beg (point))))
      (when (> end beg)
        (indent-code-rigidly beg end shift-amount "%")))
    (if (and (not c-tab-always-indent)
             (save-excursion
               (skip-chars-backward " \t")
               (not (bolp))))
      (insert-tab)
      (happy-indent-line))))

(defun happy-dedent-command ()
  (interactive)
  (happy-mode-indent-to! (max 0 (- (current-column-fixed) 4))))

(defun happy-indent-line ()
  "Indent current line as Happy (Bison) code.
Return the amount the indentation changed by."
  ;; Lines are indented if and only if a colon is found before a semicolon
  ;; while searching backward.  String-quoted characters are ignored.
  (let (indent)
    (save-excursion
      (cond
        ;; the point is either somewhere before %% or
        ;; somewhere after %% but in the literal context
        ((save-excursion
           (save-match-data
             (let ((limit (point))
                   state)
               (goto-char (point-min))
               (or (not (re-search-forward "^%%" limit t))
                   (happy-in-literal-context?
                    (save-excursion
                      (goto-char limit)
                      (if (re-search-backward
                           happy-mode-rule-start-regexp
                           nil
                           t)
                          (- (match-end 0) 1)
                        (point-min)))
                    limit)))))
         (setq indent nil))
        ((save-excursion
           (beginning-of-line)
           (looking-at-p "[ \t]*%"))
         (setq indent 0))
        (t
         (beginning-of-line)
         (if (looking-at-p happy-mode-rule-start-regexp)
             (setq indent 0)
           (progn
             (forward-line -1)
             (while (not (or (bobp)
                             (looking-at-p happy-mode-rule-start-or-body-regexp)
                             (eq (following-char) ?%)))
               (forward-line -1))
             (skip-chars-forward "^:|")
             ;; (skip-chars-forward ":| \t")
             (setq indent (current-column-fixed-uncached)))))))
    (when indent
      (happy-mode-indent-to! indent)
      (skip-chars-forward " \t"))
    indent))

;;;; happy-mode

(defun poly-happy-find-front (direction)
  (poly-alex-happy-find-front direction nil))

(define-hostmode poly-happy-hostmode
  :mode 'happy-grammar-mode)

(define-innermode poly-happy-haskell-innermode
  :mode 'haskell-mode
  :head-matcher #'poly-happy-find-front
  :tail-matcher #'poly-alex-happy-find-tail
  :head-mode 'host
  :tail-mode 'host
  :allow-nested nil
  :can-nest nil
  :protect-syntax t)

;;;###autoload (autoload 'happy-mode "happy-mode" nil t)
(define-polymode happy-mode
  :hostmode 'poly-happy-hostmode
  :innermodes '(poly-happy-haskell-innermode)
  (setq-local poly-lock-allow-background-adjustment nil))

;;;; utils

(defun happy-mode-indent-to! (col)
  "Indent current line to exactly COL'th column with spaces."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (delete-region (line-beginning-position) (point))
    (dotimes (i col)
      (insert ?\s))))

;;;;

(provide 'happy-mode)

;; happy-mode.el ends here
