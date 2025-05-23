;;; isar-goal-mode.el --- Simple Isar Mode -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Mathias Fleury
;; URL: https://bitbucket.org/zmaths/isabelle2019-vsce/

;; Keywords: lisp
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and-or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; blabla

;;; Code:

(require 'pretty-ligatures)

(require 'isar-mode)

(defvar isar-goal-mode-hook nil)

(defvar isar-goal-mode-map (make-sparse-keymap)
  "Keymap for isar major mode.")

;;;###autoload
(add-to-list 'auto-mode-alist `(,(rx bos (or "*lsp-isar-output*" "*lsp-isar-state*") eos) . isar-goal-mode))

(defvar isar-goal-most-outer-keyword
  (rx bow
      (or "proof" "prove")
      eow))

(defvar isar-goal-outer-keyword
  (rx bow
      (or "goal" "subgoal" "consts" "show")
      eow))

(defvar isar-goal-inner-keyword
  "$^[:digit:]*.")

(defvar isar-goal-tactics ;; warning
  (rx bow
      (or "Introduced" "fixed" "type" "variable" "variable(s)"
          "Ambiguous" "input""produces" "parse" "trees"
          "Ignoring" "duplicate" "rewrite" "rule" "introduction"
          "elim" "intro")
      eow))

(defvar isar-goal-minor ;; information
  (rx bow
      (or "is" "Found" "termination" "order" "Proofs" "for" "inductive" "predicate"
          "Successful" "attempt" "to" "solve" "goal" "by" "exported" "rule" "this" "calculation"
          "have" "using" "Proof" "outline" "with" "cases")
      eow))

(defconst isar-goal-font-lock-keywords-1
  (list
   ;; (cons (concat "\\<" isar-goal-outer-keyword "\\>") 'font-lock-builtin-face)
   ;; 	(cons (concat "\\<" isar-goal-inner-keyword "\\>") 'font-lock-constant-face)
   ;; 	(cons (concat "\\<" isar-goal-tactics "\\>") 'font-lock-variable-name-face)
   ;; 	(cons (concat "\\<" isar-goal-most-outer-keyword "\\>") 'font-lock-preprocessor-face)
   ;; 	(cons (concat "\\<" isar-goal-minor "\\>") 'font-lock-type-face)
   ))


(defvar isar-goal-font-lock-keywords isar-goal-font-lock-keywords-1
  "Default highlighting expressions for isar mode")

(defvar isar-goal-mode-syntax-table (make-syntax-table isar-mode-syntax-table)
  "Syntax table for isar-goal-mode")


(defun isar-goal-syntax-propertize (start end)
  "Fix of syntax highlighting.

In Isar, `(*)' does not start a compent but is the multiplication sign."
  (isar-syntax-propertize start end))

;;;###autoload
(define-derived-mode isar-goal-mode prog-mode "Isar-goal"
  "Major mode for editing isar files"
  (isar--setup-font-lock! isar-control--font-lock-keywords)
  (face-remap-set-base 'lsp-isar-font-text-overview-error nil))

;;spacemacs specific function
(when (boundp 'spacemacs-jump-handlers-isar-goal-mode)
  (setq spacemacs-jump-handlers-isar-goal-mode nil))

(provide 'isar-goal-mode)
;;; isar-goal-mode.el ends here
