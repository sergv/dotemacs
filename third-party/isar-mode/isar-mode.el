;;; isar-mode.el --- Simple Isar Mode -*- lexical-binding: t -*-

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

(require 'isar-unicode-tokens)
(require 'unicode-tokens)


(defvar isar-mode-hook nil)

(defvar isar-mode-map
  (make-sparse-keymap)
  "Keymap for isar major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.thy\\'" . isar-mode))

(defvar isar-most-outer-keyword
  (regexp-opt
   '("begin"
     "do"
     "end"
     "imports"
     "theory"
     )))

(defvar isar-outer-keyword
  (regexp-opt '(
    "ML"
    "ML_command"
    "ML_file"
    "ML_prf"
    "ML_val"
    "abbreviation"
    "assumes"
    "ax_specification"
    "axiomatization"
    "axioms"
    "chapter"
    "class"
    "class_deps"
    "classes"
    "classrel"
    "code_abort"
    "code_class"
    "code_const"
    "code_datatype"
    "code_deps"
    "code_include"
    "code_instance"
    "code_library"
    "code_module"
    "code_modulename"
    "code_monad"
    "code_pred"
    "code_reflect"
    "code_reserved"
    "code_thms"
    "code_type"
    "coinductive"
    "coinductive_set"
    "commit"
    "concrete_definition"
    "constdefs"
    "consts"
    "consts_code"
    "context"
    "corollary"
    "cpodef"
    "datatype"
    "declaration"
    "declare"
    "def"
    "definition"
    "disable_pr"
    "display_drafts"
    "domain"
    "domain_isomorphism"
    "done"
    "enable_pr"
    "end"
    "equivariance"
    "example_proof"
    "exit"
    "export_code"
    "extract"
    "extract_type"
    "finalconsts"
    "find_consts"
    "find_theorems"
    "fixes"
    "fixpat"
    "fixrec"
    "from"
    "full_prf"
    "fun"
    "function"
    "global"
    "global_interpretation"
    "guess"
    "have"
    "header"
    "help"
    "hide_class"
    "hide_const"
    "hide_fact"
    "hide_type"
    "inductive"
    "inductive_cases"
    "inductive_set"
    "init_toplevel"
    "instance"
    "instantiation"
    "interpret"
    "interpretation"
    "judgment"
    "lemma"
    "lemmas"
    "let"
    "linear_undo"
    "local"
    "local_setup"
    "locale"
    "method_setup"
    "moreover"
    "new_domain"
    "next"
    "nitpick"
    "nitpick_params"
    "no_notation"
    "no_syntax"
    "no_translations"
    "no_type_notation"
    "nominal_datatype"
    "nominal_inductive"
    "nominal_inductive2"
    "nominal_primrec"
    "nonterminals"
    "normal_form"
    "notation"
    "notepad"
    "obtains"
    "overloading"
    "paragraph"
    "parse_ast_translation"
    "parse_translation"
    "pcpodef"
    "prefer"
    "presume"
    "pretty_setmargin"
    "prf"
    "primrec"
    "print_abbrevs"
    "print_antiquotations"
    "print_ast_translation"
    "print_attributes"
    "print_binds"
    "print_cases"
    "print_claset"
    "print_classes"
    "print_codeproc"
    "print_codesetup"
    "print_commands"
    "print_configs"
    "print_context"
    "print_drafts"
    "print_facts"
    "print_induct_rules"
    "print_interps"
    "print_locale"
    "print_locales"
    "print_methods"
    "print_orders"
    "print_quotconsts"
    "print_quotients"
    "print_quotmaps"
    "print_rules"
    "print_simpset"
    "print_statement"
    "print_syntax"
    "print_theorems"
    "print_theory"
    "print_trans_rules"
    "print_translation"
    "proof"
    "prop"
    "proposition"
    "pwd"
    "qed"
    "quickcheck"
    "quickcheck_params"
    "quit"
    "quotient_definition"
    "quotient_type"
    "realizability"
    "realizers"
    "recdef"
    "recdef_tc"
    "record"
    "refute"
    "refute_params"
    "remove_thy"
    "rep_datatype"
    "repdef"
    "schematic_corollary"
    "schematic_lemma"
    "schematic_theorem"
    "sect"
    "section"
    "sepref_def"
    "sepref_definition"
    "sepref_register"
    "sepref_thm"
    "setup"
    "shows"
    "simproc_setup"
    "sledgehammer"
    "sledgehammer_params"
    "specification"
    "statespace"
    "subclass"
    "sublocale"
    "subparagraph"
    "subsect"
    "subsection"
    "subsubsect"
    "subsubsection"
    "syntax"
    "term"
    "termination"
    "text"
    "text_raw"
    "theorem"
    "theorems"
    "theory"
    "thm"
    "thm_deps"
    "thy_deps"
    "touch_thy"
    "translations"
    "txt"
    "txt_raw"
    "typ"
    "type_notation"
    "type_synonym"
    "typed_print_translation"
    "typedecl"
    "typedef"
    "types"
    "types_code"
    "undo"
    "undos_proof"
    "unused_thms"
    "use_thy"
    "value"
    "values"
    "welcome"
    "with"
    "write"
    "{"
    "}") t))

(defvar isar-inner-keyword
  (regexp-opt '("ML"
    "also"
    "apply"
    "apply_end"
    "assume"
    "back"
    "by"
    "consider"
    "define"
    "defer"
    "defer_recdef"
    "finally"
    "fix"
    "from"
    "fun"
    "global"
    "guess"
    "have"
    "hence"
    "let"
    "method"
    "next"
    "note"
    "obtain"
    "oops"
    "oracle"
    "overloading"
    "paragraph"
    "subparagraph"
    "parse_ast_translation"
    "parse_translation"
    "pcpodef"
    "proof"
    "prop"
    "show"
    "sledgehammer"
    "sledgehammer_params"
    "sorry"
    "subgoal"
    "supply"
    "then"
    "thm"
    "thm_deps"
    "thus"
    "ultimately"
    "unfolding"
    "using"
    "with") t))

(defvar isar-tactics
  (regexp-opt '("auto"
		"simp"
		"blast"
		"force"
		"fastforce"
		"fast") t))

(defvar isar-minor
  (regexp-opt '("is") t))

(defconst isar-font-lock-keywords-1
  (list (cons (concat "\\<" isar-outer-keyword "\\>") 'font-lock-builtin-face)
	(cons (concat "\\<" isar-inner-keyword "\\>") 'font-lock-constant-face)
	(cons (concat "\\<" isar-tactics "\\>") 'font-lock-variable-name-face)
	(cons (concat "\\<" isar-most-outer-keyword "\\>") 'font-lock-preprocessor-face)
	(cons (concat "\\<" isar-minor "\\>") 'font-lock-type-face)))


(defvar isar-font-lock-keywords isar-font-lock-keywords-1
  "Default highlighting expressions for isar mode")

(defvar isar-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; (modify-syntax-entry ?\" "" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?\$ "." st)
    (modify-syntax-entry ?\/ "." st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?+  "." st)
    (modify-syntax-entry ?-  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?%  "." st)
    (modify-syntax-entry ?\& "." st)
    (modify-syntax-entry ?.  "w" st)
    ;;(modify-syntax-entry ?_  "w" st)
    (modify-syntax-entry ?\' "w" st)
    (modify-syntax-entry ??  "w" st)
    (modify-syntax-entry ?\( "()1" st)
    (modify-syntax-entry ?\) ")(4" st)
    (modify-syntax-entry ?\{ "(}1b" st)
    (modify-syntax-entry ?\} "){4b" st)
    (modify-syntax-entry ?\* ". 23n" st)
  st)
  "Syntax table for isar-mode")

(defun isar-syntax-propertize (start end)
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ("\\((\\)\\(\\*\\)\\()\\)" ;; (*) are not opening comments
     (1 "w")))
   start end))

(defun isar-unicode-tokens-configure ()
  "Set the Unicode Tokens table and initialise."
  (dolist (var unicode-tokens-configuration-variables)
    (if (boundp (intern (concat "isar-" (symbol-name var))))
        (set (intern (concat "unicode-tokens-" (symbol-name var) "-variable"))
             (intern (concat "isar-" (symbol-name var))))))
  (unicode-tokens-initialise))

(defvar isar-name "isar"
  "Name of isar mode.")

;;;###autoload
(define-derived-mode isar-mode prog-mode isar-name
  "Major mode for editing isar files"
  (kill-all-local-variables)
  (set-syntax-table isar-mode-syntax-table)
  (set-keymap-parent isar-mode-map prog-mode-map)
  (use-local-map isar-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(isar-font-lock-keywords))
  (set (make-local-variable 'syntax-propertize-function)
        #'isar-syntax-propertize)
  (setq major-mode 'isar-mode)
  (setq mode-name "Isar")
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+[ \t]*")
  (setq-local comment-style 'multi-line)
  (isar-unicode-tokens-configure)
  (run-hooks 'isar-mode-hook)
  (unicode-tokens-mode 1))

;;spacemacs specific function
(when (boundp 'spacemacs-jump-handlers-isar-mode)
  (setq spacemacs-jump-handlers-isar-mode nil))

(provide 'isar-mode)

;;; isar-mode.el ends here
