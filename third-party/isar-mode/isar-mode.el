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

(require 'pretty-ligatures)

(require 'isar-unicode-tokens)

(defvar isar-mode-hook nil)
(defcustom isar-mode-remove-utf8-when-saving nil
  "Replace the '‹' by the Isabelle encoding.")

(defvar isar-mode-map
  (make-sparse-keymap)
  "Keymap for isar major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.thy\\'" . isar-mode))

;; dark blue
(defvar isar-keyword1
  (rx bow
      (or "ML"
          "ML_command"
          "ML_file"
          "ML_prf"
          "ML_val"
          "abbreviation"
          "also"
          "apply"
          "apply_end"
          "ax_specification"
          "axiomatization"
          "axioms"
          "back"
          "by"
          "case"
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
          "consider"
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
          "defer"
          "defer_recdef"
          "define"
          "definition"
          "disable_pr"
          "display_drafts"
          "domain"
          "domain_isomorphism"
          "done"
          "enable_pr"
          "equivariance"
          "example_proof"
          "exit"
          "export_code"
          "extract"
          "extract_type"
          "finalconsts"
          "finally"
          "find_consts"
          "find_theorems"
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
          "hence"
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
          "method"
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
          "note"
          "notepad"
          "oops"
          "oracle"
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
          "simproc_setup"
          "sledgehammer"
          "sledgehammer_params"
          "sorry"
          "specification"
          "statespace"
          "subclass"
          "subgoal"
          "sublocale"
          "subparagraph"
          "subsect"
          "subsection"
          "subsubsect"
          "subsubsection"
          "supply"
          "syntax"
          "term"
          "termination"
          "text"
          "text_raw"
          "then"
          "theorem"
          "theorems"
          "theory"
          "thm"
          "thm_deps"
          "thus"
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
          "ultimately"
          "undo"
          "undos_proof"
          "unfolding"
          "unused_thms"
          "use_thy"
          "using"
          "value"
          "values"
          "welcome"
          "with"
          "write"
          ;; "{"
          ;; "}"
          )
      eow))

;; green
(defvar isar-keyword2
  (rx bow
      (or "and"
          "assumes"
          "begin"
          "do"
          "end"
          "fixes"
          "for"
          "imports"
          "infixl"
          "infixr"
          "obtains"
          "shows"
          "where")
      eow))

;; light blue
(defvar isar-keyword3
  (rx bow
      (or "assume"
          "case"
          "define"
          "fix"
          "obtain"
          "show"
          "thus"
          "presume")
      eow))

(defvar isar-tactics
  (rx bow
      (or "auto"
          "blast"
          "cases"
          "contradiction"
          "force"
          "fastforce"
          "fast"
          "rule"
          "simp")
      eow))

(defvar isar-minor
  (rx bow
      (or "is" "of" "OF" "THEN")
      eow))

(defface isar-keyword1-face
  `((t (:inherit font-lock-keyword-face)))
  ""
  :group 'lsp-isar-sem)

(defface isar-keyword2-face
  `((t (:inherit font-lock-keyword-face)))
  ""
  :group 'lsp-isar-sem)

(defface isar-keyword3-face
  `((t (:inherit font-lock-keyword-face)))
  ""
  :group 'lsp-isar-sem)

(defface isar-tactics-face
  `((t (:inherit font-lock-constant-face)))
  ""
  :group 'lsp-isar-sem)

(defface isar-minor-face
  `((t (:inherit font-lock-constant-face)))
  ""
  :group 'lsp-isar-sem)

(defconst isar-font-lock-keywords
  (list
   ;; (cons isar-tactics 'isar-tactics-face)
   `(,isar-keyword1 0 'isar-keyword1-face)
   `(,isar-keyword2 0 'isar-keyword2-face)
   `(,isar-keyword3 0 'isar-keyword3-face)
   `(,isar-minor    0 'isar-minor-face))
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
   start
   end))

(defun isar-unicode-tokens-configure ()
  "Set the Unicode Tokens table and initialise."
  (dolist (var unicode-tokens-configuration-variables)
    (if (boundp (intern (concat "isar-" (symbol-name var))))
        (set (intern (concat "unicode-tokens-" (symbol-name var) "-variable"))
             (intern (concat "isar-" (symbol-name var))))))
  (unicode-tokens-initialise)
  ;; Map raw unicode to equivalent Isabelle sequences.
  (cl-loop for (token beautified)
           in (cl-concatenate 'list isar-symbols-tokens isar-extended-symbols-tokens isar-symbols-tokens)
	         do (define-key isar-mode-map (kbd beautified) (format "\\<%s>" token)))
)

;; provided by Ghilain https://github.com/m-fleury/isabelle-emacs/issues/83
(defun isar-unicodify-region-or-buffer ()
  "Open a view of the active region (or the whole buffer if none) using true Unicode tokens."
  (interactive)
  (let* ((range (if (use-region-p) (list (region-beginning) (region-end)) (list nil nil)))
         (start (car range))
         (end (cadr range))
         ;; Register all the existing symbols to be replaced.
         (symbs (append isar-extended-symbols-tokens isar-symbols-tokens isar-modifier-symbols-tokens))
         (buf (generate-new-buffer (format "%s-unicode" (buffer-name)))))
    ;; Copy the selected region (or the whole buffer) into `buf'.
    (insert-into-buffer buf start end)
    ;; Switch to the (soon to be) beautiful buffer.
    (switch-to-buffer buf)
    ;; Now replace Isabelle sequences in `buf' by actual Unicode symbols.
    (setq case-fold-search nil)
    (mapc
     (lambda (x)
       (goto-char (point-max))
       (while (re-search-backward (regexp-quote (format isar-token-format (car x))) nil t)
         (replace-match (cadr x) nil t)))
     symbs)
    ;; And this should be it!
  ))

(defun isar-replace-all-utf8-by-encoding ()
  "Remove the accidentally entered utf8 by the corresponding Isabelle encoding"
  (interactive)
  (when isar-mode-remove-utf8-when-saving
    (save-match-data
      (save-excursion
        (dolist (symbolpair (append
                             isar-symbols-tokens
                             isar-extended-symbols-tokens))
          (let* ((symbol (car symbolpair))
                 (utf8-symbol (cadr symbolpair)))
            (goto-char (point-min))
            (while (re-search-forward utf8-symbol nil t)
              (replace-match (concat "\\\\<" symbol ">") nil nil))))))))

;;;###autoload
(define-derived-mode isar-mode prog-mode "isar"
  "Major mode for editing isar files"
  (kill-all-local-variables)
  (set-syntax-table isar-mode-syntax-table)
  (set-keymap-parent isar-mode-map prog-mode-map)
  (use-local-map isar-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(isar-font-lock-keywords))
  (set (make-local-variable 'syntax-propertize-function)
        #'isar-syntax-propertize)
  (setq major-mode 'isar-mode
        mode-name "Isar")
  (setq-local comment-start "(* "
              comment-end " *)"
              comment-start-skip "(\\*+[ \t]*"
              comment-style 'multi-line)
  (pretty-ligatures-install-isabelle-ligatures!)

  (isar-unicode-tokens-configure)
  (add-hook 'after-save-hook #'isar-replace-all-utf8-by-encoding nil t)
  (unicode-tokens-mode 1))

;;spacemacs specific function
(when (boundp 'spacemacs-jump-handlers-isar-mode)
  (setq spacemacs-jump-handlers-isar-mode nil))

(provide 'isar-mode)

;;; isar-mode.el ends here
