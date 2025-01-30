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

;;; Start

(eval-when-compile
  (require 'cl-lib)
  (require 'dash))

(require 'dash)
(require 'pretty-ligatures)

(require 'isar-unicode-tokens)

(defcustom isar-mode-remove-utf8-when-saving nil
  "Replace the '‹' by the Isabelle encoding.")

(defvar isar-mode-map
  (make-sparse-keymap)
  "Keymap for isar major mode")

;;; Subscripts and superscripts

(defconst isar-control--token-properties
  `((sub          "Lower"         ,(if (display-graphic-p)
                                       '(display (raise -0.4))
                                     '(face (:width ultra-condensed :height 0.8 :slant reverse-italic))))
    (sup          "Raise"         ,(if (display-graphic-p)
                                       '(display (raise 0.4))
                                     '(face (:width ultra-condensed :height 0.8 :slant italic))))
    (bold         "Bold"          (face (:weight bold)))
    (italic       "Italic"        (face (:slant italic)))
    (big          "Bigger"        (face (:height 1.5)))
    (small        "Smaller"       (face (:height 0.75)))
    (underline    "Underline"     (face (:underline t)))
    (overline     "Overline"      (face (:overline t)))

    ;;     ;; NB: symbols for fonts need to be as in unicode-tokens-fonts
    ;;     (script       "Script font"   (face unicode-tokens-script-font-face))
    ;;     (frakt        "Frakt font"    (face unicode-tokens-fraktur-font-face))
    ;;     (serif        "Serif font"    (face unicode-tokens-serif-font-face))
    ;;     (sans         "Sans font"     (face unicode-tokens-sans-font-face))
    ;; ;    (large-symbol "Large Symbol font"
    ;; ;                 (face unicode-tokens-large-symbol-font-face))
    (keyword      "Keyword face"       (face font-lock-keyword-face))
    ;;     (function     "Function name face" (face font-lock-function-name-face))
    ;;     (type         "Type face"          (face font-lock-type-face))
    ;;     (preprocessor "Preprocessor face"  (face font-lock-preprocessor-face))
    ;;     (doc          "Documentation face" (face font-lock-preprocessor-face))
    ;;     (builtin      "Builtin face"       (face font-lock-builtin-face))
    ;;     (tacticals    "Tacticals face"     (face proof-tacticals-name-face))
    )
  "Association list mapping a symbol to a name and list of text properties.
Multiple symbols can be applied at once.")

(defconst isar-control--single-characters
  '(("Subscript" "sub" sub)
    ("Id subscript" "isub" sub)
    ("Superscript" "sup" sup)
    ("Id superscript" "isup" sup)
    ("Loc" "loc" keyword)
    ("Constant" "const" keyword)
    ("Bold" "bold" bold)
    ;; unofficial/unsupported:
    ("Italic" "italic" italic))
  "Control character tokens for Isabelle.")

(defconst isar-control--regions
  '(("Subscript" "bsub" "esub" sub)
    ("Superscript" "bsup" "esup" sup)
    ;; unofficial/unsupported:
    ("Id subscript" "bisub" "eisub" sub)
    ("Id superscript" "bisup" "eisup" sup)
    ("Bold" "bbold" "ebold" bold)
    ("Italic" "bitalic" "eitalic" italic)
    ;; ("Script" "bscript" "escript" script)
    ;; ("Frakt" "bfrakt" "efrakt" frakt)
    ;; ("Roman" "bserif" "eserif" serif)
    ;; ("Sans" "bsans" "esans" sans)
    ("Overline" "boverline" "eoverline" overline)
    ("Underline" "bunderline" "eunderline" underline)
    ("Big"   "bbig" "ebig" big)
    ("Small" "bsmall" "esmall" small)
    ;; ("Large symbols" "bbigsyms" "ebigsyms" large-symbols)
    )
  "Control sequence tokens for Isabelle.")

(defun isar-control--name-to-props (name &optional reset-face)
  "Turn the property name list SYMBS into a list of text properties.

Optional argument RESET-FACE means set the face property to nil, unless
'face is in the property list."
  (cl-assert (symbolp name))
  (let ((ps (cddr-safe (assq name isar-control--token-properties)))
        props)
    (cl-assert (not (null ps)) nil "Unknown property name: %s" name)
    (dolist (p ps)
      (setq props (append p props)))
    (when (and reset-face
               (not (memq 'face props)))
      (setq props (append '(face nil) props)))
    props))

;; this is adapted from font-lock-prepend-text-property, which
;; currently fails to merge property values for 'face property properly.
;; e.g., it makes (:slant italic (:weight bold font-lock-string-face))
;; rather than  (:slant italic :weight bold font-lock-string-face)
;;
(defun isar-control--prepend-text-property! (start end prop value &optional object)
  "Prepend to one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to append to the value
already in place.  The resulting property values are always lists.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((val (if (listp value)
                 value
               (list value)))
        next
        prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
            prev (get-text-property start prop object))
      ;; Canonicalize old forms of face property.
      (when (and (memq prop '(face font-lock-face))
                 (listp prev)
                 (let ((x (car prev)))
                   (or (keywordp x)
                       (memq x '(foreground-color background-color)))))
        (setq prev (list prev)))
      (setq prev (if (listp prev) prev (list prev)))
      ;; hack to flatten erroneously nested face property lists
      (when (and (memq prop '(face font-lock-face))
                 (listp (car prev))
                 (null (cdr prev)))
        (setq prev (car prev)))
      (put-text-property start next prop (append-plists-uniq val prev) object)
      (setq start next))))

(defun isar-control--prepend-text-properties-in-region! (start end props)
  (while props
    (isar-control--prepend-text-property! start end (car props) (cadr props))
    (setq props (cddr props))))

(defconst isar-control--char-re-template
  (eval-when-compile
    (concat
     "\\(?1:\\\\<\\^%s>\\)"
     ;; Nest ? to work on unfinished symbols while they’re being written.
     (rx (group-n 2
           (or (seq ?\\
                    (? ?<
                       (? (+ (any (?A . ?Z) (?a . ?z)))
                          (? ?>))))
               (not ?\\))
           )))))

(defconst isar-control--region-start-re-template
  "\\(?1:\\\\<\\^%s>\\)")

(defun isar-control--make-control-char-font-lock-keyword (s prop)
  (cl-assert (stringp s))
  (cl-assert (symbolp prop))
  `(,(format isar-control--char-re-template (regexp-quote s))
    (0 (isar-control--mark-char-form-start ',prop
                                           (match-beginning 1)
                                           (match-end 1)
                                           (match-beginning 2)
                                           (match-end 2)
                                           ',(isar-control--name-to-props prop)))))

(defun isar-control--mark-char-form-start (prop start-delim-start start-delim-end body-start body-end props)
  (let ((control-props
         `(invisible
           ,(if (and isar-control--current-symbol-bounds
                     (eq start-delim-start (car isar-control--current-symbol-bounds))
                     (eq start-delim-end (cdr isar-control--current-symbol-bounds)))
                ;; We’re currently inside this control sequence, don’t
                ;; hide it. We may have just moved into it from pretty
                ;; ligature so pretty ligature just issued
                ;; ‘font-lock-flush’ which invalidated whole line and
                ;; we’re refontifying region that has already been
                ;; shown by ‘isar-control--post-command-hook’.
                nil
              'isar--control-sequence)
           isar-control-sequence ,prop

           isar-control-sequence-start-delim-bounds ,(cons start-delim-start start-delim-end)
           isar-control-sequence-end-delim-bounds nil
           isar-control-sequence-body-bounds ,(cons body-start body-end)

           ;;cursor-sensor-functions (isar-control--sense-cursor)

           modification-hooks (isar-control--modification-hook)
           insert-in-front-hooks (isar-control--insert-before-hook)
           insert-behind-hooks (isar-control--insert-behind-hook))))
    (add-text-properties start-delim-start start-delim-end control-props)
    (isar-control--prepend-text-properties-in-region! body-start body-end props)))

(defun isar-control--make-control-region-font-lock-keyword (start end prop)
  (cl-assert (stringp start))
  (cl-assert (stringp end))
  (cl-assert (symbolp prop))
  (let ((start-re (regexp-quote start))
        (end-re (regexp-quote end)))
    `(,(format isar-control--region-start-re-template start-re)
      (0 (isar-control--mark-region-from-start ',prop ,(format isar-control--region-start-re-template end-re) ',(isar-control--name-to-props prop))))))

(defun isar-control--mark-region-from-start (prop end-re props)
  (let ((start-delim-start (match-beginning 0))
        (start-delim-end (match-end 0)))
    (save-match-data
      (save-excursion
        (when (re-search-forward end-re nil t)
          (let* ((end-delim-start (match-beginning 0))
                 (end-delim-end (match-end 0))

                 (control-props
                  `(invisible
                    isar--control-sequence
                    isar-control-sequence ,prop
                    isar-control-sequence-start-delim-bounds ,(cons start-delim-start start-delim-end)
                    isar-control-sequence-end-delim-bounds ,(cons end-delim-start end-delim-end)
                    isar-control-sequence-body-bounds ,(cons start-delim-end end-delim-start)

                    modification-hooks (isar-control--modification-hook)
                    insert-in-front-hooks (isar-control--insert-before-hook)
                    insert-behind-hooks (isar-control--insert-behind-hook))))
            (with-silent-modifications
              (add-text-properties start-delim-start start-delim-end control-props)
              (add-text-properties end-delim-start end-delim-end control-props)
              (isar-control--prepend-text-properties-in-region! start-delim-end
                                                                end-delim-start
                                                                props))))))))

(defconst isar-control--font-lock-keywords
  (eval-when-compile
    (append
     (--map (isar-control--make-control-char-font-lock-keyword (cl-second it) (cl-third it))
            isar-control--single-characters)
     (--map (isar-control--make-control-region-font-lock-keyword (cl-second it) (cl-third it) (cl-fourth it))
            isar-control--regions))))

(defvar-local isar-control--current-symbol-bounds nil)

(defun isar-control--post-command-hook ()
  ;; Re-apply prettification to the previous symbol.
  (when isar-control--current-symbol-bounds
    (let ((s (car isar-control--current-symbol-bounds))
          (e (cdr isar-control--current-symbol-bounds)))
      (when (or (< (point) s)
                (> (point) e))
        (font-lock-flush s e)
        (setq isar-control--current-symbol-bounds nil))))
  ;; Unprettify the current control sequence. Try positions +- 1 near cursor since
  (let ((offsets '(0 -1 ;; 1
                   ))
        (continue? t))
    (while (and offsets
                continue?)
      (let* ((offset (car offsets))
             (p (max (point-min) (+ (point) offset))))
        (when (get-text-property p 'isar-control-sequence)
          (let ((s (or (previous-single-property-change p 'isar-control-sequence) p))
                (e (or (next-single-property-change p 'isar-control-sequence) p)))
            (with-silent-modifications
              (setq isar-control--current-symbol-bounds (cons s e))
              (remove-text-properties s e '(invisible isar--control-sequence ;; composition nil
                                                      )))
            (setf continue? nil))))
      (setf offsets (cdr offsets)))))

;; This gets called before actual update takes place - buffer state and characters are exactly
;; before the chaange and all positions are valid since no characters were changed yet, hence
;; ‘isar-control-sequence-body-bounds’ and other property values are still valid.
(defun isar-control--reset-control-formatting-before-change! (beg end)
  (when-let* ((prop (get-text-property beg 'isar-control-sequence)))
    (let ((inhibit-modification-hooks t)
          (body-bounds (get-text-property beg 'isar-control-sequence-body-bounds))
          (start-delim-bounds (get-text-property beg 'isar-control-sequence-start-delim-bounds))
          (end-delim-bounds (get-text-property beg 'isar-control-sequence-end-delim-bounds))

          (control-props '(invisible
                           nil
                           isar-control-sequence nil
                           isar-control-sequence-start-delim-bounds nil
                           isar-control-sequence-end-delim-bounds nil
                           isar-control-sequence-body-bounds nil
                           modification-hooks nil
                           insert-in-front-hooks nil
                           insert-behind-hooks nil)))
      (cl-assert (symbolp prop))
      (cl-assert (listp body-bounds))
      (cl-assert (listp start-delim-bounds))
      (cl-assert (or (null end-delim-bounds) (listp end-delim-bounds)))

      (remove-text-properties (car body-bounds) (cdr body-bounds) (isar-control--name-to-props prop))

      ;; Caution: compositional thing to do here would be to remove only ourselves from
      ;; modification, insert in front and insert behind hooks. For simplicity we’re removing
      ;; all modification hooks here in expectation that noone else is going to use them with Isar
      ;; mode, which is not unreasonable but could break miserably one day. This note here
      ;; aims to leave some cues for that scenario.
      (remove-text-properties (car start-delim-bounds) (cdr start-delim-bounds) control-props)
      (when end-delim-bounds
        (remove-text-properties (car end-delim-bounds) (cdr end-delim-bounds) control-props)))))

(defun isar-control--modification-hook (beg end)
  "Remove text properties from modified control sequence."
  (isar-control--reset-control-formatting-before-change! beg end))

(defun isar-control--insert-before-hook (beg end)
  "Remove text properties from modified control sequence."
  (isar-control--reset-control-formatting-before-change! beg end))

(defun isar-control--insert-behind-hook (beg end)
  "Remove text properties from modified control sequence."
  (isar-control--reset-control-formatting-before-change! beg end))

;;; Font lock

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
          "if"
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
  (eval-when-compile
    (append
     (list
      ;; (cons isar-tactics 'isar-tactics-face)
      `(,isar-keyword1 0 'isar-keyword1-face)
      `(,isar-keyword2 0 'isar-keyword2-face)
      `(,isar-keyword3 0 'isar-keyword3-face)
      `(,isar-minor    0 'isar-minor-face))
     isar-control--font-lock-keywords))
  "Default highlighting expressions for isar mode")

;;; Syntax table

(defconst isar-mode-syntax-table
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
    (modify-syntax-entry ?\n ">" st)
  st)
  "Syntax table for isar-mode")

(defun isar-syntax-propertize (start end)
  (funcall
   (syntax-propertize-rules
    ;; Comments take priority over everything else
    ((rx (seq (group-n 1 "\\") "<comment>"))
     (1 "<"))

    ((rx (group-n 2 "(") "*" (group-n 3 ")")) ;; (*) are not opening comments
     (2 "_")
     (3 "_"))

    ((rx (or (seq (group-n 4 "\\") "<open>") (seq "\\<close" (group-n 5 ">"))))
     ;; Generic string delimiters must span single characters or adjacent characters
     ;; will be matched against each other.
     (4 "|")
     (5 "|")))
   start
   end))

(defun isar-syntax-propertize-extend-region (start end)
  "Member of ‘syntax-propertize-extend-region-functions’ that extends region until it can
be safely analyzed by ‘isar-syntax-propertize’."
  (save-excursion
    (save-match-data
      (let ((new-start nil)
            (new-end nil))
        (goto-char start)
        (when (and (re-search-backward (rx "\\<" (or (group-n 1 "open") "close") ">") nil t)
                   (setf new-start (match-beginning 1)))
          ;; Found unclosed open above.
          (setf start new-start))
        (goto-char end)
        (when (and (re-search-forward (rx "\\<" (or "open" (group-n 1 "close")) ">") nil t)
                   (setf new-end (match-end 1)))
          ;; Found unopened close after.
          (setf end new-end))
        (when (or new-start new-end)
          (cons start end))))))

;;; Helpers

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

;;; Mode definition

(defun isar--setup-font-lock! (keywords)
  (setq-local font-lock-defaults (list keywords nil)
              syntax-propertize-function #'isar-syntax-propertize

              font-lock-extra-managed-props
              (append '(invisible
                        isar-control-sequence
                        isar-control-sequence-body-bounds
                        isar-control-sequence-start-delim-bounds
                        isar-control-sequence-end-delim-bounds)
                      font-lock-extra-managed-props))

  (add-hook 'syntax-propertize-extend-region-functions
            #'isar-syntax-propertize-extend-region
            nil
            t)

  (pretty-ligatures-install-isabelle-ligatures!)
  (add-hook 'post-command-hook #'isar-control--post-command-hook nil t))

;;;###autoload
(define-derived-mode isar-mode prog-mode "isar"
  "Major mode for editing isar files"

  (setq-local comment-start "(* "
              comment-end " *)"
              comment-start-skip "(\\*+[ \t]*"
              comment-style 'multi-line

              indent-line-function #'lsp-isar-indent-line)

  (isar--setup-font-lock! isar-font-lock-keywords)

  (add-hook 'after-save-hook #'isar-replace-all-utf8-by-encoding nil t))

;;spacemacs specific function
(when (boundp 'spacemacs-jump-handlers-isar-mode)
  (setq spacemacs-jump-handlers-isar-mode nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.thy\\'" . isar-mode))

(provide 'isar-mode)

;;; isar-mode.el ends here
