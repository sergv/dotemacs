;; Copyright (c) 2013, 2014 Microsoft Corporation. All rights reserved.
;; Released under Apache 2.0 license as described in the file LICENSE.
;;
;; Author: Leonardo de Moura
;;         Soonho Kong
;;

(require 'dash)
(require 'rx)

(defconst lean-keywords1
  '("import" "prelude" "protected" "private" "noncomputable" "definition" "meta" "renaming"
    "hiding" "exposing" "parameter" "parameters" "begin" "constant" "constants"
    "lemma" "variable" "variables" "theorem" "example" "abbreviation"
    "open" "export" "axiom" "axioms" "inductive" "coinductive" "with" "without"
    "structure" "universe" "universes" "hide"
    "precedence" "reserve" "declare_trace" "add_key_equivalence"
    "match" "infix" "infixl" "infixr" "notation" "postfix" "prefix" "instance"
    "end" "this" "using" "using_well_founded" "namespace" "section"
    "attribute" "local" "set_option" "extends" "include" "omit" "classes" "class"
    "attributes" "raw" "replacing"
    "calc" "have" "show" "suffices" "by" "in" "at" "do" "let" "forall" "Pi" "fun"
    "exists" "if" "then" "else" "assume" "from"
    "mutual" "def" "run_cmd")
  "lean keywords ending with 'word' (not symbol)")
(defconst lean-keywords1-regexp
  (eval `(rx word-start (or ,@lean-keywords1) word-end)))
(defconst lean-constants
  '("#" "@" "!" "$" "->" "∼" "↔" "/" "==" "=" ":=" "<->" "/\\" "\\/" "∧" "∨"
    "≠" "<" ">" "≤" "≥" "¬" "<=" ">=" "⁻¹" "⬝" "▸" "+" "*" "-" "/" "λ"
    "→" "∃" "∀" "∘" "×" "Σ" "Π" "~" "||" "&&" "≃" "≡" "≅"
    "ℕ" "ℤ" "ℚ" "ℝ" "ℂ" "𝔸"
    "⬝e" "⬝i" "⬝o" "⬝op" "⬝po" "⬝h" "⬝v" "⬝hp" "⬝vp" "⬝ph" "⬝pv" "⬝r" "◾" "◾o"
    "∘n" "∘f" "∘fi" "∘nf" "∘fn" "∘n1f" "∘1nf" "∘f1n" "∘fn1"
    "^c" "≃c" "≅c" "×c" "×f" "×n" "+c" "+f" "+n" "ℕ₋₂")
  "lean constants")
(defconst lean-constants-regexp (regexp-opt lean-constants))
(defconst lean-numerals-regexp
  (eval `(rx word-start
             (one-or-more digit) (optional (and "." (zero-or-more digit)))
             word-end)))

(defconst lean-warnings '("sorry" "exit") "lean warnings")
(defconst lean-warnings-regexp
  (eval `(rx word-start (or ,@lean-warnings) word-end)))


(defconst lean-syntax-table
  (let ((st (make-syntax-table)))
    ;; Matching parens
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)

    ;; comment
    (modify-syntax-entry ?/ ". 14nb" st)
    (modify-syntax-entry ?- ". 123" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?« "<" st)
    (modify-syntax-entry ?» ">" st)

    ;; Word constituent
    (mapc (lambda (x) (modify-syntax-entry x "w" st))
          (list ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
                ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
                ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
                ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z))
    (mapc (lambda (x) (modify-syntax-entry x "w" st))
          (list ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    (mapc (lambda (x) (modify-syntax-entry x "w" st))
          (list ?α ?β ?γ ?δ ?ε ?ζ ?η ?θ ?ι ?κ ;;?λ
                ?μ ?ν ?ξ ?ο ?π ?ρ ?ς ?σ ?τ ?υ
                ?φ ?χ ?ψ ?ω))
    (mapc (lambda (x) (modify-syntax-entry x "w" st))
          (list ?ϊ ?ϋ ?ό ?ύ ?ώ ?Ϗ ?ϐ ?ϑ ?ϒ ?ϓ ?ϔ ?ϕ ?ϖ
                ?ϗ ?Ϙ ?ϙ ?Ϛ ?ϛ ?Ϝ ?ϝ ?Ϟ ?ϟ ?Ϡ ?ϡ ?Ϣ ?ϣ
                ?Ϥ ?ϥ ?Ϧ ?ϧ ?Ϩ ?ϩ ?Ϫ ?ϫ ?Ϭ ?ϭ ?Ϯ ?ϯ ?ϰ
                ?ϱ ?ϲ ?ϳ ?ϴ ?ϵ ?϶ ?Ϸ ?ϸ ?Ϲ ?Ϻ ?ϻ))
    (mapc (lambda (x) (modify-syntax-entry x "w" st))
          (list ?ἀ ?ἁ ?ἂ ?ἃ ?ἄ ?ἅ ?ἆ ?ἇ ?Ἀ ?Ἁ ?Ἂ ?Ἃ ?Ἄ
                ?Ἅ ?Ἆ ?Ἇ ?ἐ ?ἑ ?ἒ ?ἓ ?ἔ ?ἕ ?἖ ?἗ ?Ἐ ?Ἑ
                ?Ἒ ?Ἓ ?Ἔ ?Ἕ ?἞ ?἟ ?ἠ ?ἡ ?ἢ ?ἣ ?ἤ ?ἥ
                ?ἦ ?ἧ ?Ἠ ?Ἡ ?Ἢ ?Ἣ ?Ἤ ?Ἥ ?Ἦ ?Ἧ ?ἰ ?ἱ
                ?ἲ ?ἳ ?ἴ ?ἵ ?ἶ ?ἷ ?Ἰ ?Ἱ ?Ἲ ?Ἳ ?Ἴ ?Ἵ ?Ἶ ?Ἷ
                ?ὀ ?ὁ ?ὂ ?ὃ ?ὄ ?ὅ ?὆ ?὇ ?Ὀ ?Ὁ ?Ὂ ?Ὃ
                ?Ὄ ?Ὅ ?὎ ?὏ ?ὐ ?ὑ ?ὒ ?ὓ ?ὔ ?ὕ ?ὖ ?ὗ
                ?὘ ?Ὑ ?὚ ?Ὓ ?὜ ?Ὕ ?὞ ?Ὗ ?ὠ ?ὡ ?ὢ
                ?ὣ ?ὤ ?ὥ ?ὦ ?ὧ ?Ὠ ?Ὡ ?Ὢ ?Ὣ ?Ὤ ?Ὥ ?Ὦ
                ?Ὧ ?ὰ ?ά ?ὲ ?έ ?ὴ ?ή ?ὶ ?ί ?ὸ ?ό ?ὺ ?ύ ?ὼ
                ?ώ ?὾ ?὿ ?ᾀ ?ᾁ ?ᾂ ?ᾃ ?ᾄ ?ᾅ ?ᾆ ?ᾇ ?ᾈ
                ?ᾉ ?ᾊ ?ᾋ ?ᾌ ?ᾍ ?ᾎ ?ᾏ ?ᾐ ?ᾑ ?ᾒ ?ᾓ ?ᾔ
                ?ᾕ ?ᾖ ?ᾗ ?ᾘ ?ᾙ ?ᾚ ?ᾛ ?ᾜ ?ᾝ ?ᾞ ?ᾟ ?ᾠ ?ᾡ ?ᾢ
                ?ᾣ ?ᾤ ?ᾥ ?ᾦ ?ᾧ ?ᾨ ?ᾩ ?ᾪ ?ᾫ ?ᾬ ?ᾭ ?ᾮ ?ᾯ ?ᾰ
                ?ᾱ ?ᾲ ?ᾳ ?ᾴ ?᾵ ?ᾶ ?ᾷ ?Ᾰ ?Ᾱ ?Ὰ ?Ά ?ᾼ ?᾽
                ?ι ?᾿ ?῀ ?῁ ?ῂ ?ῃ ?ῄ ?῅ ?ῆ ?ῇ ?Ὲ ?Έ ?Ὴ
                ?Ή ?ῌ ?῍ ?῎ ?῏ ?ῐ ?ῑ ?ῒ ?ΐ ?῔ ?῕ ?ῖ ?ῗ
                ?Ῐ ?Ῑ ?Ὶ ?Ί ?῜ ?῝ ?῞ ?῟ ?ῠ ?ῡ ?ῢ ?ΰ ?ῤ ?ῥ
                ?ῦ ?ῧ ?Ῠ ?Ῡ ?Ὺ ?Ύ ?Ῥ ?῭ ?΅ ?` ?῰ ?῱ ?ῲ ?ῳ
                ?ῴ ?῵ ?ῶ ?ῷ ?Ὸ ?Ό ?Ὼ ?Ώ ?ῼ ?´ ?῾))
    (mapc (lambda (x) (modify-syntax-entry x "w" st))
          (list ?℀ ?℁ ?ℂ ?℃ ?℄ ?℅ ?℆ ?ℇ ?℈ ?℉ ?ℊ ?ℋ ?ℌ ?ℍ ?ℎ
                ?ℏ ?ℐ ?ℑ ?ℒ ?ℓ ?℔ ?ℕ ?№ ?℗ ?℘ ?ℙ ?ℚ ?ℛ ?ℜ ?ℝ
                ?℞ ?℟ ?℠ ?℡ ?™ ?℣ ?ℤ ?℥ ?Ω ?℧ ?ℨ ?℩ ?K ?Å ?ℬ
                ?ℭ ?℮ ?ℯ ?ℰ ?ℱ ?Ⅎ ?ℳ ?ℴ ?ℵ ?ℶ ?ℷ ?ℸ ?ℹ ?℺ ?℻
                ?ℼ ?ℽ ?ℾ ?ℿ ?⅀ ?⅁ ?⅂ ?⅃ ?⅄ ?ⅅ ?ⅆ ?ⅇ ?ⅈ ?ⅉ ?⅊
                ?⅋ ?⅌ ?⅍ ?ⅎ ?⅏))
    (modify-syntax-entry ?' "w" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?\. "w" st)

    ;; Lean operator chars
    (mapc (lambda (ch) (modify-syntax-entry ch "_" st))
          "!#$%&*+<=>@^|~:")

    ;; Whitespace is whitespace
    (modify-syntax-entry ?\  " " st)
    (modify-syntax-entry ?\t " " st)

    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "/" st)

    st))

(defconst lean-font-lock-defaults
  `((;; attributes
     (,(rx word-start "attribute" word-end (zero-or-more whitespace) (group (one-or-more "[" (zero-or-more (not (any "]"))) "]" (zero-or-more whitespace))))
      (1 'font-lock-doc-face))
     (,(rx (group "@[" (zero-or-more (not (any "]"))) "]"))
      (1 'font-lock-doc-face))
     (,(rx (group "#" (or "eval" "print" "reduce" "help" "check")))
      (1 'font-lock-keyword-face))
     ;; mutual definitions "names"
     (,(rx word-start
           "mutual"
           word-end
           (zero-or-more whitespace)
           word-start
           (or "inductive" "definition" "def")
           word-end
           (group (zero-or-more (not (any " \t\n\r{([,"))) (zero-or-more (zero-or-more whitespace) "," (zero-or-more whitespace) (not (any " \t\n\r{([,")))))
      (1 'font-lock-function-name-face))
     ;; declarations
     (,(rx word-start
           (group (or "inductive" (group "class" (zero-or-more whitespace) "inductive") "instance" "structure" "class" "theorem" "axiom" "axioms" "lemma" "definition" "def" "constant"))
           word-end (zero-or-more whitespace)
           (group (zero-or-more "{" (zero-or-more (not (any "}"))) "}" (zero-or-more whitespace)))
           (zero-or-more whitespace)
           (group (zero-or-more (not (any " \t\n\r{([")))))
      (4 'font-lock-function-name-face))
     ;; Constants which have a keyword as subterm
     (,(rx (or "∘if")) . 'font-lock-constant-face)
     ;; Keywords
     ("\\(set_option\\)[ \t]*\\([^ \t\n]*\\)" (2 'font-lock-constant-face))
     (,lean-keywords1-regexp . 'font-lock-keyword-face)
     (,(rx word-start (group "example") ".") (1 'font-lock-keyword-face))
     (,(rx (or "∎")) . 'font-lock-keyword-face)
     ;; Types
     (,(rx word-start (or "Prop" "Type" "Type*" "Sort" "Sort*") symbol-end) . 'font-lock-type-face)
     (,(rx word-start (group (or "Prop" "Type" "Sort")) ".") (1 'font-lock-type-face))
     ;; String
     ("\"[^\"]*\"" . 'font-lock-string-face)
     ;; ;; Constants
     (,lean-constants-regexp . 'font-lock-constant-face)
     (,lean-numerals-regexp . 'font-lock-constant-face)
     ;; place holder
     (,(rx symbol-start "_" symbol-end) . 'font-lock-preprocessor-face)
     ;; warnings
     (,lean-warnings-regexp . 'font-lock-warning-face)
     ;; escaped identifiers
     (,(rx (and (group "«") (group (one-or-more (not (any "»")))) (group "»")))
      (1 font-lock-comment-face t)
      (2 nil t)
      (3 font-lock-comment-face t))
     )))

;; Syntax Highlighting for Lean Info Mode
(defconst lean-info-font-lock-defaults
  (let ((new-entries
         `(;; Please add more after this:
           (,(rx (group (+ symbol-start (+ (or word (char ?₁ ?₂ ?₃ ?₄ ?₅ ?₆ ?₇ ?₈ ?₉ ?₀))) symbol-end (* white))) ":")
            (1 'font-lock-variable-name-face))
           (,(rx white ":" white)
            . 'font-lock-keyword-face)
           (,(rx "⊢" white)
            . 'font-lock-keyword-face)
           (,(rx "[" (group "stale") "]")
            (1 'font-lock-warning-face))
           (,(rx line-start "No Goal" line-end)
            . 'font-lock-constant-face)))
        (inherited-entries (car lean-font-lock-defaults)))
    `(,(-concat new-entries inherited-entries))))

(provide 'lean-syntax)
