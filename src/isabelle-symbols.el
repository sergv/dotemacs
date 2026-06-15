;; isabelle-symbols.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  3 December 2023
;; Description:

(require 'pretty-ligatures)

(require 'common-font)

(defun pretty-ligatures--isabelle-install! (ligatures)
  "Add hasklig ligatures for use with prettify-symbols-mode."
  (when (pretty-ligatures-supported?)
    (setq-local prettify-symbols-alist
                (append ligatures
                        prettify-symbols-alist)
                prettify-symbols-unprettify-at-point t
                prettify-symbols-compose-predicate #'pretty-ligatures--isabelle--compose-p)
    (prettify-symbols-mode)))

(defun pretty-ligatures--isabelle--compose-p (start end match-str)
  "Do not prettify withing comments or within words/operators."
  (let ((len (length match-str)))
    (and
     (not (zerop len))
     (let ((start-char (aref match-str 0))
           (end-char (aref match-str (1- len))))
       (or
        ;; Always fontify ligatures of the form "\<...>".
        (and (eq start-char ?\\)
             (eq end-char ?\>)
             (eq (aref match-str 1) ?<))
        (let* ((syntaxes-beg (if (memq (char-syntax start-char) '(?w ?_))
                                 '(?w ?_)
                               '(?. ?\\)))
               (syntaxes-end (if (memq (char-syntax end-char) '(?w ?_))
                                 '(?w ?_)
                               '(?. ?\\)))
               (next-char (or (char-after end) ?\s)))
          (and (not (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg))
               (or (eq next-char ?,)
                   (not (memq (char-syntax next-char) syntaxes-end)))
               ;; Enable ligatures in both strings and comments for now - keep the below commented.
               ;; (let ((syn (syntax-ppss)))
               ;;   (if (nth 8 syn) ;; If in string or comment...
               ;;       (nth 3 syn) ;; ... only compose if in string.
               ;;     t))
               )))))))

(defconst isabelle-arrows
  '(("leftarrow"          . "<-")
    ("Leftarrow"          . "<=")
    ("rightarrow"         . "->")
    ("Rightarrow"         . "=>")

    ;; These below have width 3
    ("leftrightarrow"     . "<->")
    ("Leftrightarrow"     . "<=>")
    ("longrightarrow"     . "-->")
    ("Longrightarrow"     . "==>")
    ("longleftarrow"      . "<--")
    ("Longleftarrow"      . "<==")

    ;; These below have width 4
    ("longleftrightarrow" . "<-->")
    ("Longleftrightarrow" . "<==>")

    ("up"                 . ?↑)
    ("Up"                 . ?⇑)
    ("down"               . ?↓)
    ("Down"               . ?⇓)
    ("updown"             . ?↕)
    ("Updown"             . ?⇕)

    ("mapsto"             . ?↦)
    ("longmapsto"         . ?⟼)

    ;; ("mapsto"         . "|->")
    ;; ("longmapsto"     . "|-->")
    ;; ("longleftrightarrow" . "<-->")
    ;; ("Longleftrightarrow" . "<==>")

    ;; ("midarrow" "─")
    ;; ("Midarrow" "═")
    ;; ("hookleftarrow" "↩")
    ;; ("hookrightarrow" "↪")
    ;; ("leftharpoondown" "↽")
    ;; ("rightharpoondown" "⇁")
    ;; ("leftharpoonup" "↼")
    ;; ("rightharpoonup" "⇀")
    ;; ("rightleftharpoons" "⇌")
    ;; ("leadsto" "↝")
    ;; ("downharpoonleft" "⇃")
    ;; ("downharpoonright" "⇂")
    ;; ("upharpoonleft" "↿")
    ;; ("restriction" "↾")
    ;; ("up" "↑")
    ;; ("Up" "⇑")
    ;; ("down" "↓")
    ;; ("Down" "⇓")
    ;; ("updown" "↕")
    ;; ("Updown" "⇕")

    ))

(defconst pretty-ligatures--isabelle-all-glyphs
  (eval-when-compile
    (append
     isabelle-arrows
     '(("Colon"    . "::")
       ("bottom"   . "bottom")
       ("forall"   . "forall")
       ("exists"   . "exists")
       ("nexists"  . "nexists")
       ("not"      . "not")
       ("emptyset" . "emptySet")
       ("in"       . "elem")
       ("notin"    . "notElem")
       ("subseteq" . "isSubsetOf")
       ("union"    . ?⋃)
       ("inter"    . ?⋂)

       ;; These two should be taller than n-ary conjunction character (?⋀)
       ("And"      . "&&")
       ("Or"       . "||")

       ("noteq"    . "/=")

       ("and"      . ?⋀) ;; Use n-ary character to take 2 symbols and be more distinctive
       ("or"       . ?⋁)

       ("Sum"      . ?∑)
       ("Prod"     . ?∏)
       ("Coprod"   . ?∐)

       ("comment"  . ?―)
       ("infinity" . ?∞)

       ("equiv"    . "equivalent"))))
  "Mapping to specific ‘Iosevka Slab Lig’ glyphs.")

(defconst pretty-ligatures--isabelle-unicode-ligatures
  '(("alpha"     . ?α)
    ("beta"      . ?β)
    ("gamma"     . ?γ)
    ("delta"     . ?δ)
    ("epsilon"   . ?ε) ;; varepsilon (some is epsilon), although PG can use dups
    ("zeta"      . ?ζ)
    ("eta"       . ?η)
    ("theta"     . ?θ)
    ("iota"      . ?ι)
    ("kappa"     . ?κ)
    ("lambda"    . ?λ)
    ("mu"        . ?μ)
    ("nu"        . ?ν)
    ("xi"        . ?ξ)
    ("pi"        . ?π)
    ("rho"       . ?ρ)
    ("sigma"     . ?σ)
    ("tau"       . ?τ)
    ("upsilon"   . ?υ)
    ("phi"       . ?φ)
    ("chi"       . ?χ)
    ("psi"       . ?ψ)
    ("omega"     . ?ω)
    ("Gamma"     . ?Γ)
    ("Delta"     . ?Δ)
    ("Theta"     . ?Θ)
    ("Lambda"    . ?Λ)
    ("Xi"        . ?Ξ)
    ("Pi"        . ?Π)
    ("Sigma"     . ?Σ)
    ("Upsilon"   . ?Υ)
    ("Phi"       . ?Φ)
    ("Psi"       . ?Ψ)
    ("Omega"     . ?Ω)

    ("AA"        . ?𝔄) ;; 120068 (#o352404, #x1d504)
    ("BB"        . ?𝔅)
    ("CC"        . ?ℭ)
    ("DD"        . ?𝔇)
    ("EE"        . ?𝔈)
    ("FF"        . ?𝔉)
    ("GG"        . ?𝔊)
    ("HH"        . ?ℌ)
    ("II"        . ?ℑ)
    ("JJ"        . ?𝔍)
    ("KK"        . ?𝔎)
    ("LL"        . ?𝔏)
    ("MM"        . ?𝔐)
    ("NN"        . ?𝔑)
    ("OO"        . ?𝔒)
    ("PP"        . ?𝔓)
    ("QQ"        . ?𝔔)
    ("RR"        . ?ℜ)
    ("SS"        . ?𝔖)
    ("TT"        . ?𝔗)
    ("UU"        . ?𝔘)
    ("VV"        . ?𝔙)
    ("WW"        . ?𝔚)
    ("XX"        . ?𝔛)
    ("YY"        . ?𝔜)
    ("ZZ"        . ?ℨ) ;; 8488 (#o20450, #x2128)
    ("aa"        . ?𝔞)
    ("bb"        . ?𝔟)
    ("cc"        . ?𝔠)
    ("dd"        . ?𝔡)
    ("ee"        . ?𝔢)
    ("ff"        . ?𝔣)
    ("gg"        . ?𝔤)
    ("hh"        . ?𝔥)
    ("ii"        . ?𝔦)
    ("jj"        . ?𝔧)
    ("kk"        . ?𝔨)
    ("ll"        . ?𝔩)
    ("mm"        . ?𝔪)
    ("nn"        . ?𝔫)
    ("oo"        . ?𝔬)
    ("pp"        . ?𝔭)
    ("qq"        . ?𝔮)
    ("rr"        . ?𝔯)
    ("ss"        . ?𝔰)
    ("tt"        . ?𝔱)
    ("uu"        . ?𝔲)
    ("vv"        . ?𝔳)
    ("ww"        . ?𝔴)
    ("xx"        . ?𝔵)
    ("yy"        . ?𝔶)
    ("zz"        . ?𝔷)

    ("bool"      . ?𝔹)
    ("complex"   . ?ℂ)
    ("nat"       . ?ℕ)
    ("rat"       . ?ℚ)
    ("real"      . ?ℝ)
    ("int"       . ?ℤ)

    ("dagger"    . ?†)
    ("ddagger"   . ?‡)

    ("nabla"     . ?∇)
    ("partial"   . ?∂)
    ("integral"  . ?∫)
    ("ointegral" . ?∮)
    ("inverse"   . "⁻¹")

    ("open"      . ?‹)
    ("close"     . ?›)

    ("circ"      . ?∘) ;; Isabelle shows compositions as ?○ but it seems to be too large .
    ("bar"       . ?¦)
    ("parallel"  . ?∥)

    ("le"        . ?≤) ;; "LE" takes 2 characters but I want to get the same width as with ASCII <
    ("ge"        . ?≥) ;; "GE" takes 2 characters but I want to get the same width as with ASCII >

    ("times"     . ?×) ;; ?⨉
    ("cdot"      . ?·)

    ("langle"         . ?\⟨)
    ("rangle"         . ?\⟩)
    ("lceil"          . ?\⌈)
    ("rceil"          . ?\⌉)
    ("lfloor"         . ?\⌊)
    ("rfloor"         . ?\⌋)
    ("lparr"          . ?\⦇)
    ("rparr"          . ?\⦈)
    ("lbrakk"         . ?\⟦)
    ("rbrakk"         . ?\⟧)
    ("lbrace"         . ?\⦃)
    ("rbrace"         . ?\⦄)
    ("guillemotleft"  . ?\«)
    ("guillemotright" . ?\»)
    ("bottom"         . ?⊥)
    ("top"            . ?⊤))
  "Mapping to regular unicode characters.")

(defconst pretty-ligatures--isabelle-replacements
  (eval-when-compile
    (append
     (--map (cons (concat "\\<" (car it) ">")
                  (pretty-ligatures--make-glyph-composition (cdr it)))
            pretty-ligatures--isabelle-all-glyphs)
     (--map (cons (concat "\\<" (car it) ">")
                  (pretty-ligatures--make-literal-singleton-composition (cdr it)))
            pretty-ligatures--isabelle-unicode-ligatures))))

;;;###autoload
(defun pretty-ligatures-install-isabelle-ligatures! ()
  (pretty-ligatures--isabelle-install! pretty-ligatures--isabelle-replacements))

(provide 'isabelle-symbols)

;; Local Variables:
;; End:

;; isabelle-symbols.el ends here
