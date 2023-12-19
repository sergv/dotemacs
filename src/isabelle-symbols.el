;; isabelle-symbols.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  3 December 2023
;; Description:

(require 'pretty-ligatures)

(defun pretty-ligatures--isabelle-install (ligatures)
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
    (and (not (zerop len))
         (let ((start-char (aref match-str 0))
               (end-char (aref match-str (1- (length match-str)))))
           (or
            ;; Always fontify ligatures of the form "\<...>".
            (and (char-equal start-char ?\\)
                 (char-equal end-char ?\>)
                 (char-equal (aref match-str 1) ?<))
            (let* ((syntaxes-beg (if (memq (char-syntax start-char) '(?w ?_))
                                     '(?w ?_)
                                   '(?. ?\\)))
                   (syntaxes-end (if (memq (char-syntax end-char) '(?w ?_))
                                     '(?w ?_)
                                   '(?. ?\\)))
                   (following-char (or (char-after end) ?\s)))
              (and (not (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg))
                   (or (char-equal following-char ?,)
                       (not (memq (char-syntax following-char) syntaxes-end)))
                   ;; Enable ligatures in both strings and comments for now.
                   ;; (let ((syn (syntax-ppss)))
                   ;;   (if (nth 8 syn) ;; If in string or comment...
                   ;;       (nth 3 syn) ;; ... only compose if in string.
                   ;;     t))
                   )))))))

(defconst isabelle-arrows
  '(("leftarrow"      . "<-")
    ("Leftarrow"      . "<=")
    ("rightarrow"     . "->")
    ("Rightarrow"     . "=>")
    ("leftrightarrow" . "<->")
    ("Leftrightarrow" . "<=>")
    ("longrightarrow" . "-->")
    ("Longrightarrow" . "==>")
    ("longleftarrow"  . "<--")
    ("Longleftarrow"  . "<==")
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
     '(("Colon"          . "::")
       ("bottom"         . "bottom")
       ("forall"         . "forall")
       ("exists"         . "exists")
       ("nexists"        . "nexists")
       ("not"            . "not")
       ("emptyset"       . "emptySet")
       ("Sum"            . "sum")
       ("Prod"           . "product")
       ("Coprod"         . "coproduct"))))
  "Mapping to specific ‘Iosevka Slab Lig’ glyphs.")

(defconst pretty-ligatures--isabelle-unicode-ligatures
  '(
    ("alpha" . ?α)
    ("beta" . ?β)
    ("gamma" . ?γ)
    ("delta" . ?δ)
    ("epsilon" . ?ε) ; varepsilon (some is epsilon), although PG can use dups
    ("zeta" . ?ζ)
    ("eta" . ?η)
    ("theta" . ?θ)
    ("iota" . ?ι)
    ("kappa" . ?κ)
    ("lambda" . ?λ)
    ("mu" . ?μ)
    ("nu" . ?ν)
    ("xi" . ?ξ)
    ("pi" . ?π)
    ("rho" . ?ρ)
    ("sigma" . ?σ)
    ("tau" . ?τ)
    ("upsilon" . ?υ)
    ("phi" . ?φ)
    ("chi" . ?χ)
    ("psi" . ?ψ)
    ("omega" . ?ω)
    ("Gamma" . ?Γ)
    ("Delta" . ?Δ)
    ("Theta" . ?Θ)
    ("Lambda" . ?Λ)
    ("Xi" . ?Ξ)
    ("Pi" . ?Π)
    ("Sigma" . ?Σ)
    ("Upsilon" . ?Υ)
    ("Phi" . ?Φ)
    ("Psi" . ?Ψ)
    ("Omega" . ?Ω)

    ("open"  . ?‹)
    ("close" . ?›))
  "Mapping to regular unicode characters.")

(defconst pretty-ligatures--isabelle-replacements
  (eval-when-compile
    (append
     (--map (cons (concat "\\<" (car it) ">")
                  (pretty-ligatures--make-glyph-composition (cdr it)))
            pretty-ligatures--isabelle-all-glyphs)
     (--map (cons (concat "\\<" (car it) ">")
                  (pretty-ligatures--make-literal-composition (cdr it)))
            pretty-ligatures--isabelle-unicode-ligatures))))

;;;###autoload
(defun pretty-ligatures-install-isabelle-ligatures! ()
  (pretty-ligatures--isabelle-install pretty-ligatures--isabelle-replacements))

(provide 'isabelle-symbols)

;; Local Variables:
;; End:

;; isabelle-symbols.el ends here
