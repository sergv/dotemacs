;; agda-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 14 September 2014
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'abbrev+)

(defun-once agda-abbrev+-make-abbrevs
  (v--map
   (make-abbrev+-abbreviation
    :trigger (car it)
    :action-type 'literal-string
    :action-data (cadr it))
   [("->" "→")
    ("=>" "⇒")
    ("forall" "∀")
    ("alpha" "α")
    ("Alpha" "Α")
    ("beta" "β")
    ("Beta" "Β")
    ("gamma" "γ")
    ("Gamma" "Γ")
    ("delta" "δ")
    ("Delta" "Δ")
    ("epsilon" "ε")
    ("eps" "ε")
    ("Epsilon" "Ε")
    ("Eps" "Ε")
    ("zeta" "ζ")
    ("Zeta" "Ζ")
    ("eta" "η")
    ("Eta" "Η")
    ("theta" "θ")
    ("Theta" "Θ")
    ("iota" "ι")
    ("Iota" "Ι")
    ("kappa" "κ")
    ("Kappa" "Κ")
    ("lambda" "λ")
    ("Lambda" "Λ")
    ("lambda'" "ƛ")
    ("mu" "μ")
    ("Mu" "Μ")
    ("nu" "ν")
    ("Nu" "Ν")
    ("xi" "ξ")
    ("Xi" "Ξ")
    ("pi" "π")
    ("Pi" "Π")
    ("rho" "ρ")
    ("Rho" "Ρ")
    ("sigma" "σ")
    ("Sigma" "Σ")
    ("tau" "τ")
    ("Tau" "Τ")
    ("upsilon" "υ")
    ("Upsilon" "Υ")
    ("phi" "φ")
    ("Phi" "Φ")
    ("chi" "χ")
    ("Chi" "Χ")
    ("psi" "ψ")
    ("Psi" "Ψ")
    ("omega" "ω")
    ("Omega" "Ω")]))

(defun agda-abbrev+-setup ()
  (setf abbrev+-skip-syntax ["w_" "^ >"]
        abbrev+-abbreviations (agda-abbrev+-make-abbrevs))
  (def-keys-for-map vim-insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'agda-abbrev+)

;; Local Variables:
;; End:

;; agda-abbrev+.el ends here
