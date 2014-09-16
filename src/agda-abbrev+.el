;; agda-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 14 September 2014
;; Description:

(require 'abbrev+)

(defun agda-abbrev+-setup ()
  (setf abbrev+-skip-syntax '("w_" "^ >")
        abbrev+-abbreviations
        (list
         (list "->" "→")
         (list "=>" "⇒")
         (list "forall" "∀")
         (list "alpha" "α")
         (list "Alpha" "Α")
         (list "beta" "β")
         (list "Beta" "Β")
         (list "gamma" "γ")
         (list "Gamma" "Γ")
         (list "delta" "δ")
         (list "Delta" "Δ")
         (list "epsilon" "ε")
         (list "eps" "ε")
         (list "Epsilon" "Ε")
         (list "Eps" "Ε")
         (list "zeta" "ζ")
         (list "Zeta" "Ζ")
         (list "eta" "η")
         (list "Eta" "Η")
         (list "theta" "θ")
         (list "Theta" "Θ")
         (list "iota" "ι")
         (list "Iota" "Ι")
         (list "kappa" "κ")
         (list "Kappa" "Κ")
         (list "lambda" "λ")
         (list "Lambda" "Λ")
         (list "lambda'" "ƛ")
         (list "mu" "μ")
         (list "Mu" "Μ")
         (list "nu" "ν")
         (list "Nu" "Ν")
         (list "xi" "ξ")
         (list "Xi" "Ξ")
         (list "pi" "π")
         (list "Pi" "Π")
         (list "rho" "ρ")
         (list "Rho" "Ρ")
         (list "sigma" "σ")
         (list "Sigma" "Σ")
         (list "tau" "τ")
         (list "Tau" "Τ")
         (list "upsilon" "υ")
         (list "Upsilon" "Υ")
         (list "phi" "φ")
         (list "Phi" "Φ")
         (list "chi" "χ")
         (list "Chi" "Χ")
         (list "psi" "ψ")
         (list "Psi" "Ψ")
         (list "omega" "ω")
         (list "Omega" "Ω")))
  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'agda-abbrev+)

;; Local Variables:
;; End:

;; agda-abbrev+.el ends here
