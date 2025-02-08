;; ptl-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  4 December 2023
;; Description:

;;;###autoload
(defun ptl-backward-up-indentation-or-sexp ()
  "PTL brother of ‘paredit-backward-up’ that considers both
sexps and indentation levels."
  (interactive)
  (indent-backward-up-indentation-or-sexp #'indent-on-blank-line?))

(vimmize-motion ptl-backward-up-indentation-or-sexp
                :name vim:ptl-backward-up-indentation-or-sexp
                :exclusive t
                :unadjusted t
                :raw-result t)

;;;###autoload
(defun ptl-up-sexp ()
  "PTL brother of ‘paredit-forward-up’ that considers only sexps for now."
  (interactive)
  (paredit-forward-up))

(vimmize-motion ptl-up-sexp
                :name vim:ptl-up-sexp
                :exclusive t
                :unadjusted t
                :raw-result t)

;;;###autoload
(defun ptl-mode-setup ()
  (init-common :use-comment t
               :use-whitespace 'tabs-and-trailing-only)
  (setup-hideshow-yafolding t nil)

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-visual-mode-local-keymap
                     vim-motion-mode-local-keymap
                     vim-operator-pending-mode-local-keymap)
    ("'" vim:ptl-backward-up-indentation-or-sexp:interactive)
    ("q" vim:ptl-up-sexp:interactive)))

;;;###autoload
(add-hook 'ptl-mode-hook #'ptl-mode-setup)

(provide 'ptl-setup)

;; Local Variables:
;; End:

;; ptl-setup.el ends here
