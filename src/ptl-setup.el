;; ptl-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  4 December 2023
;; Description:

(eval-when-compile
  (require 'macro-util)
  (require 'vim-macs))

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
(progn
  (put 'vim:ptl-backward-up-indentation-or-sexp 'type 'exclusive)
  (put 'vim:ptl-backward-up-indentation-or-sexp 'count nil)
  (put 'vim:ptl-backward-up-indentation-or-sexp 'argument 'nil)
  (defun vim:ptl-backward-up-indentation-or-sexp:raw
      (_ignored-motion _ignored-count _ignored-argument _ignored-force
                       _ignored-register)
    "Vimmized version of ‘ptl-backward-up-indentation-or-sexp’."
    (let nil (ptl-backward-up-indentation-or-sexp)))
  (defun vim:ptl-backward-up-indentation-or-sexp
      (_ignored-motion _ignored-count _ignored-argument _ignored-force
                       _ignored-register)
    "Vimmized version of ‘ptl-backward-up-indentation-or-sexp’."
    (vim-wrap-motion exclusive
      (vim:ptl-backward-up-indentation-or-sexp:raw nil nil nil nil nil)))
  (cl-defmacro vim:ptl-backward-up-indentation-or-sexp:wrapper nil
    "Vimmized version of ‘ptl-backward-up-indentation-or-sexp’."
    (list 'vim:ptl-backward-up-indentation-or-sexp nil nil nil nil nil))
  (defun vim:ptl-backward-up-indentation-or-sexp:interactive nil
    "Interactive version of ‘vim:ptl-backward-up-indentation-or-sexp’"
    (interactive)
    (let ((vim-do-not-adjust-point t))
      (if vim-active-mode
          (vim-execute-command
           #'vim:ptl-backward-up-indentation-or-sexp)
        (vim:ptl-backward-up-indentation-or-sexp:raw nil nil nil nil
                                                     nil)))))

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
  (setf vim-shift-width tab-width)

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
