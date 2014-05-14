;; agda-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 14 May 2014
;; Description:

(require 'common)
(require 'agda2)

(defun agda-setup ()
  (init-common :use-yasnippet t
               :use-nxhtml-menu nil
               :use-comment t
               :use-render-formula t)
  (def-keys-for-map vim:normal-mode-local-keymap
    ("<f6>"            agda2-load)
    ("<tab>"           eri-indent)
    ("S-<tab>"         eri-indent-reverse)
    ("S-<lefttab>"     eri-indent-reverse)
    ("S-<iso-lefttab>" eri-indent-reverse)
    ("M-."             agda2-goto-definition-keyboard)
    ("M-,"             agda2-go-back))
  (def-keys-for-map vim:visual-mode-local-keymap
    ("j"               agda2-compute-normalised-region)))

(add-hook 'agda2-mode-hook #'agda-setup)

(defun agda2-compute-normalised-region (&optional arg)
  "Computes the normal form of the given expression.
The scope used for the expression is that of the last point inside the current
top-level module.
With a prefix argument \"abstract\" is ignored during the computation."
  (interactive "P")
  (assert (region-active?))
  (multiple-value-bind (start end) (get-region-bounds)
    (let* ((expr (buffer-substring-no-properties start end))
           (cmd (concat "Cmd_compute_toplevel"
                        (if arg " True" " False")
                        " ")))
      (agda2-go t nil (concat cmd (agda2-string-quote expr))))))

(provide 'agda-setup)

;; Local Variables:
;; End:

;; agda-setup.el ends here
