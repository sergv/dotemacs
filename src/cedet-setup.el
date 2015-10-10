;; cedet-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 22 April 2012
;; Description:


(load-library (concat +emacs-standalone-path+
                      "/cedet-1.1/common/cedet.el"))


(defun wisent-grammar-mode-setup ()
  (init-common :use-yasnippet nil
               :use-whitespace 'tabs-only)

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("`"    semantic-grammar-create-package)
    ("<f9>" semantic-grammar-create-package))
  (def-keys-for-map vim:normal-mode-local-keymap
    ("j"     eval-last-sexp)
    ("J"     eval-print-last-sexp-unlimited-length)))

(add-hook 'wisent-grammar-mode-hook #'wisent-grammar-mode-setup)

;;
;; (semantic-load-enable-gaudy-code-helpers)
;; ;; (global-srecode-minor-mode 1)
;; (global-semantic-decoration-mode -1)
;; (global-semantic-stickyfunc-mode -1)
;;
;; (require 'semantic-ia)
;;
;; (setf semanticdb-default-save-directory
;;       "/home/sergey/emacs/prog-data/cedet/")
;;
;; (require 'semanticdb)
;; (global-semanticdb-minor-mode 1)
;;
;;
;; ;; (semanticdb-create-ebrowse-database "/usr/include")

(provide 'cedet-setup)

;; Local Variables:
;; End:

;; cedet-setup.el ends here
