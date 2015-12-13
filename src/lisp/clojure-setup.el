;; clojure-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  3 February 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'general-lisp-setup)
(require 'clojure-mode)
(require 'kibit)
(require 'clojure-abbrev+)
(require 'clojure-compile)
(require 'browse-kill-ring-setup)
(require 'eproj-setup)

(setf clojure-max-backtracking 10)

(defun clojure-setup ()
  (lisp-setup :use-fci t)
  (setq-local lisp-indent-function #'clojure-indent-function)
  (setq-local forward-sexp-function #'clojure-forward-logical-sexp)
  ;; (setq-local forward-sexp-function #'forward-sexp)
  (def-keys-for-map (vim:insert-mode-local-keymap
                     vim:normal-mode-local-keymap)
    ("M-/"    complete-symbol)
    ("<f9>"   clojure-compile)
    ("S-<f9>" kibit))

  (setup-eproj-symbnav)
  (clojure-abbrev+-setup))

(provide 'clojure-setup)

;; Local Variables:
;; End:

;; clojure-setup.el ends here
