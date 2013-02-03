;; clojure-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  3 February 2013
;; Description:

(add-to-list 'load-path (concat +emacs-standalone-path+
                                "/clojure-mode"))
(add-to-list 'load-path (concat +emacs-standalone-path+
                                "/nrepl.el"))
(require 'clojure-mode)
(require 'clojure-test-mode)
(require 'nrepl)
(require 'clojure-abbrev+)

(setf nrepl-tab-command 'indent-for-tab-command
      nrepl-history-size 100000
      nrepl-history-file (concat +prog-data-path+ "/nrepl-history"))

(defun clojure-setup ()
  (lisp-setup :use-cl-indent nil)
  (clojure-enable-nrepl)

  (set (make-local-variable 'forward-sexp-function)
       'clojure-forward-sexp)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC" nrepl-switch-to-repl-buffer)
    ("M-/"     complete-symbol)
    ("<f1>"    nrepl-load-current-buffer ;; clojure-load-file
               )

    ("M-."     nrepl-jump)
    ("M-,"     nrepl-jump-back)

    (", j d"   nrepl-javadoc)
    (", d"     nrepl-doc)
    (", m"     nrepl-macroexpand-1)
    (", M"     nrepl-macroexpand-all)

    ("j"       nrepl-eval-last-expression))
  (clojure-abbrev+-setup))

(add-hook 'clojure-mode-hook #'clojure-setup)


(defun nrepl-setup ()
  (init-common :use-yasnippet nil
               :use-whitespace nil
               :use-render-formula nil)
  (init-repl)

  (rainbow-delimiters-mode 1)
  (hs-minor-mode 1)
  (setq-local hs-hide-comments-when-hiding-all nil)
  (enable-paredit-mode)

  (setq-local comment-style 'indent)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local comment-padding " ")

  (setq-local comint-use-prompt-regexp nil)
  (setq-local comint-prompt-regexp "^[^> \n\t\r\f\v]*\\(>+:?\\|[*?]+\\) *")

  (setf vim:normal-mode-local-keymap           (make-keymap)
        vim:visual-mode-local-keymap           (make-sparse-keymap)
        vim:insert-mode-local-keymap           (make-sparse-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap)
        vim:motion-mode-local-keymap           (make-sparse-keymap)))

(add-hook 'nrepl-mode-hook #'nrepl-setup)

(provide 'clojure-setup)

;; Local Variables:
;; End:

;; clojure-setup.el ends here
