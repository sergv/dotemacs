;; clojure-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  3 February 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'general-lisp-setup)
(require 'clojure-mode)
(require 'clojure-test-mode)
(require 'nrepl)
(require 'kibit)
(require 'clojure-abbrev+)
(require 'clojure-compile)
(require 'browse-kill-ring-setup)
(require 'eproj-setup)

(setf clojure-max-backtracking 10)

(defun clojure-setup ()
  (lisp-setup :use-cl-indent nil :use-whitespace t)
  (clojure-enable-nrepl)

  (nrepl-reset-buffer-ns!)

  (setq-local lisp-indent-function #'clojure-indent-function)
  (setq-local forward-sexp-function #'clojure-forward-sexp)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC" nrepl-switch-to-repl-buffer)

    (", j d"   nrepl-javadoc)
    (", d"     nrepl-doc)
    (", m"     nrepl-macroexpand-1)
    (", M"     nrepl-macroexpand-all)

    ("j"       nrepl-eval-last-expression))
  (def-keys-for-map (vim:insert-mode-local-keymap
                     vim:normal-mode-local-keymap)
    ("M-/"     complete-symbol)
    ("<f6>"    nrepl-load-current-buffer ;; clojure-load-file
     )
    ("<f9>"    clojure-compile)
    ("`"       clojure-compile)
    ("S-<f9>"  kibit)
    ("C-`"     kibit))

  (setup-eproj-symbnav)
  (clojure-abbrev+-setup))

(def-keys-for-map nrepl-mode-map
  ("M-p" nil)
  ("M-n" nil))

(defun nrepl-setup ()
  (init-common :use-yasnippet nil
               :use-whitespace nil
               :use-render-formula nil)
  (init-repl :create-keymaps nil)

  (clojure-mode-font-lock-setup)

  (rainbow-delimiters-mode 1)
  (hs-minor-mode -1)
  (enable-paredit-mode)

  (setq-local comment-style 'indent)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local comment-padding " ")

  (setq-local comint-use-prompt-regexp nil)
  (setq-local comint-prompt-regexp "^[^> \n\t\r\f\v]*\\(>+:?\\|[*?]+\\) *")

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap)
    *lisp-search-keybindings*)
  (def-keys-for-map vim:normal-mode-local-keymap
    *lisp-vim-normal-mode-keybindings*
    ("SPC SPC"  nrepl-delete-current-input)

    (", j d"    nrepl-javadoc)
    (", d"      nrepl-doc)
    (", m"      nrepl-macroexpand-1)
    (", M"      nrepl-macroexpand-all)

    ("j"        nrepl-eval-last-expression))
  (def-keys-for-map (vim:insert-mode-local-keymap
                     vim:normal-mode-local-keymap)
    ("C-SPC"    comint-clear-buffer-above-prompt)

    ("M-/"      complete-symbol)
    ("<up>"     nrepl-previous-input)
    ("<down>"   nrepl-next-input)
    ("S-<up>"   nrepl-previous-prompt)
    ("S-<down>" nrepl-next-prompt)

    ("M-."      nrepl-jump)
    ("M-,"      nrepl-jump-back)
    ("C-M-p"    browse-nrepl-input-history))
  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap
                     vim:operator-pending-mode-local-keymap
                     vim:motion-mode-local-keymap)
    *lisp-vim-movement-keybindings*))

(provide 'clojure-setup)

;; Local Variables:
;; End:

;; clojure-setup.el ends here
