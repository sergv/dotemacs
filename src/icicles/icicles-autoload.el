;; icicles-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 18 May 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(autoload 'icy-mode "icicles" "" t)
(autoload 'icicle-file "icicles" "" t)
(autoload 'icicle-locate-file "icicles" "" t)
(autoload 'icicle-locate-file-other-window "icicles" "" t)
(autoload 'icicle-buffer "icicles" "" t)
(autoload 'icicle-read-file-name "icicles" "" t)
(autoload 'icicle-completing-read "icicles" "" t)
(autoload 'icicle-read-string "icicles" "" t)
(autoload 'icicle-shell-command "icicles" "" t)
(autoload 'icicle-pp-eval-expression "icicles" "" t)
(autoload 'icicle-pp-eval-expression-in-minibuffer "icicles" "" t)
(autoload 'icicle-delete-window "icicles" "" t)

(autoload 'completion-list-setup "icicles-setup")
(add-hook 'completion-list-mode-hook #'completion-list-setup)

(eval-after-load 'icicles
  '(progn
     (load "icicles-setup")))
(load-library "icicles-opt")


(require 'icicles-util)

(def-keys-for-map minibuffer-local-completion-map
  ("<prior>" nil)
  ("<next>"  nil))

(dolist (map (list minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-filename-completion-map
                   minibuffer-local-filename-must-match-map
                   minibuffer-local-isearch-map

                   minibuffer-inactive-mode-map))
  (icicles-util/bind-minibuffer-keys map :sexp-keys nil))
(icicles-util/bind-minibuffer-keys minibuffer-local-map :sexp-keys t)

(def-keys-for-map completion-list-mode-map
  +vim-special-keys+
  ("<up>"     previous-completion)
  ("<down>"   next-completion)
  ("<escape>" remove-buffer))

(eval-after-load 'icicles
  '(progn
     (icicles-util/bind-minibuffer-keys icicle-read-expression-map :sexp-keys t)

     (def-keys-for-map icicle-read-expression-map
       ("C-/" lisp-complete-symbol))))

(provide 'icicles-autoload)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; icicles-autoload.el ends here
