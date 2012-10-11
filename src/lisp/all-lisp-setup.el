;; all-lisp-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


;;;

(require 'cl)
;; (require 'cl-mode)

(defconst +common-lisp-file-extensions+
  '("l" "cl" "asd" "lsp" "lisp" "clisp")
  "List of common lisp file extensions")

(defconst +scheme-file-extensions+
  '("scm" "sc" "stk" "ss" "sch" "oak")
  "List of scheme file extensions")

(setf auto-mode-alist
      (append
       (list (cons (eval `(rx "." (or ,@+common-lisp-file-extensions+) eot))
                   'lisp-mode)
             (cons (rx (or ".sbclrc"
                           ".ecl"
                           ".eclrc") eol)
                   'lisp-mode)
             (cons (eval `(rx "." (or ,@+scheme-file-extensions+) eot))
                   'scheme-mode))
       (remove-if (lambda (x)
                    (member x '(scheme-mode
                                lisp-mode
                                common-lisp-mode
                                cl-mode)))
                  auto-mode-alist
                  :key #'cdr)))

;; (defalias 'lisp-mode #'cl-mode)
;; (defalias 'common-lisp-mode #'cl-mode)

(add-hook 'lisp-mode-hook #'ansi-lisp-highlight-keywords)

(autoload 'lisp-setup "general-lisp-setup")
(autoload 'lisp-repl-setup "general-lisp-setup")

;; required by common lisp
(autoload 'turn-on-cldoc-mode "cldoc" nil t)

(autoload 'common-lisp-setup "common-lisp-setup")
(autoload 'slime-interpreter-setup "common-lisp-setup")
(autoload 'slime-repl-setup "common-lisp-setup")
(autoload 'slime-sldb-setup "common-lisp-setup")
(autoload 'slime-inspector-setup "common-lisp-setup")
(autoload 'slime-fuzzy-completions-setup "common-lisp-setup")
(autoload 'slime-xref-setup "common-lisp-setup")

(add-hook 'lisp-mode-hook #'common-lisp-setup)
(add-hook 'lisp-interaction-mode-hook #'lisp-repl-setup)
(add-hook 'cl-mode-hook #'common-lisp-setup)

(add-hook 'slime-inferior-process-start-hook #'slime-interpreter-setup)
(add-hook 'slime-repl-mode-hook              #'slime-repl-setup)
(add-hook 'sldb-mode-hook                    #'slime-sldb-setup)
(add-hook 'slime-inspector-mode-hook         #'slime-inspector-setup)
(add-hook 'slime-fuzzy-completions-mode-hook #'slime-fuzzy-completions-setup)
(add-hook 'slime-xref-mode-hook              #'slime-xref-setup t)


(defconst scheme-use-slime? nil)

(if scheme-use-slime?
  (autoload 'scheme-setup "scheme-setup-with-slime")
  (autoload 'scheme-setup "scheme-setup"))
(autoload 'scheme-repl-setup "scheme-setup")

(add-hook 'scheme-mode-hook #'scheme-setup)
(add-hook 'inferior-scheme-mode-hook #'scheme-repl-setup)

;; (require 'scheme-setup)


;; defined here becouse it's used throughout outher lisp packages
(defvar *elisp-do-not-move-files*
  '(".emacs"
    "dotemacs.el"
    "emacs-general-conf.el")
  "List of file names that should be moved to `+bytecode-lib+'.")

(autoload 'emacs-lisp-setup "emacs-lisp-setup")

(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-setup)

;; (require 'emacs-lisp-setup)

(eval-after-load
 'slime
 '(begin
   (add-to-list 'slime-lisp-modes 'cl-mode)
   (when scheme-use-slime?
     (add-to-list 'slime-lisp-modes 'scheme-mode))))



(provide 'lisp-setup)

;; Local Variables:
;; End:

;; all-lisp-setup.el ends here
