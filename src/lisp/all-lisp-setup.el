;; all-lisp-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


;;;

(eval-when-compile (require 'cl-lib))

;;; old lisps

(defconst +scheme-file-extensions+
  '("scm" "sc" "stk" "ss" "sch" "oak")
  "List of scheme file extensions")

(defconst +lisp-modes+
  '(emacs-lisp-mode
    common-lisp-mode
    scheme-mode
    clojure-mode
    lisp-mode)
  "List of modes that are considered to be lisp.")

(setf auto-mode-alist
      (cons (cons (rx "." (or "lisp" "asd" "asdf") eot)
                  'common-lisp-mode)
            (cons (cons (eval `(rx "." (or ,@+scheme-file-extensions+) eot))
                        'scheme-mode)
                  (filter (comp #'not
                                (partial-first #'memq '(scheme-mode common-lisp-mode))
                                #'cdr)
                          auto-mode-alist))))

(autoload 'lisp-setup "general-lisp-setup")
(autoload 'lisp-repl-setup "general-lisp-setup")

(autoload 'common-lisp-setup "common-lisp-setup")
(autoload 'common-lisp-compile-and-load-file "common-lisp-setup")

(add-hook 'lisp-mode-hook #'common-lisp-setup)

(autoload 'scheme-setup "scheme-setup")
(autoload 'scheme-repl-setup "scheme-setup")

(add-hook 'scheme-mode-hook #'scheme-setup)
(add-hook 'inferior-scheme-mode-hook #'scheme-repl-setup)

(autoload 'emacs-lisp-setup "emacs-lisp-setup")
(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-setup)

(autoload 'lisp-pos-is-beginning-of-sexp? "general-lisp-setup")
(autoload 'lisp-pos-is-end-of-sexp? "general-lisp-setup")


;;; clojure

(add-to-list 'load-path (concat +emacs-standalone-path+
                                "/clojure-mode"))
(add-to-list 'load-path (concat +emacs-standalone-path+
                                "/nrepl.el"))

(require 'clojure-mode-autoload)
(require 'nrepl-autoload)

(setf nrepl-tab-command 'indent-for-tab-command
      nrepl-history-size 100000
      nrepl-history-file (concat +prog-data-path+ "/nrepl-history"))

(put 'nrepl-server-command 'safe-local-variable #'string?)
(make-variable-buffer-local 'nrepl-server-command)


(autoload 'clojure-setup "clojure-setup")
(add-hook 'clojure-mode-hook #'clojure-setup)

(autoload 'nrepl-setup "clojure-setup")
(add-hook 'nrepl-mode-hook #'nrepl-setup)

(provide 'all-lisp-setup)

;; Local Variables:
;; End:

;; all-lisp-setup.el ends here
