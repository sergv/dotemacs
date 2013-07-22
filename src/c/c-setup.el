;; c-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'common)
(require 'cc-setup)
(require 'c-abbrev+)
(require 'eproj-setup)

(defun c-setup ()
  (cc-setup :define-special-keys t)

  (cc-setup/set-up-c-basic-offset)

  (setf hs-forward-sexp-func #'c-hideshow-forward-sexp)

  (if-buffer-has-file
    (setq-local compile-command
                (let* ((fname  (file-name-nondirectory buffer-file-name))
                       (target (file-name-sans-extension fname)))
                  (setq compile-command
                        (join-lines (list "gcc"
                                          "-W"
                                          "-Wall"
                                          "-O2"
                                          "-I."
                                          "-o"
                                          target
                                          fname)
                                    " ")))))

  (if-has-makefile-command
   (setq-local compile-command
               (concat "make " (file-name-sans-extension
                                (file-name-nondirectory buffer-file-name)))))

  (def-keys-for-map vim:visual-mode-local-keymap
    (", m" c-macro-expand))
  (c-abbrev+-setup)
  (setup-eproj-symbnav)
  (setup-outline-headers :header-start "/"
                         :header-symbol "*"
                         :length-min 3
                         :length-max 9))

(provide 'c-setup)

;; Local Variables:
;; End:

;; c-setup.el ends here
