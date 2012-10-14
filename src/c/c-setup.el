;; c-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'cc-setup)
(require 'c-abbrev+)

(defun c-setup ()
  (cc-setup :define-special-keys t)

  (if-buffer-has-file
   (set (make-local-variable 'compile-command)
        (let* ((fname  (file-name-nondirectory buffer-file-name))
               (target (file-name-sans-extension fname)))
          (setq compile-command
                (mapconcat #'identity
                           (list "gcc"
                                 "-W"
                                 "-Wall"
                                 "-O2"
                                 "-I."
                                 "-o"
                                 target
                                 fname)
                           " ")))))

  (if-has-makefile-command
   (set (make-local-variable 'compile-command)
        (concat "make " (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name)))))

  (c-abbrev+-setup))

(provide 'c-setup)

;; Local Variables:
;; End:

;; c-setup.el ends here
