;; c++-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'c-like-setup)
(require 'c++-abbrev+)

(defun c++-setup ()
  (c-like-setup)

  (if-buffer-has-file
   (set (make-local-variable 'compile-command)
        (let* ((fname  (file-name-nondirectory buffer-file-name))
               (target (file-name-sans-extension fname)))
          (mapconcat #'identity
                     (list "g++"
                           ;; "-std=c++0x"
                           "-W"
                           "-Wall"
                           "-Wextra"
                           "-Weffc++"
                           "-Wold-style-cast"
                           "-Woverloaded-virtual"
                           "-Wconversion"
                           "-Wuninitialized"
                           "-Wshadow"
                           "-pedantic"
                           "-O2"
                           "-I."
                           "-o"
                           target
                           fname)
                     " "))))

  (if-has-makefile-command
   (set (make-local-variable 'compile-command)
        (concat "make " (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name)))))

  (c++-abbrev+-setup))


(provide 'c++-setup)

;; c++-setup.el ends here
