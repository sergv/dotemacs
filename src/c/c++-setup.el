;; c++-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'cc-setup)
(require 'c++-abbrev+)


(when (platform-os-type? 'windows)
  (defvar *c++-related-file-cache*
    (make-hash-table :test 'equal))

  (defun c++-find-related-file ()
    (interactive)
    (let* ((filename (buffer-file-name (current-buffer)))
           (path (split-string filename "/"))
           (ext (file-name-extension filename))
           (file-nodir (file-name-nondirectory (last path))))
      (aif (gethash *c++-related-file-cache* filename nil)
        (find-file it)
        (letrec ((path-join (lambda (path)
                              (mapconcat #'identity path "/")))
                 (find-subroot
                   (lambda (path look-for-dir)
                     (let ((dir (funcall path-join
                                         (append path
                                                 (list look-for-dir)))))
                       (cond ((null path)
                              (error "No %s subdirectory found while moving upward starting from %s"
                                     (file-name-directory filename)))
                             ((file-exists? dir)
                              path)
                             (t
                              (funcall find-subroot
                                       (butlast path)
                                       look-for-dir)))))))
          ;; note: subroot - root of some git submodule

          (let* ((subroot (find-subroot path
                                        (cond ((string= ext "h") "src")
                                              ((string= ext "inc")
                                               (file-name-directory (last path)))
                                              ((string= ext "cpp") "include"))))
                 (alt-ext (cond ((string= ext "h") "cpp")
                                ((string= ext "inc") "h")
                                ((string= ext "cpp") "h")))
                 (alternative-name
                   (concat (file-name-sans-extension file-nodir)
                           "."
                           alt-ext))
                 (aif (find-rec subroot-dir
                                :filep (lambda (p)
                                         (string= alternative-name
                                                  (file-name-nondirectory p))))
                   (progn
                     (assert (= 1 (length it)))
                     (puthash filename (car it) *c++-related-file-cache*)
                     (find-file (car it)))
                   (error "No *.%s file found for %s" alt-ext filename)))))))))



(defun c++-setup ()
  (cc-setup :define-special-keys t)

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

  (when (platform-os-type? 'windows)
    (def-keys-for-map vim:normal-mode-local-keymap
      ("SPC SPC" c++-find-related-file)))

  (c++-abbrev+-setup))


(provide 'c++-setup)

;; c++-setup.el ends here
