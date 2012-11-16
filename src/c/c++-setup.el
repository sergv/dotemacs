;; c++-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'cc-setup)
(require 'c++-abbrev+)


(when (platform-use? 'work)
  (defvar *c++-related-file-cache*
    (make-hash-table :test 'equal))

  (defun c++-find-related-file ()
    (interactive)
    (let* ((filename (buffer-file-name (current-buffer)))
           (path (split-string filename "/"))
           (ext (file-name-extension filename))
           (file-dir (file-name-directory filename))
           (file-nodir (file-name-nondirectory (car (last path)))))
      (aif (gethash filename *c++-related-file-cache* nil)
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
                                     file-dir))
                             ((file-exist? dir)
                              path)
                             (t
                              (funcall find-subroot
                                       (butlast path)
                                       look-for-dir)))))))
          ;; note: subroot - root of some git submodule

          (let* ((subroot (funcall find-subroot
                                   path
                                   (cond ((string= ext "h") "src")
                                         ((or (string= ext "inc")
                                              (string= ext "inl")
                                              (string= ext "incl"))
                                          (file-name-directory (car (last path))))
                                         ((string= ext "cpp") "include"))))
                 (alt-ext (cond ((string= ext "h") "cpp")
                                ((or (string= ext "inc")
                                     (string= ext "inl")
                                     (string= ext "incl"))
                                 "h")
                                ((string= ext "cpp") "h")))
                 (alternative-name
                   (concat (file-name-sans-extension file-nodir)
                           "."
                           alt-ext))
                 (alt-name-in-same-dir (concat file-dir "/" alternative-name)))
            (if (file-exist? alt-name-in-same-dir)
              (progn
                (puthash filename alt-name-in-same-dir *c++-related-file-cache*)
                (puthash alt-name-in-same-dir filename *c++-related-file-cache*)
                (find-file alt-name-in-same-dir))
              (aif (find-rec (funcall path-join subroot)
                             :filep (lambda (p)
                                      (string= alternative-name
                                               (file-name-nondirectory p))))
                (progn
                  (assert (= 1 (length it)))
                  (puthash filename (car it) *c++-related-file-cache*)
                  (puthash (car it) filename *c++-related-file-cache*)
                  (find-file (car it)))
                (error "No *.%s file found for %s" alt-ext filename)))))))))



(defun c++-setup ()
  (cc-setup :define-special-keys t
            :use-c-eldoc nil ;; c-eldoc is too unreliable and too slow for C++
            )

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

  (when (platform-use? 'work)
    (def-keys-for-map vim:normal-mode-local-keymap
      ("SPC SPC" c++-find-related-file)))

  (c++-abbrev+-setup))


(provide 'c++-setup)

;; c++-setup.el ends here
