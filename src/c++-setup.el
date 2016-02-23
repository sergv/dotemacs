;; c++-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'common)
(require 'cc-setup)
(require 'c++-abbrev+)
(require 'select-mode)
(require 'eproj-setup)
(require 'clang-format)

(defparameter *c++-related-file-cache*
  (make-hash-table :test 'equal))

(defun c++-find-related-file ()
  (interactive)
  (let* ((filename   buffer-file-name)
         (path       (split-string filename "/"))
         (ext        (file-name-extension filename))
         (file-dir   (file-name-directory filename))
         (file-nodir (file-name-nondirectory (car (last path)))))
    (aif (gethash filename *c++-related-file-cache* nil)
      (find-file it)
      (letrec ((path-join (lambda (path) (join-lines path "/")))
               (find-subroot
                (lambda (path needle)
                  (let ((dir (funcall path-join
                                      (append path
                                              (list needle)))))
                    (cond ((null path)
                           (error "No %s subdirectory found while moving upward starting from %s"
                                  needle
                                  file-dir))
                          ((file-exists? dir)
                           path)
                          (t
                           (funcall find-subroot
                                    (butlast path)
                                    needle)))))))
        (let* ((header-exts '("h" "hh" "hpp" "hxx"))
               (inline-exts '("inc" "inl" "incl"))
               (source-exts '("cc" "cc" "cpp" "cxx"))
               (alt-exts (cond ((member ext header-exts)
                                source-exts)
                               ((or (member ext inline-exts)
                                    (member ext source-exts))
                                header-exts)
                               (t
                                nil)))
               (alternative-names
                (--map (concat (file-name-sans-extension file-nodir)
                               "."
                               it)
                       alt-exts))
               (alt-names-in-same-dir
                (--map (concat file-dir "/" it)
                       alternative-names)))
          (aif (find-if #'file-exists? alt-names-in-same-dir)
            (progn
              (puthash filename it *c++-related-file-cache*)
              (puthash it filename *c++-related-file-cache*)
              (find-file it))
            ;; note: subroot - root of some git submodule
            (let ((subroot (funcall find-subroot
                                    path
                                    (cond ((member ext header-exts)
                                           "src")
                                          ((member ext inline-exts)
                                           (file-name-directory (car (last path))))
                                          ((member ext source-exts)
                                           "include")))))
              (aif (find-rec (funcall path-join subroot)
                             :filep (lambda (p)
                                      (string= alternative-name
                                               (file-name-nondirectory p))))
                (pcase it
                  (`(,related)
                   (puthash filename related *c++-related-file-cache*)
                   (puthash related filename *c++-related-file-cache*)
                   (find-file related))
                  (choices
                   (select-start-selection
                    choices
                    :buffer-name "select file"
                    :on-selection
                    (lambda (idx selection-type)
                      (let ((alt-file (elt choices idx)))
                        (select-exit)
                        (puthash filename alt-file *c++-related-file-cache*)
                        (puthash alt-file filename *c++-related-file-cache*)
                        (funcall
                         (pcase selection-type
                           (`same-window  #'find-file)
                           (`other-window #'find-file-other-window))
                         alt-file)))
                    :item-show-function
                    (lambda (x) (concat x "\n"))
                    :preamble-function
                    (lambda () (concat "Select desired alternative file\n"))
                    :separator-function
                    (apply-partially #'select-make-bold-separator
                                     "--------\n"))))
                (error "No %s file found for %s"
                       (--map (concat "*." it) alt-exts)
                       filename)))))))))

(defun c++-indentation-indent-buffer ()
  (clang-format-buffer))

(defun c++-setup ()
  (cc-setup :define-special-keys t)
  (cc-setup/set-up-c-basic-offset :use-work-code-style t)
  (setf hs-forward-sexp-func #'c-hideshow-forward-sexp)
  (setq-local indent-tabs-mode nil)
  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC" c++-find-related-file))
  (c++-abbrev+-setup)
  (setup-eproj-symbnav))


(provide 'c++-setup)

;; c++-setup.el ends here
