;; c++-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'c++-abbrev+)
(require 'cc-setup)
(require 'clang-format)
(require 'common)
(require 'indentation)
(require 'select-mode)

(defparameter *c++-related-file-cache*
  (make-hash-table :test 'equal))

;;;###autoload
(defconst +c-header-exts+ '("h"))
;;;###autoload
(defconst +c-source-exts+ '("c"))

;;;###autoload
(defconst +cpp-header-exts+
  (append +c-header-exts+
          '("hh" "hxx" "hpp" "h++" "inl" "inc" "incl" "ino")))
;;;###autoload
(defconst +cpp-source-exts+
  (append +c-source-exts+
          '("cc" "cxx" "cpp" "c++")))

;;;###autoload
(defconst +c-extensions+
  (append +c-header-exts+ +c-source-exts+))
;;;###autoload
(defconst +cpp-extensions+
  (append +cpp-header-exts+ +cpp-source-exts+))

(defun c++-find-related-file ()
  (interactive)
  (let* ((filename   buffer-file-name))
    (aif (gethash filename *c++-related-file-cache* nil)
        (find-file it)
      (let* ((path       (split-string filename "/"))
             (ext        (file-name-extension filename))
             (file-dir   (file-name-directory filename))
             (file-nodir (file-name-nondirectory (car (last path))))


             (header-exts '("h" "hh" "hpp" "hxx"))
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
                     (select-mode-start-selection
                      choices
                      :buffer-name "select file"
                      :after-init #'select-mode-setup
                      :on-selection
                      (lambda (idx alt-file selection-type)
                        (select-mode-exit)
                        (puthash filename alt-file *c++-related-file-cache*)
                        (puthash alt-file filename *c++-related-file-cache*)
                        (funcall
                         (pcase selection-type
                           (`same-window  #'find-file)
                           (`other-window #'find-file-other-window))
                         alt-file))
                      :item-show-function
                      (lambda (x) (concat x "\n"))
                      :preamble
                      (lambda () (concat "Select desired alternative file\n")))))
                (error "No %s file found for %s"
                       (--map (concat "*." it) alt-exts)
                       filename)))))))))

(defun c++-format-buffer ()
  (interactive)
  (clang-format-buffer (format "{ IndentWidth: %s }" c-basic-offset)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.in\\(?:l\\|c\\|cl\\)\\'" . c++-mode))

(puthash 'c++-mode
         #'c++-format-buffer
         *mode-indent-functions-table*)

;;;###autoload
(defun c++-setup ()
  (cc-setup :define-special-keys t)
  (cc-setup/set-up-c-basic-offset)
  (setq-local company-backends
              '(company-clang
                company-files
                (company-eproj company-dabbrev-code company-keywords)
                company-dabbrev)
              hs-forward-sexp-func #'c-hideshow-forward-sexp
              indent-tabs-mode nil)
  (setup-folding t '(:header-symbol "/" :length-min 3))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC" c++-find-related-file))
  (c++-abbrev+-setup)
  (setup-eproj-symbnav))

;;;###autoload
(add-hook 'c++-mode-hook #'c++-setup)

;;;###autoload
(defun c++-file-magic-function ()
  (when-buffer-has-file
    (let ((ext (file-name-extension buffer-file-name)))
      ;; check for null since .emacs doesn't have extension
      (when (and ext
                 (member ext +cpp-header-exts+))
        (save-excursion
          (save-match-data
            (let ((search-result
                   (re-search-forward (rx
                                       (or "class"
                                           "namespace"
                                           "::"
                                           ;; it's quite rare to see other template
                                           ;; open brace styles so lets accomodate
                                           ;; only for frequently used ones
                                           (regex "template[[:space:]]*<")
                                           (regex "\\(?:public\\|protected\\|private\\)[[:space:]]*:")))
                                      nil
                                      t)))
              search-result)))))))

;; this will make sure that *.h c++ header will be correctly handled
;;;###autoload
(add-to-list 'magic-mode-alist (cons #'c++-file-magic-function #'c++-mode))

(provide 'c++-setup)

;; c++-setup.el ends here
