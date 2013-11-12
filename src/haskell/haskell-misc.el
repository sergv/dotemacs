;; haskell-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 20 September 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile (require 'cl-lib))

(require 'macro-util)
(require 'advices-util)
(require 'common)

(require 'abbrev+)
(require 'haskell-compile)

;;; definitions

(defconst +haskell-tmp-path+ (concat +tmp-path+ "/haskell-tmp"))

(make-directory +haskell-tmp-path+ t)

(setf haskell-compile-command
      (or (getenv "HASKELL_COMPILE_COMMAND")
          (concat "ghc -W -Wall -fwarn-monomorphism-restriction "
                  "-ferror-spans -fforce-recomp "
                  (when (platform-os-type? 'linux)
                    ;; needed for ghc 7.4 and gold linker
                    "-rtsopts -pgml /usr/bin/gcc ")
                  (format "-hidir %s " +haskell-tmp-path+)
                  (format "-odir %s " +haskell-tmp-path+)
                  (format "-tmpdir %s " +haskell-tmp-path+)
                  ;; llvm
                  ;; "-fllvm -optlc-O3 -optlo-O3 "
                  "-c \"%s\"")))


(defconst +haskell-compile-error-or-warning-regexp+
  (rx bol
      (+? (not (any ?\s ?\t ?\r ?\n)))
      ":"
      (or
       ;; "121:1" & "12:3-5"
       (seq (+ (any (?0 . ?9)))
            ":"
            (+ (any (?0 . ?9)))
            (? "-"
               (+ (any (?0 . ?9)))))
       ;; "(289,5)-(291,36)"
       (seq "("
            (+ (any (?0 . ?9)))
            ","
            (+ (any (?0 . ?9)))
            ")-("
            (+ (any (?0 . ?9)))
            ","
            (+ (any (?0 . ?9)))
            ")"))
      ":"
      (? (group " Warning:")))
  "Regexp matching both errors and warnings.")


;; for outline
(defconst haskell-type-signature-regexp "[^:\n]::\\([^:\n]\\|$\\)")
(defconst haskell-toplevel-signature-regexp "^[^ ].*[^:\n]::\\([^:\n]\\|$\\)")
(defconst haskell-toplevel-data-declaration-regexp "^[ \t]*data[ \t]+\\(?:.\\|\n\\)+?=")
(defconst haskell-toplevel-class-declaration-regexp "^[ \t]*class[ \t]+\\(?:.\\|\n\\)+?where")
(defconst haskell-toplevel-instance-declaration-regexp "^[ \t]*instance[ \t]+\\(?:.\\|\n\\)+?where")
(defconst haskell-main-function-regexp "^main[ \t]*=[ \t\n\r]*\\(?:do\\)?")
(defconst haskell-commented-line-regexp "^[ \t]*-- ")

;; just useful utility
(defconst haskell-operator-regexp "\\(\\s_\\|\\\\\\)+"
  "For qualification consult `haskell-font-lock-keywords-create'
in haskell-font-lock.el")

(defsubst haskell-operator? (expr)
  "Return t if EXPR is a Haskell operator (e.g. !!, ++, Data.Map.!, etc.)"
  (string-match-pure? haskell-operator-regexp expr))


(defconst haskell-module-quantification-regexp
  (let ((conid "\\b[[:upper:]][[:alnum:]'_]*\\b"))
    (concat "\\b\\(?:" conid "\\.\\)+")))

(defun haskell-remove-module-qualification (name)
  "Removes hierarchihal modules qualification (e.g. Data.Map.null -> null,
 Prelude.++ -> ++, etc)"
  (save-match-data
    (if (string-match (concat "^\\("
                              haskell-module-quantification-regexp
                              "\\)")
                      name)
      (replace-match "" t t name 1)
      name)))

;;; compilation

(require 'compilation-setup)

(defun haskell-jump-to-error (buffer msg)
  "Jump to error if compilation wasn't sucessfull, ignore warnings."
  (when (eq (cdr (assq 'mode *compile-caller-info*)) 'haskell-mode)
    (when (string-match-pure? "^exited" msg)
      (with-current-buffer buffer
        (goto-char (point-min))
        (save-match-data
          (let ((found nil))
            ;; find first error
            (while (and (not found)
                        (re-search-forward +haskell-compile-error-or-warning-regexp+ nil t))
              ;; if first group didn't match then it's an error at point
              (unless (match-string-no-properties 1)
                (setf found t))))
          (setf *compile-caller-info* nil)
          (when found
            (goto-char (match-beginning 0))
            (compile-goto-error)))))))

(defun haskell-compile-file (&optional edit-command)
  "Similar to `haskell-compile' but recognizes makefiles."
  (interactive "P")
  (let* ((fname (file-name-nondirectory buffer-file-name))
         (dir (file-name-directory buffer-file-name))
         (has-makefile? (any? (lambda (fname)
                                (file-exists? (concat dir "/" fname)))
                              "makefile"
                              "Makefile"
                              "MAKEFILE"
                              "GNUMakefile")))
    (if haskell-has-makefile?
      (compilation-start "make" 'haskell-compilation-mode)
      (haskell-compile edit-command))))

(defun haskell-compilation-setup ()
  (set (make-local-variable '*compilation-jump-error-regexp*)
       +haskell-compile-error-or-warning-regexp+)

  (set (make-local-variable 'compilation-first-column) 1) ;; GHC counts from 1.
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-scroll-output) nil))

(add-hook 'haskell-compilation-mode-hook #'haskell-compilation-setup)


;;; haddock for modules

(defun inferior-haskell-haddock-module (name)
  "Find and open the Haddock documentation of module NAME.
Only works for module in a package installed with ghc-pkg, or
whatever the value of `haskell-package-manager-name' is.

This function needs to find which package a given module belongs
to.  In order to do this, it computes a module-to-package lookup
alist, which is expensive to compute (it takes upwards of five
seconds with more than about thirty installed packages).  As a
result, we cache it across sessions using the cache file
referenced by `inferior-haskell-module-alist-file'. We test to
see if this is newer than `haskell-package-conf-file' every time
we load it."
  (interactive
   (let ((name (haskell-ident-at-point)))
     (list (read-string (if (> (length name) 0)
                          (format "Find documentation of module (default %s): " name)
                          "Find documentation of module: ")
                        nil nil name))))
  (setq name (inferior-haskell-map-internal-ghc-ident name))
  (let ( ;; Find the module and look it up in the alist
        (alist-record (assoc name (inferior-haskell-module-alist))))

    (if alist-record
      (progn ;; if documentation for such module exists at all
        (let* ((package (nth 1 alist-record))
               (file-name (concat (subst-char-in-string ?. ?- name) ".html"))
               (local-path (concat (nth 2 alist-record) "/" file-name))
               (url (if (or (eq inferior-haskell-use-web-docs 'always)
                            (and (not (file-exists-p local-path))
                                 (eq inferior-haskell-use-web-docs 'fallback)))
                      (concat inferior-haskell-web-docs-base package "/" file-name
                              ;; no haddock anchor for module names
                              )
                      (and (file-exists-p local-path)
                           ;; no haddock anchor for module names
                           (concat "file://" local-path)))))
          (if url (browse-url url) (error "Local file doesn't exist"))))
      (error "No documentation for module %s found" name))))


;;; miscellany

(defalias 'inferior-haskell-haddock-identifier 'inferior-haskell-find-haddock)


;;; Automatized definitions using advices-util and macro-util

;;; align functions

(make-align-function haskell-align-on-equals
                     "=[^=]"
                     :require-one-or-more-spaces t)
(make-align-function haskell-align-on-arrows
                     "-> ")
(make-align-function haskell-align-on-left-arrows
                     "<- ")
(make-align-function haskell-align-on-guards
                     "|[^|]"
                     :require-one-or-more-spaces t)
(make-align-function haskell-align-on-commas
                     ",[^,)]")
(make-align-function haskell-align-on-comments
                     "-- "
                     :require-one-or-more-spaces t)
(make-align-function haskell-align-on-double-colons
                     "::[^:]")

;;; custom queries to inferior-haskell

(haskell:make-query-to-inferior haskell-type               inferior-haskell-type t)
(haskell:make-query-to-inferior haskell-info               inferior-haskell-info)
(haskell:make-query-to-inferior haskell-haddock-identifier inferior-haskell-haddock-identifier)
(haskell:make-query-to-inferior haskell-haddock-module     inferior-haskell-haddock-module)
(haskell:make-query-to-inferior haskell-find-definition    inferior-haskell-find-definition)
(haskell:make-query-to-inferior haskell-hoogle-at-point    haskell-hoogle)
(haskell:make-query-to-inferior haskell-hayoo-at-point     haskell-hayoo)

;;; expand on search


;; (search-def-autoexpand-advices (show-subtree) (haskell-mode))

;;; remember position on query

;; (defadvice:remember-position-on-query inferior-haskell-find-definition)
;; (defadvice:remember-position-on-query haskell-find-definition)


(provide 'haskell-misc)

;; Local Variables:
;; End:

;; haskell-misc.el ends here
