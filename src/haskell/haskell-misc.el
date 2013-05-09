;; haskell-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 20 September 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'macro-util)
(require 'advices-util)
(require 'common)

(require 'abbrev+)

;;; definitions

;; put it at top of file since it sometimes needs refinement
(defun make-ghc-command (filename optimize)
  (join-lines (append
               (list "ghc"
                     "-O2"
                     "-W"
                     "-Wall"
                     "-fwarn-monomorphism-restriction"
                     "-fno-warn-unused-do-bind"
                     "-fno-warn-type-defaults"
                     "-fno-warn-name-shadowing"
                     "-fno-warn-wrong-do-bind"
                     "--make"
                     "-main-is"
                     (file-name-sans-extension filename)
                     filename
                     "-rtsopts"
                     ;; this is really required with ghc 7.4 and my gold linker
                     "-pgml"
                     "/usr/bin/gcc"
                     ;; "-fforce-recomp"
                     )
               (when optimize
                 (append
                  '("-O2")
                  ;; (when (y-or-n-p "Use LLVM? ")
                  ;;   '("-fllvm"
                  ;;     "-optlc-O3"
                  ;;     "-optlo-O3"))
                  )))
              " "))


;; outline stuff
(defconst haskell-type-signature-regexp "[^:\n]::\\([^:\n]\\|$\\)")
(defconst haskell-toplevel-signature-regexp "^[^ ].*[^:\n]::\\([^:\n]\\|$\\)")
(defconst haskell-toplevel-data-declaration-regexp "^[ \t]*data[ \t]+\\(?:.\\|\n\\)+?=")
(defconst haskell-toplevel-class-declaration-regexp "^[ \t]*class[ \t]+\\(?:.\\|\n\\)+?where")
(defconst haskell-toplevel-instance-declaration-regexp "^[ \t]*instance[ \t]+\\(?:.\\|\n\\)+?where")
(defconst haskell-main-function-regexp "^main[ \t]*=[ \t]*\\(?:do\\)?")
(defconst haskell-commented-line-regexp "^[ \t]*-- ")

;; utility stuff
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


(defconst +haskell-compile-error-regexp+
  (rxx ((filename-char
         ;; that is, haskell filename should not contain spaces
         (regex "[^/\n\t\r\f\v]"))
        (filename
         (seq (*? "/"
                  (+ filename-char))
              (? "/")
              (+? filename-char)
              (or ".hs"
                  ".lhs"
                  ".hsc"))))
    (group
     filename)
    ":"
    (group
     (+ digit))
    ":"
    (group
     (+ digit))
    (? "-"
       (+ digit))
    (? ":"))
  "Regexp which is used by `compile' to detect errors.")

(defconst +haskell-compile-warning-regexp+
  (rxx ((filename-char
         ;; that is, haskell filename should not contain spaces
         (regex "[^/\n\t\r\f\v]"))
        (filename (seq (*? "/"
                           (+ filename-char))
                       (? "/")
                       (+? filename-char)
                       (or ".hs"
                           ".lhs"
                           ".hsc"))))
    bol
    (group ;; this get's highlighted as hyperlink
     (group
      filename)
     ":"
     (group
      (+ digit))
     ":"
     (group
      (+ digit))
     (? "-"
        (+ digit)))
    ":"
    (group
     (? "\n")
     (+ whitespace)
     "Warning:"))
  "Regexp which is used by `compile' to detect warnings.")


;;; compilation

(require 'compilation-setup)

(defun haskell-reload-on-successful-compilation (buffer msg)
  "If compilation was sucessfull then reload source file into ghci."
  (when (eq (cdr (assq 'mode *compile-caller-info*)) 'haskell-mode)
    (when (string-match "^finished" msg)
      (with-current-buffer (cdr (assoc 'buffer *compile-caller-info*))
        (inferior-haskell-load-file nil t t))
      (setq *compile-caller-info* nil))))

(defun haskell-jump-to-error (buffer msg)
  "Jump to error if compilation wasn't sucessfull, ignore warnings."
  (when (eq (cdr (assq 'mode *compile-caller-info*)) 'haskell-mode)
    (when (string-match "^exited" msg)
      (with-current-buffer buffer
        (goto-char (point-min))
        (save-match-data
          (let ((continue t))
            (while (and continue
                        (re-search-forward +haskell-compile-error-regexp+ nil t))
              (goto-char (match-beginning 0))
              (if (looking-at-pure? +haskell-compile-warning-regexp+)
                (forward-line 1)
                (setq continue nil)))))
        (setq *compile-caller-info* nil)
        (compile-goto-error)))))

(defvar-local haskell-has-makefile? nil
  "Is set to t by `haskell-setup' when current haskell file has
entry in makefile and should be build with make.")

(defun haskell-compile (&optional optimize)
  "Start compilation of Haskell file."
  (interactive (list current-prefix-arg))
  (let ((fname (file-name-nondirectory buffer-file-name)))
    (compilation-start (if haskell-has-makefile?
                         "make"
                         ;; if there's makefile then let it handle project
                         ;; build as it wants
                         ;; (concat "make "
                         ;;         (file-name-sans-extension fname))
                         (make-ghc-command fname optimize))
                       #'haskell-compilation-mode)))

(define-compilation-mode haskell-compilation-mode "Haskell compilation"
  "Mode for ghc compilation."

  (set (make-local-variable 'compilation-error-regexp-alist)
       (list
        (list +haskell-compile-warning-regexp+ ;; regex
              2                                ;; file-group
              3                                ;; line-group
              4                                ;; column-group
              1                                ;; type - 1 - warning
              1 ;; hyperlink subexpression - this gets highlighted
              )
        (list +haskell-compile-error-regexp+
              1 ;; file-group
              2 ;; line-group
              3 ;; column-group
              2 ;; type - 2 - real error
              )))

  (set (make-local-variable '*compilation-jump-error-regexp*)
       +haskell-compile-error-regexp+)

  (set (make-local-variable 'compilation-first-column) 1) ;GHC counts from 1.
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-scroll-output) nil))


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

;;; define abbreviations



(provide 'haskell-misc)

;; Local Variables:
;; End:

;; haskell-misc.el ends here
