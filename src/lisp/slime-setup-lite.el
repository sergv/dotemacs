;; slime-setup-lite.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  3 February 2013
;; Description:

(require 'common)
(require 'more-scheme)
(require 'general-lisp-setup)

(add-to-list 'load-path +slime-path+)

(setf slime-repl-history-file (concat +prog-data-path+ "/slime-repl-history"))

(require 'slime)

(slime-setup '(slime-fontifying-fu
               slime-indentation))

(define-common-lisp-style "my-style"
  "My custom indentation style, very similar to modern one."
  (:inherit "modern")
  (:variables
   (lisp-indent-maximum-backtracking 10)
   (lisp-align-keywords-in-calls t)
   (lisp-loop-indent-subclauses nil)
   (lisp-lambda-list-keyword-parameter-indentation 0)

   (indent-tabs-mode nil)
   (comment-fill-column nil))

  (:indentation
   (if (4 2 2))
   (setf (nil))
   (setq (as setf))
   (defpackage (4 &rest (&whole 2 &rest nil)))
   (bind (as let))
   (bind* (as let*))
   (define-foreign-library (4 &rest 2))
   (begin (as progn))
   (labels ((&whole 2 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))))


(define-common-lisp-style "clisp"
  "Indentation style used in CLISP sources."
  (:inherit "modern")
  (:variables
   (lisp-indent-maximum-backtracking 10)
   (lisp-align-keywords-in-calls t)
   (lisp-loop-indent-subclauses nil)
   (lisp-lambda-list-keyword-parameter-indentation 0)

   (indent-tabs-mode nil)
   (comment-fill-column nil))

  (:indentation
   (and (&rest 2))
   (appease-cerrors (&rest 2))
   (assert (&rest 2))
   (block (4 &rest 2)) ;; default
   (case (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
   (catch (4 &rest 2)) ;; default
   (ccase (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
   (check-type (2 2 &rest 2))
   (compiler-let ((&whole 4 &rest (&whole 1 1 2)) &body)) ;; default
   (cond (&rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
   (ctypecase (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
   (decf (2 2))
   (declaim (&rest 2))
   (declare (&rest 2))
   (def-c-enum (4 &rest 2))
   (def-c-struct (4 &rest 2))
   (defclass (10 (&whole 10 1 &rest 1) &rest (&whole 2 &rest 1)))
   (defconstant (4 2 2)) ;; default
   (defgeneric (4 (&whole 4 1 &rest 1) &body))
   (define-condition (18 (&whole 18 1 &rest 1) &rest (&whole 2 &rest 1)))
   (define-modify-macro (4 (&whole 4 1 &rest 1) 4 &body))
   (define-setf-expander (4 (&whole 4 1 &rest 1) &body))
   (define-setf-method (4 (&whole 4 1 &rest 1) &body))
   (define-symbol-macro (4 &body))
   (definternational (4 &body))
   (deflanguage (4))
   (deflocalized (4 4 &body))
   (defmacro (4 (&whole 4 1 &rest 1) &body))
   (defmethod lisp-indent-defmethod) ;; default
   (defpackage (4 &rest 2))
   (defparameter (4 2 2)) ;; default
   ;; FIXME: How to deal with both short and long forms of defsetf?
   ;;(defsetf (4 (&whole 4 1 &rest 1) 2 &body))
   ;;(defsetf (14 (&whole 14 1 &rest 1) (&whole 14 1 &rest 1) &body))
   (defstruct ((&whole 4 &rest (&whole 2 &rest 1)) &rest (&whole 2 &rest 1))) ;; default
   (deftype (9 (&whole 9 1 &rest 1) &body))
   (defun (7 (&whole 7 1 &rest 1) &body))
   (defvar (4 2 2)) ;; default
   (destructuring-bind ((&whole 6 1 &rest 1) 4 &body))
   (deutsch (2 1 2 1 2))
   (do lisp-indent-do)  ;; default
   (do* lisp-indent-do) ;; default
   (do-all-symbols ((&whole 4 1 &rest 1) &body))
   (do-external-symbols ((&whole 4 1 &rest 1) &body))
   (do-symbols ((&whole 4 1 &rest 1) &body))
   (dohash ((&whole 4 1 &rest 1) (&whole 4 1 &rest 1) &body))
   (dolist ((&whole 4 1 1) &body))
   (doseq ((&whole 4 1 1) &body))
   (dotimes ((&whole 4 1 1) &body))
   (ecase (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
   (english (2 1 2 1 2))
   (etypecase (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
   (eval-when (4 &body)) ;; default
   (exit-on-error (&body))
   (fcase '(6 4 &rest (&whole 2 &rest 1)))
   (flet ((&whole 4 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))
   (formatter (&body))
   (francais (2 1 2 1 2))
   (function (&body))
   (generic-flet ((&whole 4 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))
   (generic-function ((&whole 4 1 &rest 1) &body))
   (generic-labels ((&whole 4 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))
   (go (2))
   (handler-bind (2 &body))
   (handler-case (2 &rest (&whole 2 (&whole 4 1 &rest 1) &body)))
   ;; CLISP source indents the two branchs of an 'if' form equally.
   (if (4 2 2))
   (ignore-errors (&body))
   (in-package (&body))
   (incf (2 2))
   (labels ((&whole 4 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))
   (lambda ((&whole 4 1 &rest 1) &body))
   (let ((&whole 4 &rest (&whole 1 2 2)) &body))
   (let* ((&whole 4 &rest (&whole 1 2 2)) &body))
   (load-time-value (&body))
   (locally (2 &body))
   ;; CLISP sources don't use the "big" LOOP - its semantics is too unreliable.
   (loop (&body))
   (loop-finish ())
   (macrolet ((&whole 4 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))
   (:method ((&whole 4 1 &rest 1) &body)) ; for defgeneric etc.
   (muffle-cerrors (&rest 2))
   (multiple-value-bind ((&whole 6 &rest 1) 4 2 &rest 2))
   (multiple-value-call (4 2 &rest 2))
   (multiple-value-list (2))
   (multiple-value-prog1 (2 &rest 2))
   (multiple-value-setq (4 2)) ;; default
   (nth-value (2 2))
   (optimize (&rest 2))
   (or (&rest 2))
   (pop (2))
   (print-unreadable-object ((&whole 4 1 &rest 1) &body))
   (prog ((&whole 4 1 &rest 1) &rest lisp-indent-tagbody))
   (prog* ((&whole 4 1 &rest 1) &rest lisp-indent-tagbody))
   (prog1 (2 &body))
   (prog2 (2 2 &body))
   (progn (&body))     ;; default
   (progv (4 4 &body)) ;; default
   (psetf (7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7))
   (psetq (7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7))
   (push (&body))
   (pushnew (&body))
   (quote (2))
   (remf (2 2))
   (restart-bind ((&whole 4 &rest 1) &body))
   (restart-case (4 &rest (&whole 2 (&whole 4 1 &rest 1) &body)))
   (return (&body)) ;; default
   (return-from (2 &body))
   (rotatef (&body))
   (setf (6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6))
   (setq (6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6))
   (:shadowing-import-from (4 &rest 2))
   (shiftf (&body))
   (space (2))
   (step (2))
   (symbol-macrolet ((&whole 4 &rest (&whole 1 2 &rest 2)) &body))
   (tagbody lisp-indent-tagbody) ;; default
   (the (4 2))
   (the-environment ())
   (throw (4 &body)) ;; default
   (time (2))
   (trace (&body))
   (typecase (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
   (unless (4 &body)) ;; default
   (untrace (&body))
   (unwind-protect (2 &body))
   (when (4 &body)) ;; default
   (with-accessors ((&whole 4 1 &rest 1) 4 &body))
   (with-condition-restarts (4 4 &body))
   (with-hash-table-iterator (4 &body))
   (with-input-from-string ((&whole 4 1 &rest 1) &body))
   (with-keyboard (&body))
   (with-open-file ((&whole 4 1 &rest 1) &body))
   (with-open-stream (4 &body))
   (with-output-to-printer ((&whole 4 1 &rest 1) &body))
   (with-output-to-string ((&whole 4 1 &rest 1) &body))
   (with-package-iterator ((&whole 4 1 &rest 1) &body))
   (with-restarts ((&whole 4 &rest (&whole 2 (&whole 4 1 &rest 1) &body)) &body))
   (with-simple-restart ((&whole 4 1 &rest 1) &body))
   (with-slots ((&whole 4 1 &rest 1) 4 &body))
   (with-standard-io-syntax (&body))
   (without-floating-point-underflow (&body))))


(define-common-lisp-style "emacs"
  "My custom indent style for emacs lisp."
  (:inherit "my-style")

  (:indentation
   (aif (as if))
   (eval-when-compile (2))
   (begin (as progn))
   (condition-case (4 4 &body))
   (def-keys-for-map (&body))
   (with-temp-buffer (&body))
   (while (4 &body))
   (rxx (as let))
   (letrec (as let))
   (redefun (as defun))
   (edefun (as defun))
   (defvar-local (as defvar))
   (define-print-info-skeleton (4 &body))
   (define-lisp-print-info-skeleton (as define-print-info-skeleton))
   (define-repeated-function (4 &body))
   (define-switch-to-interpreter (4 (&whole 2 &rest 1) &rest 2))
   (check-for-stop (4 &rest 1))
   (skip-and-check (as check-for-stop))
   (moving-one-item-forward (4 4 &body))
   (forward-sexp-with-bounds (4 4 &body))
   (defstruct* (as defstruct))
   (sexpy-define-pattern-fontifier (4 &rest 1))
   (with-current-frame (as with-current-buffer))
   (with-disabled-undo (1))
   (with-preserved-buffer-modified-p (1))
   (with-inhibited-modification-hooks (1))
   (with-inhibited-read-only (1))
   (ert-deftest (as defun))
   (with-hidden-comments (1))
   (with-hidden-cloze-hints (1))
   (with-hidden-cloze-text (1))
   (magit-define-command (as defun))
   (magit-section-action (4 2))
   (make-buf-tag-pred (&rest 1))
   (pcase (4 &rest 2))))


(define-common-lisp-style "scheme"
  "Custom indent style for scheme."
  (:inherit "modern")
  (:variables
   (lisp-indent-maximum-backtracking 10)
   (lisp-align-keywords-in-calls t)
   (lisp-loop-indent-subclauses nil)
   (lisp-lambda-list-keyword-parameter-indentation 0)

   (indent-tabs-mode nil)
   (comment-fill-column nil))

  (:indentation
   (if             (4 4 4))
   (aif            (as if))
   (define         (nil &body))
   (define-macro   (as define))
   (define-syntax  (as define))
   (define-method  (as define))
   (define-generic (as define))
   (module         (nil nil 0))
   (syntax-rules   (as define))
   ;; guile-specific
   (lambda*        (as lambda))
   (define*        (as define))
   (letrec         (as let))
   (let-values     (as let))
   (let*-values    (as let))))


(setf common-lisp-style-default "my-style")

(provide 'slime-setup-lite)

;; Local Variables:
;; End:

;; slime-setup-lite.el ends here
