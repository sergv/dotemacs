;; emacs-lisp-highlight.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 29 December 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'common)

(defgroup emacs-lisp nil
  "Ansi Common Lisp highlighting."
  :group 'faces)

(defface emacs-lisp-constant-face
  '((t (:inherit font-lock-constant-face)))
  "Face to highlight conventional and other constants,
i.e. names matching regexp \\+.*\\+., self-evaluating symbols, numbers."
  :group 'emacs-lisp)

(defface emacs-lisp-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face to highlight both :keywords and function calls to standard functions."
  :group 'emacs-lisp)

(defface emacs-lisp-global-variable-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face to highlight conventional global variables,
i.e. names matching regexp \\*.*\\*."
  :group 'emacs-lisp)

(defface emacs-lisp-predicate-face
  '((t (:inherit font-lock-function-name-face)))
  "Face to highlight Scheme-like predicates: eq? and other ending with
question mark"
  :group 'emacs-lisp)

(defface emacs-lisp-mutating-op-face
  '((t (:inherit font-lock-function-name-face)))
  "Face to highlight Scheme-like mutating operations: set! and other ending
with exclamation mark"
  :group 'emacs-lisp)

(defconst +emacs-lisp-basic-keywords+
  (eval-when-compile
    `(;; support nearly full numeric tower
      ;; upd: full numeric tower is supported (? needs tests)
      ;; upd upd: rationals were not handled properly, merged with integers
      ;; - everything else seems to work fine
      (,(rx-let ((int-or-rat (seq (? (regexp "[+-]"))
                                  (+ digit)
                                  ;; now rationals go here
                                  (? "/"
                                     (+ digit))))
                 (binary      (seq "#b" (+ (regexp "[01]"))))
                 (octal       (seq "#o" (+ (regexp "[0-7]"))))
                 (hexadecimal (seq "#x" (+ (regexp "[0-9abcdefABCDEF]"))))
                 ;; (rational    (seq integer "/" integer))
                 (float-point (seq (? (regexp "[+-]"))
                                   (or (seq (+ digit)
                                            (? "." (* digit)))
                                       (seq (* digit)
                                            "." (+ digit)))))
                 (float       (seq float-point
                                   (? (regexp "[deDE]")
                                      float-point)))
                 (number (seq (or (seq symbol-start
                                       (or int-or-rat
                                           float))
                                  binary
                                  octal
                                  hexadecimal)
                              symbol-end))
                 ;; (number (seq (or int-or-rat
                 ;;                  binary
                 ;;                  octal
                 ;;                  hexadecimal
                 ;;                  ;; rational
                 ;;                  float)
                 ;;              symbol-end))
                 (complex (seq "#c("
                               (* whitespace)
                               number
                               (+ whitespace)
                               number
                               (* whitespace)
                               ")")))
          (rx
           (or number
               complex)))
       (0 'emacs-lisp-constant-face))

      ("\\_<\\(?:\\(?:[^: \t\n\r]+:\\)?\\(\\+[^+ \n\t\r\f]+\\+\\)\\)\\_>"
       (1 'emacs-lisp-constant-face))
      ("\\_<\\(?:\\(?:[^: \t\n\r]+:\\)?\\(\\*[^* \n\t\r\f]+\\*\\)\\)\\_>"
       (1 'emacs-lisp-global-variable-face))

      ;; handle both :keywords and #:sybols-without-home-package
      ("\\(#\\)?\\_<\\(\\:\\(?:\\s_\\|\\sw\\)+\\)\\_>"
       ;; symbols without home package, as special kind of constant
       (0 (when (re-group-matched? 1)
            'emacs-lisp-constant-face))
       ;; keywords
       (2 (unless (re-group-matched? 1)
            'emacs-lisp-keyword-face)))

      ("\\_<\\(?:[^ \n\t]+\\)\\?\\_>" 0 'emacs-lisp-predicate-face)
      ("\\_<\\(?:[^ \n\t]+\\)!\\_>"   0 'emacs-lisp-mutating-op-face)

      ;; make pretty lambdas
      ("(\\(lambda\\)\\_>"
       (0 (prog1 nil
            (compose-region (match-beginning 1)
                            (match-end 1)
                            ,(make-char 'greek-iso8859-7 107) ;; ?λ
                            ))))

      ;; ensure that pretty lambda goes away as soon as we type something after it
      ("(lambda[^ \n\t\v\f]"
       (0 (unless (memq (get-text-property (match-beginning 0)
                                           'face)
                        '(font-lock-comment-face
                          font-lock-string-face))
            (decompose-region (match-beginning 0)
                              (match-end 0))
            nil)))

      ("\\_<\\(?:t\\|nil\\)\\_>" (0 'emacs-lisp-constant-face)))))

(defun emacs-lisp-highlight-keywords (&optional mode)
  (font-lock-remove-keywords
   mode
   `(("\\<:\\sw+\\>"
      (0 font-lock-builtin-face))
     ("\\<:\\sw+\\>"
      0 font-lock-builtin-face)
     ("\\<\\&\\sw+\\>"
      (0 font-lock-type-face))
     ("\\<\\&\\sw+\\>"
      0 font-lock-type-face)))

  (font-lock-add-keywords mode +emacs-lisp-basic-keywords+))

;; some safety compile-time checks
(eval-when-compile
  (letrec ((check-font-lock-keywords
            (lambda (keywords)
              (unless (-all? #'listp keywords)
                (error "Non-list entry: %s"
                       (--find (not (listp it)) keywords)))
              (mapc (lambda (entry)
                      (unless (or (stringp (car entry))
                                  (symbolp (car entry)))
                        (error "Neither string nor symbol first value %s of entry: %s"
                               (car entry)
                               entry))
                      (cond
                        ((funcall highlight-entryp (cdr entry))
                         (funcall check-highlight-entry (cdr entry)))
                        ((-all? highlight-entryp (cdr entry))
                         (mapc check-highlight-entry (cdr entry)))
                        (t
                         (error "Non-highlight directive(s) found: %s" (cdr entry)))))
                    keywords)
              (values)))

           (check-highlight-entry
            (lambda (entry)
              (let ((face-entry (cadr entry)))
                (when (and (listp face-entry)
                           (symbolp (cadr face-entry))
                           (= 2 (length face-entry))
                           (not (eq 'quote (car face-entry))))
                  (error "Unquoted face: %s" face-entry))
                (when (symbolp face-entry)
                  (error "Unquoted face: %s" face-entry)))))

           (highlight-entryp
            (lambda (form)
              (and (listp form)
                   (numberp (car form))
                   (<= 2 (length form))))))

    (funcall check-font-lock-keywords +emacs-lisp-basic-keywords+)))


(provide 'emacs-lisp-highlight)

;; Local Variables:
;; End:

;; emacs-lisp-highlight.el ends here
