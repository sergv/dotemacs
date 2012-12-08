;; abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  1 November 2011
;; Description:


(require 'custom)
(eval-when-compile '(require 'cl))

(defvar-local abbrev+-abbreviations
    (list
     (list "^pwd" #'(lambda () (expand-file-name default-directory))))
  "Alist of list triples (REGEX ACTION [PREDICATE]), PREDICATE being
optional.

REGEX is the regexp that must be matched against for text to be inserted.

ACTION is actual string or function, either symbol or lambda,
that should return string to be inserted or nonempty list
of functions, either symbols or lambdas that would be called
one after another and which should perform actual substitution themselves,
but deletion of matched text would be handled by abbrev+ facilities.

PREDICATE is function of no arguments, either symbol or lambda, that
will be called before performing ACTION and should return nil or t.
If t is returned then ACTION would be performed.

Functions could make use of variable `abbrev+-text-to-substitute' that
will have the value of text matched by the regular expression.")

(defvar-local abbrev+-skip-syntax
    '("w" "w_" "w_." "^->")
  "List of syntaxes that will be tried one after the other
to find match for car-element in `abbrev+-abbreviations'")

(defvar abbrev+-text-to-substitute nil
  "Will contain value of text that will be replaced when function
from `abbrev+-abbreviations' second element will be called.")

(defvar abbrev+-after-expand-and-space-hook nil
  "Hook to be run after expansion was carried out and trailing space was
inserted. Space is important - if conditions to insert space were not
met then this hook would not run.")


(defun abbrev+-get-substitution (str)
  "Return substitution for STR obtained by matching STR against
car's of `abbrev+-abbreviations' and returning corresponding element in cdr."
  (save-match-data
   (let ((res (find-if #'(lambda (re)
                           (and (string-match re str)
                                (= (match-beginning 0) 0)
                                (= (match-end 0) (length str))))
                       abbrev+-abbreviations
                       :key #'first)))
     res)))

(defun abbrev+--funcall (f)
  "Somewhat specialized funcall, may be totally redundant with respect
to `funcall'. Anyway this should funcall either ordinary functions
and quoted, #'-ed and byte-compiled functions too."
  (cond
    ((or (symbolp f)
         (functionp f)
         (byte-code-function-p f))
     (funcall f))
    ((and (listp f)
          (or (eq 'function (car f))
              (eq 'lambda (car f))))
     (funcall (cdr f)))))

(defun abbrev+-perform-substitution (action)
  "Perform actual substitution treating SUBST as cdr of entry
in `abbrev+-abbreviations' whose car matched. Return two arguments:
first being t if after substitution it may be desirable to insert space
and second being actual substituted text."
  (let ((p (point))
        (insert-spacep
          (cond
            ((stringp action)
             (insert action)
             t)
            ((and (listp action)
                  (or (listp (car action))
                      (symbolp (car action))
                      (functionp (car action))
                      (byte-code-functino-p (car action))))
             ;; it's a list of functions so they should
             ;; be called sequentially
             (mapc #'abbrev+--funcall
                   action)
             nil)
            (t
             (insert (abbrev+--funcall action))
             t))))
    (values insert-spacep (buffer-substring-no-properties p (point)))))


(defun abbrev+-expand (&optional dont-expand)
  "Expand text before point that matches against one of regular expressions in
`abbrev-abbreviations'. Returns nil if nothing was substituted."
  (interactive (list current-prefix-arg))
  (unless dont-expand
    (let ((p (point))
          entry
          str
          result)
      (loop
        for syntax in abbrev+-skip-syntax
        until entry
        do
           (goto-char p)
           (skip-syntax-backward syntax)
           (setf str (buffer-substring-no-properties (point) p))
           (setf entry (abbrev+-get-substitution
                        str)))
      (if entry
        (let ((action (second entry))
              (predicate (third entry))
              substitutep
              (point-before-predicate-call (point)))
          (setf substitutep
                (if predicate
                  (abbrev+--funcall predicate)
                  ;; do substitution if no predicate supplied
                  t))
          (if substitutep
            (progn
              (goto-char point-before-predicate-call)
              (delete-region (point) p)
              (setf abbrev+-text-to-substitute str)
              (multiple-value-bind (insert-spacep substituted-text)
                  (abbrev+-perform-substitution action)
                (setf abbrev+-text-to-substitute nil)

                (if (string= str substituted-text)
                  ;; text before and after substitution don't changed
                  ;; - treat this as if no substitution was performed
                  (setf result nil)
                  (progn
                    (when (and insert-spacep
                               (not (char-equal (char-after) ?\s)))
                      (insert " "))
                    ;; substitution was succesfull
                    (setf result t)))))
            ;; if we do not perform substitution then return to original point
            (progn
              (goto-char p)
              ;; no substitutions were performed
              (setf result nil))))
        ;; no suitable entry found
        (progn
          (goto-char p)
          ;; no substitutions were performed
          (setf result nil)))
      result)))

(defun abbrev+-insert-space-or-expand-abbrev (&optional dont-expand)
  (interactive (list current-prefix-arg))
  (unless (abbrev+-expand dont-expand)
    (insert " ")))

(defun abbrev+-org-self-insert-or-expand-abbrev (&optional dont-expand)
  (interactive (list current-prefix-arg))
  (unless (abbrev+-expand dont-expand)
    (org-self-insert-command 1)))

(defun make-re-with-optional-suffix (str suffix-len)
  (letrec ((make-suffix
             (lambda (list)
               (if (null list)
                 (list ??)
                 (nconc
                  (list ?\\
                        ?\(
                        ??
                        ?:
                        (car list))
                  (funcall make-suffix (cdr list))
                  (list ?\\
                        ?\)
                        ??))))))
    (concat (subseq str 0 suffix-len)
            (apply #'string
                   (funcall make-suffix
                            (string-to-list
                             (subseq str suffix-len)))))))

(defun make-abbrev+-re-for-lisp-func-name (name-parts)
  "Return re that matches emacs lisp function name, NAME-PARTS is
a list of the (partN suffix-lengthN) elements, resulting re would
match part1-part2-...-partN with optional dashes and suffix
recognition."
  (concat "("
          (mapconcat (lambda (x) (make-re-with-optional-suffix (car x) (cadr x)))
                     name-parts
                     "-?")
          "\\_>"))


(provide 'abbrev+)

;; Local Variables:
;; End:

;; abbrev+.el ends here
