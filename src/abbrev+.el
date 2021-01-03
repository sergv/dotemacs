;; abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  1 November 2011
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)

(defstruct (abbrev+-abbreviation
            (:constructor make--abbrev+-abbreviation))
  ;; Regular expression that should be matched in order for
  ;; this abbreviation to activate.
  (trigger nil :read-only t)
  ;; Whether to ignore case when matching the trigger.
  (trigger-is-case-sensitive nil :read-only t)
  ;; Symbol, one of 'literal-string, 'literal-string-no-space-at-end ,
  ;; 'function-result, 'function-with-side-effects or 'yas-snippet.
  (action-type nil :read-only t)
  ;; Action, type depends on value of action-type field.
  ;; For 'literal-string it's a string.
  ;; For 'literal-string-no-space-at-end it's a string.
  ;; For 'function-result it's a function that returns a string.
  ;; For 'function-with-side-effects it's a function that inserts the
  ;; necessary content itself.
  ;; For 'yas-snippet it's a string that will be passed to `yas-expand-snippet'.
  (action-data nil :read-only t)
  ;; Function of no arguments, either symbol or lambda, that
  ;; will be called before performing ACTION and should return nil or t.
  ;; If t is returned then ACTION would be performed.
  ;;
  ;; Optional.
  (predicate nil :read-only t)

  ;; Function of no arguments, either symbol or lambda, that
  ;; will be called before after ACTION. Its return value will be ignored.
  ;;
  ;; Optional.
  (on-successful-expansion nil :read-only t))

(defun* make-abbrev+-abbreviation (&key trigger trigger-is-case-sensitive action-type action-data predicate on-successful-expansion)
  (cl-assert (stringp trigger))
  (cl-assert (or (null predicate) (functionp predicate)))
  (cl-assert (or (null on-successful-expansion)
                 (functionp on-successful-expansion)))
  (cl-assert (symbolp action-type))
  (cl-assert
   (pcase action-type
     ((or `literal-string `literal-string-no-space-at-end `yas-snippet)
      (cl-assert (stringp action-data))
      t)
     ((or `function-result `function-with-side-effects)
      (cl-assert (or (symbolp action-data)
                     (functionp action-data)
                     (byte-code-function-p action-data)))
      t)
     (typ nil)))
  (make--abbrev+-abbreviation
   :trigger (concat "\\`" trigger "\\'")
   :trigger-is-case-sensitive trigger-is-case-sensitive
   :action-type action-type
   :action-data action-data
   :predicate predicate
   :on-successful-expansion on-successful-expansion))

(defvar-local abbrev+-abbreviations
  (vector
   (make-abbrev+-abbreviation
    :trigger "^pwd"
    :action-type 'function-result
    :action-data (lambda () (expand-file-name default-directory))))
  "A list of `abbrev+-abbreviation' structures.")

(defvar-local abbrev+-skip-syntax
  ["w" "w_" "w_." "^->"]
  "List of syntaxes that will be tried one after the other
to find match for car-element in `abbrev+-abbreviations'")

(defvar abbrev+-after-expand-and-space-hook nil
  "Hook to be run after expansion was carried out and trailing space was
inserted. Space is important - if conditions to insert space were not
met then this hook would not run.")


(defun abbrev+--get-substitution (str)
  "Return substitution for STR obtained by matching STR against
trigger of `abbrev+-abbreviations' and returning corresponding element in cdr."
  (v--find
   (let ((case-fold-search (not (abbrev+-abbreviation-trigger-is-case-sensitive it))))
     (string-match-p (abbrev+-abbreviation-trigger it) str))
   abbrev+-abbreviations))

(defun abbrev+-perform-substitution (abbrev)
  "Perform actual substitution. Return two arguments: first being
t if after substitution it may be desirable to insert space and
second being actual substituted text."
  (cl-assert (abbrev+-abbreviation-p abbrev))
  (let ((data (abbrev+-abbreviation-action-data abbrev))
        (action-type (abbrev+-abbreviation-action-type abbrev)))
    (prog1
        (pcase action-type
          ((or `literal-string `literal-string-no-space-at-end)
           (cl-assert (stringp data))
           (insert data)
           (eq action-type 'literal-string))
          (`function-result
           (cl-assert (or (symbolp data) (functionp data) (byte-code-function-p data)))
           (let ((res (funcall data)))
             (unless (stringp res)
               (error "Action %s didn't return string"
                      (substring-no-properties
                       (pp-to-string data)
                       0
                       80)))
             (insert res))
           t)
          (`function-with-side-effects
           (cl-assert (or (symbolp data) (functionp data) (byte-code-function-p data)))
           (funcall data)
           nil)
          (`yas-snippet
           (cl-assert (stringp data))
           (yas-expand-snippet data)
           nil)
          (typ
           (error "Unknown action type %s of abbreviation %s" typ abbrev)))
      (awhen (abbrev+-abbreviation-on-successful-expansion abbrev)
        (funcall it)))))

(defun abbrev+-expand ()
  "Expand text before point that matches against one of triggers
of `abbrev-abbreviations'. Returns boolean indicating whether
expansion was performed."
  (let ((start (point))
        entry
        str
        result)
    (loop
      for syntax across abbrev+-skip-syntax
      until entry
      do
      (goto-char start)
      (skip-syntax-backward " " (line-beginning-position))
      (let ((beginning (point)))
        (cond
          ((stringp syntax)
           (skip-syntax-backward syntax))
          ((vectorp syntax)
           (loop
             for s across syntax
             do (skip-syntax-backward s)))
          (t
           (error "Invalid abbrev+ skip syntax: %s" syntax)))
        (setf str (buffer-substring-no-properties (point) beginning)
              entry (abbrev+--get-substitution str))))
    (let ((result
           (when (and entry
                      (aif (abbrev+-abbreviation-predicate entry)
                          (save-excursion (funcall it))
                        ;; do substitution if no predicate supplied
                        t))
             (delete-region (point) start)
             (let* ((point-before-substitution (point))
                    (insert-spacep (abbrev+-perform-substitution entry))
                    (new-text (buffer-substring-no-properties point-before-substitution (point))))
               (unless (string-equal str new-text)
                 (when (and insert-spacep
                            (or (eobp)
                                (not (char-equal (char-after) ?\s))))
                   (insert " "))
                 ;; substitution was succesfull
                 t)))))
      (unless result
        (goto-char start))
      result)))

(defvar-local abbrev+-fallback-function (lambda () (insert-char ?\s))
  "Fallback function called by `abbrev+-insert-space-or-expand-abbrev'
if no expansion was produced.")

;;;###autoload
(defun abbrev+-insert-space-or-expand-abbrev (&optional dont-expand)
  (interactive (list current-prefix-arg))
  (when (or dont-expand
            (not (abbrev+-expand)))
    (funcall abbrev+-fallback-function)))

;;;###autoload
(defun abbrev+-org-self-insert-or-expand-abbrev (&optional dont-expand)
  (interactive (list current-prefix-arg))
  (when (or dont-expand
            (not (abbrev+-expand)))
    (org-self-insert-command 1)))

(defun abbrev+--make-re-with-optional-suffix (str suffix-len)
  (declare (pure t) (side-effect-free t))
  (letrec ((make-suffix
            (lambda (list)
              (if (null list)
                (list ??)
                (append
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
  (declare (pure t) (side-effect-free t))
  (concat "("
          (mapconcat (lambda (x) (abbrev+--make-re-with-optional-suffix (car x) (cadr x)))
                     name-parts
                     "-?")))

(provide 'abbrev+)

;; Local Variables:
;; End:

;; abbrev+.el ends here
