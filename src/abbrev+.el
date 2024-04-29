;; abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  1 November 2011
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'common)
(require 'dash)
(require 'trie)

(declare-function org-self-insert-command "org")
(declare-function yas-expand-snippet "yasnipet")

(cl-defstruct (abbrev+-abbreviation
               (:constructor make--abbrev+-abbreviation))
  ;; Whether to expand even if there’re whitespace characters between current point
  ;; and trigger string.
  ;; E.g. with _|_ denoting point:
  ;;
  ;; foobar   _|_
  ;;       ^^^
  (followed-by-space nil :read-only t)
  ;; Symbol, one of 'literal-string, 'literal-string-no-space-at-end ,
  ;; 'function-with-side-effects, 'function-with-side-effects-and-args
  ;; or 'yas-snippet.
  (action-type nil :read-only t)
  ;; Action, type depends on value of action-type field.
  ;; For 'literal-string it's a string.
  ;; For 'literal-string-no-space-at-end it's a string.
  ;; For 'function-with-side-effects it's a function that inserts the
  ;; necessary content itself.
  ;; For 'function-with-side-effects-and-args it's a function that inserts the
  ;; necessary content itself and takes some args.
  ;; For 'yas-snippet it's a string that will be passed to `yas-expand-snippet'.
  (action-data nil :read-only t)
  ;; Function of no arguments, either symbol or lambda, that
  ;; will be called before performing ACTION and should return nil or t.
  ;; If t is returned then ACTION would be performed.
  ;;
  ;; Will be called with point just before the abbreviation text to be replaced
  ;; by the expansion.
  ;;
  ;; Optional.
  (predicate nil :read-only t)

  ;; Function of no arguments, either symbol or lambda, that
  ;; will be called before after ACTION. Its return value will be ignored.
  ;;
  ;; Optional.
  (on-successful-expansion nil :read-only t))

(cl-defun make-abbrev+-abbreviation (&key followed-by-space action-type action-data predicate on-successful-expansion)
  (cl-assert (memq followed-by-space '(nil t)))
  (cl-assert (or (null predicate) (functionp predicate)))
  (cl-assert (or (null on-successful-expansion)
                 (functionp on-successful-expansion)))
  (cl-assert (symbolp action-type))
  (cl-assert
   (pcase action-type
     ((or `literal-string `literal-string-no-space-at-end `yas-snippet)
      (cl-assert (stringp action-data))
      t)
     (`function-with-side-effects
      (cl-assert (or (symbolp action-data)
                     (functionp action-data)
                     (byte-code-function-p action-data)))
      t)
     (`function-with-side-effects-and-args
      (cl-assert (and (listp action-data)
                      (or (symbolp (car action-data))
                          (functionp (car action-data))
                          (byte-code-function-p (car action-data)))))
      t)
     (typ nil)))
  (make--abbrev+-abbreviation
   :followed-by-space followed-by-space
   :action-type action-type
   :action-data action-data
   :predicate predicate
   :on-successful-expansion on-successful-expansion))

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
          (`function-with-side-effects
           (cl-assert (or (symbolp data) (functionp data) (byte-code-function-p data)))
           (funcall data)
           nil)
          (`function-with-side-effects-and-args
           (cl-assert (listp data))
           (let ((func-name (car data))
                 (args (cdr data)))
             (cl-assert (or (symbolp func-name) (functionp func-name) (byte-code-function-p func-name)))
             (apply func-name args))
           nil)
          (`yas-snippet
           (cl-assert (stringp data))
           (yas-expand-snippet data)
           nil)
          (typ
           (error "Unknown action type %s of abbreviation %s" typ abbrev)))
      (awhen (abbrev+-abbreviation-on-successful-expansion abbrev)
        (funcall it)))))

(defun abbrev+-compile-abbreviations (abbrevs)
  (let ((space-followed (make-empty-trie))
        (regular (make-empty-trie)))
    (dolist (entry abbrevs)
      (cl-assert (consp entry))
      (let ((triggers (car entry))
            (abbrev   (cdr entry)))
        (dolist (key triggers)
          (cl-assert (stringp key))
          ;;(cl-assert (abbrev+-abbreviation-p abbrev))
          (let ((rk (reverse key)))
            (trie-insert-with! rk
                               abbrev
                               regular
                               (lambda (old _new)
                                 (error "Key %s already has abbreviation defined: %s" key old)))
            (when (abbrev+-abbreviation-followed-by-space abbrev)
              (trie-insert-with! rk
                                 abbrev
                                 space-followed
                                 (lambda (old _new)
                                   (error "Key %s already has abbreviation defined: %s" key old))))))))
    (cons (trie-opt-recover-sharing! space-followed)
          (trie-opt-recover-sharing! regular))))

(defvar-local abbrev+-abbreviations nil
  "Pair of tries produced by ‘abbrev+-compile-abbreviations’. car
is for matching abbrevs that could be delimited by a space (but
it’s not mandatory) and second ones that only should be expanded
if point is just after the trigger.")

(defvar-local abbrev+-do-not-expand-predicate nil
  "Global predicate, if it returns t then no expansions will be attempted.

nil value stands for no predicate and hence no such check.")

(defun abbrev+--syntax-changes-between? (pos1 pos2)
  "Check whether characters at POS1 and POS2 have different syntax."
  (let ((c1 (char-before pos1))
        (c2 (char-before pos2)))
    (cond
      (c1
       (if c2
           (not (eq (char-syntax c1)
                    (char-syntax c2)))
         t))
      (c2 t)
      (t nil))))

(defun abbrev+-expand ()
  "Expand text before point that matches against one of triggers
of `abbrev-abbreviations'. Returns boolean indicating whether
expansion was performed."
  (when (or (not abbrev+-do-not-expand-predicate)
            (not (funcall abbrev+-do-not-expand-predicate)))
    (let* ((line-start (line-beginning-position))
           (start (point))
           (p-vanilla start)
           p-followed
           (followed-trie (car abbrev+-abbreviations))
           (vanilla-trie (cdr abbrev+-abbreviations))
           found
           found-pt
           found-vanilla
           found-vanilla-pt
           found-followed
           found-followed-pt)
      (skip-syntax-backward " " line-start)
      (setf p-followed (point))
      (cl-assert (<= p-followed p-vanilla))

      (while (and
              ;; Don’t stop at first find, we want longest match. So continue while
              ;; we have tries that could match something at hand. Once tries
              ;; become nil we wouldn’t find anything an at that point we’ll use
              ;; whatever we found last - that’s the longest match.
              ;; (not found)
              (or followed-trie
                  vanilla-trie)
              (<= line-start p-followed))

        (let ((c-vanilla (char-before p-vanilla))
              (c-followed (char-before p-followed)))

          (setf followed-trie (and followed-trie
                                   (trie-lookup-node-char c-followed followed-trie))
                vanilla-trie (and vanilla-trie
                                  (trie-lookup-node-char c-vanilla vanilla-trie))))

        (when vanilla-trie
          (setf found-vanilla nil
                found-vanilla-pt nil))
        (when followed-trie
          (setf found-followed nil
                found-followed-pt nil))

        (let ((vanilla-value (trie-node-value-get vanilla-trie nil))
              (followed-value (trie-node-value-get followed-trie nil))
              (p-vanilla-prev (1- p-vanilla))
              (p-followed-prev (1- p-followed)))

          (when (and vanilla-value
                     followed-value
                     (not (eq vanilla-value followed-value)))
            (error "Conflict: two abbrevs matched at the same time module space following:\n----\n%s\n----\n%s"
                   vanilla-value
                   followed-value))
          (when (and vanilla-value
                     (abbrev+--syntax-changes-between? p-vanilla p-vanilla-prev))
            (setf found-vanilla vanilla-value
                  found-vanilla-pt p-vanilla))
          (when (and followed-value
                     (abbrev+--syntax-changes-between? p-followed p-followed-prev))
            (setf found-followed followed-value
                  found-followed-pt p-followed))

          ;; Move 1 character back.
          (setf p-vanilla p-vanilla-prev
                p-followed p-followed-prev)))

      (cond
        ((and found-vanilla-pt found-followed-pt)
         ;; Take the closest one to where we started
         (if (> found-vanilla-pt found-followed-pt)
             (setf found found-vanilla
                   found-pt found-vanilla-pt)
           (setf found found-followed
                 found-pt found-followed-pt)))
        (found-vanilla-pt
         (setf found found-vanilla
               found-pt found-vanilla-pt))
        (found-followed-pt
         (setf found found-followed
               found-pt found-followed-pt)))

      (let ((res (when found
                   (cl-assert (<= line-start found-pt))

                   (when (aif (abbrev+-abbreviation-predicate found)
                             (save-excursion
                               (goto-char (1- found-pt))
                               (funcall it))
                           ;; do substitution if no predicate supplied
                           t)
                     (delete-region (1- found-pt) start)
                     (let* ((insert-space? (abbrev+-perform-substitution found)))
                       (when (and insert-space?
                                  (or (eobp)
                                      (not (eq (char-after) ?\s))))
                         (insert ?\s))
                       ;; substitution was succesfull
                       t)))))
        (unless res
          (goto-char start))
        res))))

;;;###autoload
(defun abbrev+-insert-space-or-expand-abbrev (&optional dont-expand)
  (interactive (list current-prefix-arg))
  (when (or dont-expand
            (not (abbrev+-expand)))
    (insert-char ?\s)))

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
    (concat (substring str 0 suffix-len)
            (apply #'string
                   (funcall make-suffix
                            (string-to-list
                             (substring str suffix-len)))))))

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

(defun abbrev+--expand-name-part (x)
  (let ((name (car x))
        (count (cadr x))
        (limit (caddr x)))
    (cl-assert (stringp name))
    (cl-assert (or (null count) (numberp count)))
    (if count
        (let* ((prefix (substring name 0 count))
               (res nil))
          (cl-loop
           for i from count to (or limit (length name))
           do
           (push (concat prefix (substring name count i))
                 res))
          res)
      (list name))))

(defun abbrev+--list-product (xss)
  (if xss
      (let ((items (car xss)))
        (mapcan (lambda (rest)
                  (mapcar (lambda (x)
                            (concat x rest))
                          items))
                (abbrev+--list-product (cdr xss))))
    '(())))

(defun make-abbrev+-triggers-for-func-name (delims name-parts)
  "Return list of triggers matches emacs lisp function name, NAME-PARTS is
a list of the (str [start [end]]) elements, resulting re would
match part1-part2-...-partN with optional dashes and suffix
recognition."
  (declare (pure t) (side-effect-free t))
  (abbrev+--list-product
   (-interpose delims
               (-map #'abbrev+--expand-name-part
                     name-parts))))

(defun make-abbrev+-prefixes (s start &optional limit)
  (cl-assert (stringp s))
  (cl-assert (fixnump start))
  (cl-assert (< start (length s)))
  (let ((res nil))
    (cl-loop
     for i from start to (or limit (length s))
     do
     (push (substring s 0 i) res))
    res))

(provide 'abbrev+)

;; Local Variables:
;; End:

;; abbrev+.el ends here
