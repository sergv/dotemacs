;; sexpy-highlight.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  3 March 2012
;; Description:

;;;; Preamble

(setf ;; debug-on-error t
 *sexpy-debug* nil
 *sexpy-verbosity* 1
 message-log-max t
 )

(defun sexpy-trim-string (n str)
  (if (< n (length str))
    (subseq str 0 n)
    str))

(eval-when-compile
  (require 'cl-lib)

  (defmacro sexpy-dbg-message (verbosity &rest args)
    (when (and *sexpy-debug*
               (<= verbosity *sexpy-verbosity*))
      `(message ,@args))))

(require 'common)
(require 'lisp-jit-lock)


;;;;; Pattern matcher

;;; Patterns

;; (atom? <name>)
;; (skip-atom)
;; (skip-atom*)
;; (list? <name>)
;; (skip-everything) - skip ethier atom or list once
;; (skip-everything*) - skips until closing parethesis
;; (parenthesized <pattern>*) - encloses patterns into parenthesis
;; (scase
;;   ([ (<symbol>+) | atom? | list? | string? | anything? ] <pattern>*)*
;;   ([ t | else ] <patern>*)? )
;; (alt <atom-patten> <sexp-pattern>)
;; (tree name (<atom-to-track>*))
;; (tree-atoms name) <- collects all atoms in tree
;; (done) <- instantly stops matching and throws 'sexpy-match-done with current
;; dictionary

;; (repeat <pattern>*)
;; Note: repeat should be the latest pattern in enclosing list

(defmacro sexpy-assert* (form &optional message &rest args)
  `(unless ,form
     (error ,message ,@args)))

(defun sexpy-check-pattern (pattern)
  "Check pattern for various problems, errors, inconsistencies, misspellings,
misunderstandings, misguidings, malignant intentions, unintended typos, bugs
and general malfunctions."
  (cond
    ((sexpy-pattern-scase? pattern)
     (sexpy-assert* (every (lambda (entry)
                             (and (listp entry)
                                  (or (and (listp (car entry))
                                           (not (null (car entry)))
                                           (every #'symbolp
                                                  (car entry)))
                                      (eq t (car entry))
                                      (eq 'else (car entry))
                                      (eq 'atom? (car entry))
                                      (eq 'list? (car entry))
                                      (eq 'string? (car entry))
                                      (eq 'anything? (car entry)))))
                           (sexpy-scase-entries pattern))
                    "BAD SCASE PATTERN: %S" pattern)
     (flet ((get-pos (branch)
                     (position branch (sexpy-scase-entries pattern)
                               :test #'eq
                               :key #'car))
            (count-branches (branch)
                            (count branch (sexpy-scase-entries pattern)
                                   :test #'eq
                                   :key #'car))
            (has-branch? (branch)
                         (member* branch (sexpy-scase-entries pattern)
                                  :test #'eq
                                  :key #'car)))
       (sexpy-assert* (not (and (has-branch? t) (has-branch? 'else)))
                      "ONLY ONE OF T OR ELSE CASES SHOULD BE PRESENT IN %S"
                      pattern)
       (let ((atom-pos (get-pos 'atom?))
             (list-pos (get-pos 'list?))
             (string-pos (get-pos 'string?))
             (anything-pos (get-pos 'anything?))
             (t-pos (get-pos t))
             (else-pos (get-pos 'else)))
         (when (and atom-pos string-pos)
           (sexpy-assert* (< string-pos atom-pos)
                          "STRING CASE SHOULD GO BEFORE ATOM CASE IN %S"
                          pattern))
         (dolist (p (list t-pos else-pos))
           (when p
             (dolist (c (list atom-pos list-pos string-pos))
               (when c
                 (sexpy-assert* (< c p)
                                "ATOM, LIST AND STRING CASES SHOULD GO BEFORE T OR ELSE CASE IN %S"
                                pattern)))))
         (when anything-pos
           (dolist (c (list atom-pos list-pos string-pos))
             (when c
               (sexpy-assert* (< c anything-pos)
                              "ATOM, LIST AND STRING CASES SHOULD GO BEFORE ANYTHING CASE IN %S"
                              pattern)))))
       (dolist (count (list (count-branches 'atom?)
                            (count-branches 'list?)
                            (count-branches 'string?)
                            (count-branches 'anything?)
                            (count-branches t)
                            (count-branches 'else)))
         (when (and count
                    (< 0 count))
           (sexpy-assert* (= 1 count)
                          "NO DUPLICATE CASES ALLOWED IN SCASE PATTERN %S"
                          pattern))))
     (mapc (lambda (entry)
             (sexpy-check-pattern (cdr entry)))
           (sexpy-scase-entries pattern))
     t)
    ((sexpy-pattern-tree? pattern)
     (sexpy-assert* (symbolp (sexpy-pattern-name pattern))
                    "NAME SHOULD BE A SYMBOL: %S"
                    (sexpy-pattern-name))
     (sexpy-assert* (and (listp (sexpy-tree-permitted-atoms pattern))
                         (not (null (sexpy-tree-permitted-atoms pattern)))
                         (every #'symbolp (sexpy-tree-permitted-atoms pattern)))
                    "PERMITTED ATOMS OF TREE %S SHOULD BE NONEMPTY LIST OF SYMBOLS %S"
                    (sexpy-pattern-name pattern)
                    (sexpy-tree-permitted-atoms pattern)))
    ((sexpy-pattern-alt? pattern)
     (sexpy-assert* (= 3 (length pattern))
                    "ALT PATTERN MUST CONTAIN TWO CASES, ATOM AND SEXP")
     (sexpy-check-pattern (sexpy-alt-atom-case pattern))
     (sexpy-check-pattern (sexpy-alt-sexp-case pattern)))
    ((or (sexpy-pattern-parenthesized? pattern)
         (sexpy-pattern-repeat? pattern))
     (map #'sexpy-check-pattern (sexpy-pattern-body pattern)))
    ((sexpy-pattern-has-name? pattern)
     (sexpy-assert* (symbolp (sexpy-pattern-name pattern))
                    "NAME SHOULD BE A SYMBOL: %S"
                    (sexpy-pattern-name pattern))
     t)
    ((sexpy-single-pattern? pattern)
     t)
    ((and (listp pattern)
          (not (sexpy-single-pattern? pattern)))
     (let ((repeat-pos (position-if #'sexpy-pattern-repeat? pattern))
           (repeat-entry (find-if #'sexpy-pattern-repeat? pattern)))
       (when repeat-pos
         (sexpy-assert* (= repeat-pos (1- (length pattern)))
                        (concat "REPEAT PATTERN SHOULD BE THE LATEST "
                                "ENTRY IN THE ENCLOSING LIST %S")
                        pattern)))
     (mapc #'sexpy-check-pattern pattern)
     t)
    (t
     (error "BAD PATTERN: NEITHER SINGLE NOR COMBINATION: %S" pattern))))

(defun sexpy-list-with-head? (list head)
  (and (listp list)
       (eq head (car list))))

(defun sexpy-pattern-atom? (pattern)
  (sexpy-list-with-head? pattern 'atom?))

(defun sexpy-pattern-skip-atom? (pattern)
  (sexpy-list-with-head? pattern 'skip-atom))

(defun sexpy-pattern-skip-atom*? (pattern)
  (sexpy-list-with-head? pattern 'skip-atom*))

(defun sexpy-pattern-list? (pattern)
  (sexpy-list-with-head? pattern 'list?))

(defun sexpy-pattern-skip-everything? (pattern)
  (sexpy-list-with-head? pattern 'skip-everything))

(defun sexpy-pattern-skip-everything*? (pattern)
  (sexpy-list-with-head? pattern 'skip-everything*))

(defun sexpy-pattern-parenthesized? (pattern)
  (sexpy-list-with-head? pattern 'parenthesized))

(defun sexpy-pattern-repeat? (pattern)
  (sexpy-list-with-head? pattern 'repeat))

(defun sexpy-pattern-scase? (pattern)
  (sexpy-list-with-head? pattern 'scase))

(defun sexpy-pattern-alt? (pattern)
  (sexpy-list-with-head? pattern 'alt))

(defun sexpy-pattern-tree? (pattern)
  (sexpy-list-with-head? pattern 'tree))

(defun sexpy-pattern-tree-atoms? (pattern)
  (sexpy-list-with-head? pattern 'tree-atoms))

(defun sexpy-pattern-done? (pattern)
  (sexpy-list-with-head? pattern 'done))


(defun sexpy-single-pattern? (pattern)
  "Predicate on arbitrary lists, tries to determine whether given list
is a feasible pattern."
  (and (listp pattern)
       (symbolp (car pattern))
       (memq (car pattern)
             '(atom?
               skip-atom
               skip-atom*
               list?
               skip-everything
               skip-everything*
               parenthesized
               scase
               alt
               tree
               tree-atoms
               done

               repeat))))

(defun sexpy-pattern-has-name? (pattern)
  "Returns t if PATTERN has some name associated with it."
  (and (listp pattern)
       (or (sexpy-pattern-atom? pattern)
           (sexpy-pattern-list? pattern)
           (sexpy-pattern-tree? pattern)
           (sexpy-pattern-tree-atoms? pattern))))


(defun sexpy-pattern-name (pattern)
  (cond
    ((sexpy-pattern-has-name? pattern)
     (second pattern))
    (t
     (error "PATTERN %S HAS NO NAME" pattern))))

(defsetf sexpy-pattern-name (pattern) (new-name)
  `(cond
     ((sexpy-pattern-has-name? ,pattern)
      (setf (second ,pattern) ,new-name))
     (t
      (error "PATTERN %S HAS NO NAME" ,pattern))))


(defun sexpy-pattern-body (pattern)
  (cond
    ((or (sexpy-pattern-repeat? pattern)
         (sexpy-pattern-parenthesized? pattern))
     (cdr pattern))
    (t
     (error "PATTERN %S HAS NO SINGLE BODY" pattern))))

(defsetf sexpy-pattern-body (pattern) (new-body)
  `(cond
     ((or (sexpy-pattern-repeat? ,pattern)
          (sexpy-pattern-parenthesized? ,pattern))
      (setf (cdr ,pattern) ,new-body))
     (t
      (error "PATTERN %S HAS NO SINGLE BODY" ,pattern))))


(defun sexpy-scase-entries (pattern)
  (assert (sexpy-pattern-scase? pattern))
  (cdr pattern))

(defsetf sexpy-scase-entries (pattern) (new-entries)
  `(cond
     ((sexpy-pattern-scase? ,pattern)
      (setf (cdr ,pattern) ,new-entries))
     (t
      (error "NOT SCASE PATTERN: %s" pattern))))

(defun sexpy-alt-atom-case (pattern)
  (cadr pattern))

(defsetf sexpy-alt-atom-case (pattern) (new-case)
  `(cond
     ((sexpy-pattern-alt? ,pattern)
      (setf (cadr ,pattern) ,new-case))
     (t
      (error "NOT ALT PATTERN: %s" pattern))))

(defun sexpy-alt-sexp-case (pattern)
  (caddr pattern))

(defsetf sexpy-alt-sexp-case (pattern) (new-case)
  `(cond
     ((sexpy-pattern-alt? ,pattern)
      (setf (caddr ,pattern) ,new-case))
     (t
      (error "NOT ALT PATTERN: %s" pattern))))

(defun sexpy-tree-permitted-atoms (pattern)
  (assert (sexpy-pattern-tree? pattern))
  (third pattern))


(defun sexpy-pattern-extract-names (pattern)
  (labels ((extract (pattern names)
                    (cond
                      ((sexpy-pattern-has-name? pattern)
                       (cons (sexpy-pattern-name pattern)
                             names))
                      ((or (sexpy-pattern-repeat? pattern)
                           (sexpy-pattern-parenthesized? pattern))
                       (reduce
                        (lambda (names pat)
                          (extract pat names))
                        (sexpy-pattern-body pattern)
                        :initial-value names))
                      ((sexpy-pattern-scase? pattern)

                       (reduce
                        (lambda (names pat)
                          (extract pat names))
                        (map #'cdr
                             (sexpy-scase-entries pattern))
                        :initial-value names))
                      ((sexpy-pattern-alt? pattern)
                       (extract (sexpy-alt-sexp-case pattern)
                                (extract (sexpy-alt-atom-case pattern)
                                         names)))
                      ((and (not (null pattern))
                            (not (sexpy-single-pattern? pattern)))
                       (extract (cdr pattern)
                                (extract (car pattern)
                                         names)))
                      (t
                       names))))
    (nreverse (extract pattern nil))))

(defun sexpy-map-pattern-tree (pattern pred f)
  (flet ((transform-pattern (pat)
                            (sexpy-map-pattern-tree pat pred f)))
    (cond
      ((funcall pred pattern)
       (funcall f pattern))
      ((null pattern)
       nil)
      ;; ((or (sexpy-pattern-atom? pattern)
      ;;      (sexpy-pattern-skip-atom? pattern)
      ;;      (sexpy-pattern-skip-atom*? pattern)
      ;;      (sexpy-pattern-list? pattern)
      ;;      (sexpy-pattern-skip-everything? pattern)
      ;;      (sexpy-pattern-skip-everything*? pattern)
      ;;      (sexpy-pattern-tree? pattern)
      ;;      (sexpy-pattern-tree-atoms? pattern)
      ;;      (sexpy-pattern-done? pattern))
      ;;  pattern)
      ((or (sexpy-pattern-parenthesized? pattern)
           (sexpy-pattern-repeat? pattern))
       (let ((x (copy-list pattern)))
         (setf (sexpy-pattern-body x)
               (transform-pattern
                (sexpy-pattern-body x)))
         x))
      ((sexpy-pattern-scase? pattern)
       (let ((x (copy-list pattern)))
         (setf (sexpy-scase-entries x)
               (map (lambda (entry)
                      (cons (car entry)
                            (transform-pattern (cdr entry))))
                    (sexpy-scase-entries x)))
         x))
      ((sexpy-pattern-alt? pattern)
       (let ((x (copy-list pattern)))
         (setf (sexpy-alt-atom-case x)
               (transform-pattern (sexpy-alt-atom-case x)))
         (setf (sexpy-alt-sexp-case x)
               (transform-pattern (sexpy-alt-sexp-case x)))
         x))
      ((sexpy-single-pattern? pattern)
       pattern)
      (t
       (cons (transform-pattern (car pattern))
             (transform-pattern (cdr pattern)))))))

(defun sexpy-map-tree (tree pred f)
  (sexpy-map-pattern-tree tree pred f))

(defun sexpy-map-tree-deep (tree pred f)
  "Don't stop after first substitution."
  (labels ((recur-subst (pat)
                        (sexpy-map-tree (funcall f pat)
                                        pred
                                        #'recur-subst)))
    (sexpy-map-tree
     tree
     pred
     #'recur-subst)))

(defun sexpy-transform-pattern-to-numbers (pattern used-names)
  "Enumerate all placeholder names in PATTERN with numbers
starting from 0 and return two values: alist old names along with
their new numbers and new pattern.

USED-NAMES - list of names that will be actually used."
  (loop
    with pat = pattern
    for name in (intersection
                 (remove-duplicates (sexpy-pattern-extract-names pattern)
                                    :from-end t)
                 used-names)
    for i from 0
    collect (list name i) into names-dict
    do (setf pat (sexpy-map-tree
                  pat
                  (lambda (x)
                    (and (sexpy-single-pattern? x)
                         (sexpy-pattern-has-name? x)
                         (eq name (sexpy-pattern-name x))))
                  (lambda (x)
                    (let ((y (copy-list x)))
                      (setf (sexpy-pattern-name y) i)
                      y))))
    finally return (values names-dict
                           ;; and now we should eliminate all names that end
                           ;; up being non-numbers since they will not ever
                           ;; be used
                           (sexpy-map-tree
                            pat
                            (lambda (p)
                              (and (sexpy-pattern-has-name? p)
                                   (not (numberp (sexpy-pattern-name p)))))
                            (lambda (p)
                              '(skip-everything))))))


;;; pattern preprocessor

(defstruct (sexpy-preproc-directive
            (:constructor make-sexpy-preproc-directive
                          (tag args-count subst-procedure)))
  tag
  args-count
  subst-procedure)

(defvar *sexpy-preproc-registered-directives*
  nil
  "List of sexpy-preproc-directive structs.")

(defun sexpy-preproc-registered-directive? (tag)
  (member* tag *sexpy-preproc-registered-directives*
           :test #'eq
           :key #'sexpy-preproc-directive-tag))

(defun sexpy-preproc-register-directive (tag args-count subst-func)
  "TAG - symbol
PREDICATE, SUBST-FUNC - functions of one arg - part of pattern."
  (if (member* tag *sexpy-preproc-registered-directives*
               :test #'eq
               :key #'sexpy-preproc-directive-tag)
    (error "Directive %s is already registered" tag)
    (push (make-sexpy-preproc-directive tag args-count subst-func)
          *sexpy-preproc-registered-directives*)))

(defun sexpy-preproc-ungerister-directive (tag)
  (setf *sexpy-preproc-registered-directives*
        (remove* tag
                 *sexpy-preproc-registered-directives*
                 :test #'eq
                 :key #'sexpy-preproc-directive-tag)))

(defun sexpy-preprocess-pattern (pattern)
  (flet ((find-directive (pat)
                         (find-if (lambda (directive)
                                    (and (eq (car pat)
                                             (sexpy-preproc-directive-tag directive))
                                         (= (sexpy-preproc-directive-args-count
                                             directive)
                                            (length (cdr pat)))))
                                  *sexpy-preproc-registered-directives*)))
    (sexpy-map-tree-deep
     pattern
     (lambda (pat)
       (and (listp pat)
            (find-directive pat)))
     (lambda (pat)
       (let ((directive (find-directive pat)))
         (apply (sexpy-preproc-directive-subst-procedure
                 directive)
                (cdr pat)))))))

;;; dictionaries go here

(defun sexpy-dict-value (dict key)
  "Get value of cell in DICT referenced by KEY. For internal use only."
  (aref (dict-bindings dict) key))

(defun sexpy-dict-push-value (dict key value)
  (setf (aref (dict-bindings dict) key)
        (cons value
              (aref (dict-bindings dict) key)))
  dict)

(defun sexpy-dict-pop-value (dict key)
  "Return next value in DICT that corresponds to KEY and delete
it from dict."
  (pop (aref (dict-bindings dict) key)))

(defun sexpy-dict-pop-values (dict key)
  "Return all values in DICT under KEY and delete them from dict."
  (prog1 (aref (dict-bindings dict) key)
    (setf (aref (dict-bindings dict) key) nil)))

(defun sexpy-dict-get-values (dict key)
  "Get all values in DICT that correspond to KEY."
  (aref (dict-bindings dict) key))

(defun sexpy-dict-empty? (dict)
  "Return t if there's no entries in DICT."
  (loop
    for v across (dict-bindings dict)
    always (null v)))

(defun sexpy-add-to-dict (dict pattern beg end &optional repeating)
  "Add entry with BEG and END to DICT depending on PATTERN and REPEATING.
PATTERN provides name for dict entry (which is a number, translation is done
when compiling and checking patterns in high-level macros) and type of addition
to use.
If REPEATING is nil then error is signaled when adding of entry is issued
for name which is already defined, else entries are stacked into list."
  (cond
    ((and (not repeating)
          (or (sexpy-pattern-atom? pattern)
              (sexpy-pattern-list? pattern)))
     (let ((key (sexpy-pattern-name pattern)))
       (when (sexpy-dict-value dict key)
         (error "KEY %S IS ALREADY DEFINED IN DICTIONARY %S"
                (sexpy-pattern-name pattern)
                dict))
       (sexpy-dict-push-value dict
                              key
                              (list beg end
                                    ;; (buffer-substring-no-properties beg end)
                                    ))))
    ((or (and (not (null repeating))
              (or (sexpy-pattern-atom? pattern)
                  (sexpy-pattern-list? pattern)))
         (sexpy-pattern-tree? pattern)
         (sexpy-pattern-tree-atoms? pattern))
     (let* ((key (sexpy-pattern-name pattern)))
       (sexpy-dict-push-value dict
                              key
                              (list beg end
                                    ;; (buffer-substring-no-properties beg end)
                                    ))))
    (t
     (error "ADD-TO-DICT: UNSUPPORTED PATTERN %S" pattern))))

(defstruct* (dict
             (:constructor make--dict (failed? bindings)))
  failed?
  bindings)

(defun sexpy-make-dict (failed? size)
  "Make new dictionary object with reserved space SIZE and failed flag
FAILED?."
  (make--dict failed?
              (make-vector size nil)))

(defun sexpy-make-dict-for-pattern (pattern)
  "Create dictionary object specific for pattern and it's names."
  (sexpy-make-dict
   nil
   (length (remove-duplicates (sexpy-pattern-extract-names pattern)))))

(defun sexpy-mark-dict-as-failed (dict)
  "Destructively mark DICT as failed one."
  (let ((d dict))
    (setf (dict-failed? d) t)
    ;; (message "LOOKING AT: %S"
    ;;          (buffer-substring-no-properties (point)
    ;;                                          (point-max)))
    d))

(defun sexpy-mark-dict-as-ok (dict)
  (let ((d dict))
    (setf (dict-failed? d) nil)
    d))

(defun sexpy-clear-dict (dict)
  (loop
    for i below (length (dict-bindings dict))
    do (setf (aref (dict-bindings dict) i) nil)))

(defconst +sexpy-failed-dict+ (sexpy-make-dict t 0))

(defsubst sexpy-dict-entry-beg (entry)
  (car entry))

(defsubst sexpy-dict-entry-end (entry)
  (cadr entry))


;;; functions related to buffer, its content and position and navigation

(defun sexpy-skip-whitespace ()
  (skip-syntax-forward " >")
  (cond
    ;; some simple reader macro
    ((looking-at-p "#[+-]")
     (sexpy-forward-sexp-safe +sexpy-failed-dict+ 1)
     (sexpy-skip-whitespace))
    ((looking-at-p ";")
     (end-of-line)
     (sexpy-skip-whitespace))
    ;; do nothing
    (t)))

(defsubst sexpy-at-beginning-of-sexp? ()
  (and (not (eobp))
       (char= ?\( (char-after))))

(defsubst sexpy-at-end-of-sexp? ()
  (and (not (eobp))
       (char= ?\) (char-after))))

(defsubst sexpy-at-beginning-of-string? ()
  (and (not (eobp))
       (char= ?\" (char-syntax (char-after)))))

(defun sexpy-inside-string-or-comment? ()
  "Return t if point is positioned inside a string."
  (save-excursion
    (let* ((end (point))
           (begin (line-beginning-position))
           (state (parse-partial-sexp begin end)))
      (or (elt state 3)
          (elt state 4)))))

(defun* sexpy-forward-sexp-safe (dict &optional (count 1))
  (condition-case err
      (let ((parse-sexp-ignore-comments t))
        (let ((p (scan-sexps (point) count)))
          (sexpy-dbg-message 4 "FORWARD-SEXP-SAFE: MOVING THROUGH %S"
                             (buffer-substring-no-properties (point)
                                                             (or p (point-max))))
          (goto-char (or p (point-max)))))
    (error (message "FORWARD-SEXP-SAFE: ERROR: %s %s" (car err) (cdr err))
           (throw 'sexpy-match-fail (sexpy-mark-dict-as-failed dict)))))

(defsubst sexpy-end-of-sexp-at-point ()
  (assert (lisp-pos-is-beginning-of-list? (point)))
  (or (scan-sexps (point) 1) (point-max)))

(defmacro sexpy-forward-sexp-with-bounds (dict begin end &rest body)
  `(let ((,begin (point))
         (,end nil))
     (sexpy-forward-sexp-safe ,dict 1)
     (setf end (point))
     ,@body))

(defsubst sexpy-end-of-limit? (limit)
  (or (eobp)
      (<= limit (point))))

;;; sexpy-matcher itself

(defun sexpy-match (pattern dict limit &optional repeating)
  (catch 'sexpy-match-fail
    (cond
      ((<= limit (point))
       dict)
      ((dict-failed? dict)
       (sexpy-dbg-message 2 "FAILED DICT")
       dict)
      ((sexpy-pattern-done? pattern)
       (throw 'sexpy-match-done dict))
      ((sexpy-pattern-atom? pattern)
       (sexpy-dbg-message 3 "PATTERN-ATOM %S" pattern)
       (sexpy-skip-whitespace)
       (if (sexpy-at-beginning-of-sexp?)
         (sexpy-mark-dict-as-failed dict)
         (sexpy-forward-sexp-with-bounds dict begin end
                                         (sexpy-add-to-dict dict pattern begin end repeating))))
      ((sexpy-pattern-list? pattern)
       (sexpy-dbg-message 3 "PATTERN-LIST %S" pattern)
       (sexpy-skip-whitespace)
       (if (sexpy-at-beginning-of-sexp?)
         (sexpy-forward-sexp-with-bounds dict begin end
                                         (sexpy-add-to-dict dict pattern begin end repeating))
         (sexpy-mark-dict-as-failed dict)))

      ((sexpy-pattern-skip-atom? pattern)
       (sexpy-dbg-message 3 "PATTERN-SKIP-ATOM")
       (sexpy-skip-whitespace)
       (if (sexpy-at-beginning-of-sexp?)
         (sexpy-mark-dict-as-failed dict)
         (progn
           (sexpy-forward-sexp-safe dict 1)
           dict)))
      ((sexpy-pattern-skip-atom*? pattern)
       (sexpy-dbg-message 3 "PATTERN-SKIP-ATOM*")
       (sexpy-skip-whitespace)
       (while (and (not (sexpy-at-end-of-sexp?))
                   (not (sexpy-at-beginning-of-sexp?))
                   (not (sexpy-end-of-limit? limit)))
         (sexpy-forward-sexp-safe dict 1)
         (sexpy-skip-whitespace))
       dict)
      ((sexpy-pattern-skip-everything? pattern)
       (sexpy-dbg-message 3 "PATTERN-SKIP-EVERYTHING")
       (sexpy-skip-whitespace)
       (if (or (sexpy-at-end-of-sexp?)
               (sexpy-end-of-limit? limit))
         (sexpy-mark-dict-as-failed dict)
         (prog1 dict
           (sexpy-forward-sexp-safe dict 1))))
      ((sexpy-pattern-skip-everything*? pattern)
       (sexpy-dbg-message 3 "PATTERN-SKIP-EVERYTHING*")
       (sexpy-skip-whitespace)
       (while (and (not (sexpy-at-end-of-sexp?))
                   (not (sexpy-end-of-limit? limit)))
         (sexpy-forward-sexp-safe dict 1)
         (sexpy-skip-whitespace))
       dict)

      ((sexpy-pattern-repeat? pattern)
       (sexpy-dbg-message 3 "PATTERN REPEAT %S" pattern)
       (sexpy-skip-whitespace)
       (while (and (not (sexpy-at-end-of-sexp?))
                   (not (sexpy-end-of-limit? limit))
                   (not (dict-failed? dict)))
         (do ((pat (sexpy-pattern-body pattern) (cdr pat)))
             ((or (null pat)
                  (dict-failed? dict)
                  (sexpy-end-of-limit? limit)
                  (sexpy-at-end-of-sexp?)))
           (setf dict (sexpy-match (car pat) dict limit t))
           (sexpy-skip-whitespace))
         (sexpy-skip-whitespace))
       dict)
      ((sexpy-pattern-scase? pattern)
       (sexpy-dbg-message 3 "PATTERN SCASE %S" pattern)
       (sexpy-skip-whitespace)
       (labels ((iterate (entries)
                         (cond
                           ((null entries)
                            (sexpy-mark-dict-as-failed dict))
                           ((and (not (sexpy-at-end-of-sexp?))
                                 (not (sexpy-end-of-limit? limit))
                                 (or (and (eq 'atom? (caar entries))
                                          (not (sexpy-at-beginning-of-sexp?)))
                                     (and (eq 'list? (caar entries))
                                          (sexpy-at-beginning-of-sexp?))
                                     (and (eq 'string? (caar entries))
                                          (sexpy-at-beginning-of-string?))
                                     (eq 'anything? (caar entries))))
                            (sexpy-match (cdar entries) dict limit repeating))
                           ((or (eq t (caar entries))
                                (eq 'else (caar entries))
                                (and (listp (caar entries))
                                     (looking-at-p (concat "\\(?:"
                                                           (mapconcat #'symbol-name
                                                                      (caar entries)
                                                                      "\\|")
                                                           "\\)\\_>"))))
                            (sexpy-match (cdar entries) dict limit repeating))
                           (t
                            (iterate (cdr entries))))))
         (iterate (sexpy-scase-entries pattern))))
      ((sexpy-pattern-alt? pattern)
       (sexpy-dbg-message 3 "PATTERN ALT %S" pattern)
       (sexpy-skip-whitespace)
       (if (sexpy-at-beginning-of-sexp?)
         (sexpy-match (sexpy-alt-sexp-case pattern) dict limit repeating)
         (sexpy-match (sexpy-alt-atom-case pattern) dict limit repeating)))
      ((sexpy-pattern-tree? pattern)
       (sexpy-dbg-message 3 "PATTERN TREE %S" pattern)
       (let ((atoms-re (concat (mapconcat #'symbol-name
                                          (sexpy-tree-permitted-atoms pattern)
                                          "\\|")
                               "\\_>")))
         (labels ((traverse-tree (d)
                                 (sexpy-skip-whitespace)
                                 (cond
                                   ((sexpy-at-beginning-of-sexp?)
                                    (forward-char 1)
                                    (sexpy-dbg-message 4 "****OPENING PAREN****")
                                    (while (and (not (sexpy-at-end-of-sexp?))
                                                (not (sexpy-end-of-limit? limit)))
                                      (setf d (traverse-tree d)))
                                    (when (sexpy-at-end-of-sexp?)
                                      (forward-char 1)
                                      (sexpy-dbg-message 4 "****CLOSING PAREN****"))
                                    d)
                                   ((looking-at-p atoms-re)
                                    (sexpy-forward-sexp-with-bounds
                                     d begin end
                                     (sexpy-add-to-dict d pattern begin end repeating)))
                                   (t
                                    (sexpy-forward-sexp-safe d 1)
                                    d))))
           (traverse-tree dict))))
      ((sexpy-pattern-tree-atoms? pattern)
       (sexpy-dbg-message 3 "PATTERN TREE ATOMS %S" pattern)
       (labels ((traverse-tree (d)
                               (sexpy-skip-whitespace)
                               (cond
                                 ((sexpy-at-beginning-of-sexp?)
                                  (forward-char 1)
                                  (sexpy-dbg-message 4 "****OPENING PAREN****")
                                  (while (and (not (sexpy-at-end-of-sexp?))
                                              (not (sexpy-end-of-limit? limit)))
                                    (setf d (traverse-tree d)))
                                  (when (sexpy-at-end-of-sexp?)
                                    (forward-char 1)
                                    (sexpy-dbg-message 4 "****CLOSING PAREN****"))
                                  d)
                                 (t
                                  (sexpy-forward-sexp-with-bounds
                                   d begin end
                                   (sexpy-add-to-dict d pattern begin end repeating))))))
         (traverse-tree dict)))

      ;; it's combined pattern and it's first element is
      ;; list - we should recurse there and also match open parenthesis
      ((sexpy-pattern-parenthesized? pattern)
       (sexpy-dbg-message 3 "PARENTHESIZED PATTERN %S"
                          pattern)
       (sexpy-dbg-message 1 "LOOKING-AT: %S"
                          (sexpy-trim-string
                           50
                           (buffer-substring-no-properties (point)
                                                           limit
                                                           ;; (point-max)
                                                           )))
       (sexpy-skip-whitespace)
       (if (sexpy-at-beginning-of-sexp?)
         (progn
           (forward-char 1)
           (sexpy-dbg-message 4 "****OPENING PAREN****")

           (let ((new-dict (sexpy-match (sexpy-pattern-body pattern)
                                        dict
                                        limit
                                        repeating)))
             (sexpy-skip-whitespace)
             (if (sexpy-at-end-of-sexp?)
               (progn
                 (forward-char 1)
                 (sexpy-dbg-message 4 "****CLOSING PAREN****")
                 new-dict)
               (sexpy-mark-dict-as-failed new-dict))))
         (sexpy-mark-dict-as-failed dict)))
      ((null pattern)
       (sexpy-dbg-message 3 "NULL PATTERN")
       dict)
      (t
       (sexpy-dbg-message 3 "COMBINED PATTERN %S" pattern)
       (sexpy-match (cdr pattern)
                    (sexpy-match (car pattern)
                                 dict
                                 limit
                                 repeating)
                    limit
                    repeating)))))


;;;;; useful CL preprocessor directives

;; (cl-types types-var)
(unless (sexpy-preproc-registered-directive? 'cl-types)
  (sexpy-preproc-register-directive
   'cl-types
   1
   (lambda (cl-type-var)
     `(tree
       ,cl-type-var
       (and
        function
        simple-array
        arithmetic-error
        generic-function
        simple-base-string
        array
        hash-table
        simple-bit-vector
        atom
        integer
        simple-condition
        base-char
        keyword
        simple-error
        base-string
        list
        simple-string
        bignum
        logical-pathname
        simple-type-error
        bit
        long-float
        simple-vector
        bit-vector
        member
        simple-warning
        broadcast-stream
        method
        single-float
        built-in-class
        method-combination
        standard-char
        cell-error
        mod
        standard-class
        character
        nil
        standard-generic-function
        class
        not
        standard-method
        compiled-function
        null
        standard-object
        complex
        number
        storage-condition
        concatenated-stream
        or
        stream
        condition
        package
        stream-error
        cons
        package-error
        string
        control-error
        parse-error
        string-stream
        division-by-zero
        pathname
        structure-class
        double-float
        print-not-readable
        structure-object
        echo-stream
        program-error
        style-warning
        end-of-file
        random-state
        symbol
        eql
        ratio
        synonym-stream
        error
        rational
        t
        extended-char
        reader-error
        two-way-stream
        file-error
        readtable
        type-error
        file-stream
        real
        unbound-slot
        fixnum
        restart
        unbound-variable
        float
        satisfies
        undefined-function
        floating-point-inexact
        sequence
        unsigned-byte
        floating-point-invalid-operation
        serious-condition
        values
        floating-point-overflow
        short-float
        vector
        floating-point-underflow
        signed-byte
        warning)))))

;; (type type-var)
(unless (sexpy-preproc-registered-directive? 'type)
  (sexpy-preproc-register-directive
   'type
   1
   (lambda (type-var)
     `(scase
       (string?
        (skip-everything))
       (atom?
        (atom? ,type-var))
       (list?
        (cl-types ,type-var))
       (t)))))

;; (lambda-args arg-var)
(unless (sexpy-preproc-registered-directive? 'lambda-args)
  (sexpy-preproc-register-directive
   'lambda-args
   1
   (lambda (arg)
     `(alt
       (skip-everything)
       (parenthesized
        (repeat
         (scase
          ((&optional &rest &key &aux)
           (skip-everything))
          (atom?
           (atom? ,arg))
          (list?
           (parenthesized
            (atom? ,arg)
            (skip-everything*)))
          (t
           (skip-everything*)))))))))

;; (lambda-args+specifiers arg-var specifiers-var)
(unless (sexpy-preproc-registered-directive? 'lambda-args+specifiers)
  (sexpy-preproc-register-directive
   'lambda-args+specifiers
   2
   (lambda (arg arg-specifier)
     `(alt
       (skip-everything)
       (parenthesized
        (repeat
         (scase
          ((&optional &rest &key &aux)
           (atom? ,arg-specifier))
          (atom?
           (atom? ,arg))
          (list?
           (parenthesized
            (atom? ,arg)
            (skip-everything*)))
          (t
           (skip-everything*)))))))))

;; (declaration-specifier keyword affected-var type-var)
(unless (sexpy-preproc-registered-directive? 'declaration-specifier)
  (sexpy-preproc-register-directive
   'declaration-specifier
   3
   (lambda (keyword affected-var type-var)
     `(parenthesized
       (scase
        ((dynamic-extent ignore ignorable)
         (atom? ,keyword)
         (repeat
          (alt (atom? ,affected-var)
               (parenthesized
                (skip-everything)
                (atom? ,affected-var)))))
        ((optimize)
         (atom? ,keyword)
         (repeat
          (scase
           ((compilation-speed
             debug
             safety
             space
             speed)
            (atom? ,keyword))
           (list?
            (parenthesized
             (scase
              ((compilation-speed
                debug
                safety
                space
                speed)
               (atom? ,keyword)
               (skip-everything))
              (t
               (skip-everything*)))))
           (t
            (skip-everything*)))))
        ((ftype type)
         (atom? ,keyword)
         (type ,type-var)
         (repeat
          (atom? ,affected-var)))
        ((inline notinline special)
         (atom? ,keyword)
         (repeat
          (atom? ,affected-var)))
        (t))))))

;; (sexpy-preproc-ungerister-directive 'declaration-specifier)

;; (optional-doc doc-var)
(unless (sexpy-preproc-registered-directive? 'optional-doc)
  (sexpy-preproc-register-directive
   'optional-doc
   1
   (lambda (doc-var)
     `(scase
       (string? (atom? ,doc-var))
       (t)))))

;;;;; fontification functions

;;; prelude

(defmacro preserve-buffer-undo-list (&rest body)
  (let ((hist-sym (gensym)))
    `(let ((,hist-sym buffer-undo-list))
       (message "PRESERVE-BUFFER-UNDO-LIST: BUFFER-UNDO-LIST LENGTH BEFORE: %s"
                (length buffer-undo-list))
       (setf buffer-undo-list t)
       (unwind-protect
           (progn ,@body)
         (setf buffer-undo-list ,hist-sym))
       (message "PRESERVE-BUFFER-UNDO-LIST: BUFFER-UNDO-LIST LENGTH AFTER: %s"
                (length buffer-undo-list)))))

(require 'elp)

(defvar *sexpy-pattern-fontify-form-regexp*
  "(\\(?:\\sw\\|\\s_\\)+\\_>"
  "Regexp used to find form start relative to current position to
initiate fontification update.")

(defun sexpy-refresh-pattern-fontify-re-cache ()
  (setf *sexpy-pattern-fontify-form-regexp*
        (concat "("
                (regexp-opt *sexpy-pattern-fontify-defined-heads*)
                "\\_>")))


(defvar *sexpy-pattern-fontify-defined-heads* nil
  "List of head strings for which have matchers defined.")

(defun sexpy-pattern-fontify-up-list (&optional up-list-limit)
  "Move up list until either 0 column is reached, found sexp with
matching `*sexpy-pattern-fontify-form-regexp*' or UP-LIST-LIMIT
reaches zero in case it was non-nil.

If UP-LIST-LIMIT was specified function returns t if suitable sexp was
found or nil otherwise."
  (let ((case-fold-search t))
    ;; (skip-syntax-forward " <>")
    (skip-syntax-forward "^()")
    (while (and (or (null up-list-limit)
                    (< 0 up-list-limit))
                (not (= 0 (current-column)))
                (not (looking-at-p *sexpy-pattern-fontify-form-regexp*)))
      (backward-up-list)
      (when up-list-limit
        (decf up-list-limit)))
    (when (not (null up-list-limit))
      (looking-at-p *sexpy-pattern-fontify-form-regexp*))))


(defun* sexpy-update-ordered-list (list
                                   new-entry
                                   &key
                                   (cmp-pred #'<)
                                   (eq-pred #'=))
  "CMP-PRED takes two entries and returns t if left value should go before
right one in LIST. EQ-PRED is used to remove old value from list."
  (labels ((insert-new-entry (list)
                             (cond
                               ((null list)
                                (list new-entry))
                               ((funcall cmp-pred new-entry (car list))
                                (cons new-entry list))
                               (t
                                (cons (car list)
                                      (insert-new-entry (cdr list)))))))
    (insert-new-entry (remove* new-entry list :test eq-pred))))

(defmacro sexpy-define-comparer (name cmp-func access-func)
  (let ((arg1 (gensym))
        (arg2 (gensym)))
    `(defsubst ,name (,arg1 ,arg2)
       (funcall ,cmp-func
                (funcall ,access-func ,arg1)
                (funcall ,access-func ,arg2)))))

;;; pattern fontifiers

(defstruct (sexpy-pattern-fontifier
            (:constructor make-sexpy-pattern-fontifier
                          (head-regexp function priority)))
  head-regexp
  function ;; symbol for now
  priority)

(sexpy-define-comparer
 sexpy-pattern-fontifier-lesser-priority?
 #'<
 #'sexpy-pattern-fontifier-priority)

(sexpy-define-comparer
 sexpy-pattern-fontifier-equal?
 #'string=
 #'sexpy-pattern-fontifier-head-regexp)


(defun sexpy-re-search-forward-beg-noerr-no-comments-no-case-tagged (regexp limit tag)
  "This is similar to `re-search-forward', but search in case-insensetive,
maches inside strings and comments are ignored, and matches that have 'sexpy-tagged
text property not equal to TAG are ignored too."
  (let ((found nil)
        (case-fold-search t))
    (while (and (not found)
                (re-search-forward regexp limit t))
      (when (and (not (sexpy-inside-string-or-comment?))
                 (let ((prop (get-text-property (match-end 0) 'sexpy-tagged)))
                   (or (null prop)
                       (eq tag prop))))
        (goto-char (match-beginning 0))
        (setf found t)))
    found))

(defmacro* sexpy-define-fontifier (name
                                   head-re
                                   pattern
                                   name-face-alist
                                   &key
                                   (names-to-track nil))
  "Define pattern-based fontifier and corresponding dictionary for it's use.

HEAD-RE - regexp to match beginning of sexp on which PATTERN should operate.

PATTERN - tree for use with `sexpy-match'.

NAME-FACE-ALIST - alist of (name face) pairs, name and face being both symbols.
Every name should reference name from PATTERN.

NAMES-TO-MARK - list of names that should be marked during fontification to prevent
their fontification twice (or n times depending on your luck) by other pattern-based
fontifiers."
  (let ((names (map #'car name-face-alist))
        (raw-pattern (sexpy-preprocess-pattern pattern)))

    ;; satefy checks
    (sexpy-check-pattern raw-pattern)
    (let* ((sexpy-pattern-names (sexpy-pattern-extract-names raw-pattern))
           (diff (set-difference names sexpy-pattern-names))
           (mark-diff (set-difference names-to-track sexpy-pattern-names)))
      (when diff
        (error "NAMES %S DO NOT APPEAR IN PATTERN %S"
               diff
               raw-pattern))
      (when mark-diff
        (error "THESE NAMES TO MARK %S DO NOT APPEAR IN PATTERN %S"
               mark-diff
               raw-pattern)))

    (let ((fontifier-name name ;; (intern (symbol-name (gensym "MATCHER-")))
                          )
          (dict-name (intern (symbol-name (gensym "FONTIFIER-DICT-")))))
      (multiple-value-bind (name-num-alist num-pattern)
          (sexpy-transform-pattern-to-numbers raw-pattern names)

        (let ((num-face-alist
               (sort (map (lambda (x)
                            (let ((name (first x)))
                              (list (second (assoc* name name-num-alist
                                                    :test #'eq))
                                    (second x))))
                          name-face-alist)
                     (lambda (a b) (< (car a) (car b)))))
              (nums-to-track (map (lambda (name)
                                    (cadr (assoc name name-num-alist)))
                                  names-to-track)))
          `(progn
             (defvar ,dict-name ,(sexpy-make-dict-for-pattern num-pattern))

             (defun ,fontifier-name (limit)
               (save-match-data
                 (while (sexpy-re-search-forward-beg-noerr-no-comments-no-case-tagged
                         ,head-re
                         limit
                         ',fontifier-name)
                   (when (dict-failed? ,dict-name)
                     (sexpy-mark-dict-as-ok ,dict-name))
                   ;; push limit somewhat futher
                   (setf limit (max limit
                                    (sexpy-end-of-sexp-at-point)))
                   (setf ,dict-name (catch 'sexpy-match-done
                                      (sexpy-match ',num-pattern
                                                   ,dict-name
                                                   limit)))
                   (loop
                     for num in ',nums-to-track
                     do (dolist (val (sexpy-dict-get-values ,dict-name num))
                          (put-text-property (sexpy-dict-entry-beg val)
                                             (sexpy-dict-entry-end val)
                                             'sexpy-tagged
                                             ;; sign this word with unique fontifier
                                             ',fontifier-name)))
                   (loop
                     for (num face) in ',num-face-alist
                     do (dolist (val (sexpy-dict-pop-values ,dict-name num))
                          (put-text-property (sexpy-dict-entry-beg val)
                                             (sexpy-dict-entry-end val)
                                             'font-lock-face
                                             face)))
                   (sexpy-clear-dict ,dict-name)
                   (when (< limit (point))
                     (goto-char limit)))))))))))

(defvar *sexpy-lisp-pattern-fontifiers* nil
  "List of either pattern-based or regexp based fontification procedures of
one argument. Entries are ordered by decrease of priority.")

(defmacro* sexpy-define-pattern-fontifier (head
                                           pattern
                                           name-face-alist
                                           &key
                                           (priority 0)
                                           (names-to-track nil))
  "This is front-end to `sexpy-define-fontifier', and it keeps track of
priorities, defined fontifiers and their regexp heads and
sometimes does profiling with elp package."
  (let ((name (cond
                ((listp head)
                 (intern (concat (mapconcat (lambda (x)
                                              (upcase
                                               (symbol-name x)))
                                            head
                                            "-")
                                 "-FONTIFIER")))
                (t
                 (error "BAD HEAD %S" head))))
        (head-re (concat
                  "(\\(?:"
                  (cond
                    ((and (listp head)
                          (every #'symbolp head))
                     (regexp-opt (map #'symbol-name head)))
                    (t
                     (error "HEAD MUST BE LIST OF SYMBOLS")))
                  "\\)\\_>")))
    `(progn
       ;; (when (intersection (map #'symbol-name head)
       ;;                     *sexpy-pattern-fontify-defined-heads*
       ;;                     :test #'string=)
       ;;   (error "SYMBOLS %s ARE ALREADY DEFINED"
       ;;          (intersection (map #'symbol-name head)
       ;;                        *sexpy-pattern-fontify-defined-heads*
       ;;                        :test #'string=)))

       (sexpy-define-fontifier
        ,name
        ,head-re
        ,pattern
        ,name-face-alist
        :names-to-track ,names-to-track)

       (setf *sexpy-lisp-pattern-fontifiers*
             (sexpy-update-ordered-list
              *sexpy-lisp-pattern-fontifiers*
              (make-sexpy-pattern-fontifier ,head-re
                                            #',name
                                            ,priority)
              :cmp-pred #'sexpy-pattern-fontifier-lesser-priority?
              :eq-pred #'sexpy-pattern-fontifier-equal?))

       ,(cond
          ((listp head)
           `(setf *sexpy-pattern-fontify-defined-heads*
                  (append ',(map #'symbol-name head)
                          *sexpy-pattern-fontify-defined-heads*)))
          (t
           (error "BAD HEAD %S" head)))

       (elp-instrument-function #',name))))

;;; fontification routines

(defun sexpy-extend-font-lock-region-after-change (beg end old-len)
  (save-excursion
    (goto-char beg)
    (condition-case nil
        (when (sexpy-pattern-fontify-up-list 4)
          (cons (point)
                (sexpy-end-of-sexp-at-point)))
      (error nil))))

(defun sexpy-set-up-fontification ()
  (sexpy-refresh-pattern-fontify-re-cache)
  (jit-lock-register #'sexpy-font-lock-fontify-region t)
  (setf font-lock-extend-after-change-region-function
        #'sexpy-extend-font-lock-region-after-change)
  ;; kinda ugly
  (run-with-timer 0.5 nil #'font-lock-fontify-buffer))

(defun sexpy-font-lock-fontify-region (beg end)
  "Fontify region between BEG and END points."
  (let ((modification-flag (buffer-modified-p)))
    (let ((inhibit-modification-hooks t)
          (inhibit-point-motion-hooks t)
          (inhibit-redisplay t))
      (save-excursion
        (dolist (pat-fontifier *sexpy-lisp-pattern-fontifiers*)
          (goto-char beg)
          (funcall (sexpy-pattern-fontifier-function pat-fontifier)
                   end))))
    ;; restore flag and force redisplay
    (set-buffer-modified-p modification-flag)))

;;;;; fontification definitions

(sexpy-define-pattern-fontifier (defpackage)
                                (parenthesized
                                 (atom? head)
                                 (atom? package-name)
                                 (repeat
                                  (parenthesized
                                   (scase
                                    ((:shadow :export :intern)
                                     (atom? section-type)
                                     (repeat (atom? values)))
                                    ((:nicknames :use)
                                     (atom? section-type)
                                     (repeat (atom? package-name)))
                                    ((:documentation)
                                     (atom? section-type)
                                     (atom? doc))
                                    ((:import-from :shadowing-import-from)
                                     (atom? section-type)
                                     (atom? package-name)
                                     (repeat (atom? values)))
                                    ((:size)
                                     (atom? section-type)
                                     (skip-everything))
                                    (t
                                     (skip-everything*))))))
                                ((head         ansi-lisp-keyword-face)
                                 (package-name ansi-lisp-defined-data-name-face)
                                 (doc          ansi-lisp-doc-face)
                                 (section-type ansi-lisp-keyword-face)
                                 (values       ansi-lisp-exported-symbols-face))
                                :priority 10)

(sexpy-define-pattern-fontifier (in-package)
                                (parenthesized
                                 (atom? head)
                                 (atom? package-name))
                                ((head         ansi-lisp-keyword-face)
                                 (package-name ansi-lisp-defined-data-name-face)))

(sexpy-define-pattern-fontifier (use-package)
                                (parenthesized
                                 (atom? head)
                                 (repeat
                                  (atom? package-name)))
                                ((head         ansi-lisp-keyword-face)
                                 (package-name ansi-lisp-defined-data-name-face)))

(sexpy-define-pattern-fontifier (defun defmacro)
                                (parenthesized
                                 (atom? head)
                                 (atom? name)
                                 ;; lambda list
                                 (lambda-args+specifiers arg arg-specifier)
                                 (optional-doc doc)
                                 (done))
                                ((head ansi-lisp-keyword-face)
                                 (name ansi-lisp-defined-name-face)
                                 (arg-specifier ansi-lisp-constant-face)
                                 (arg default)
                                 (doc  ansi-lisp-doc-face))
                                :priority 10
                                :names-to-track (arg))

(sexpy-define-pattern-fontifier (defparameter defvar)
                                (parenthesized (atom? head)
                                               (atom? name)
                                               (skip-everything)
                                               (optional-doc doc))
                                ((head ansi-lisp-keyword-face)
                                 (name ansi-lisp-defined-name-face)
                                 (doc  ansi-lisp-doc-face)))

(sexpy-define-pattern-fontifier (defconstant)
                                (parenthesized (atom? head)
                                               (atom? name)
                                               (skip-everything)
                                               (optional-doc doc))
                                ((head ansi-lisp-keyword-face)
                                 (name ansi-lisp-constant-face)
                                 (doc  ansi-lisp-doc-face)))

(sexpy-define-pattern-fontifier (defstruct)
                                (parenthesized
                                 (atom? head)
                                 (alt (atom? structure-name)
                                      (parenthesized
                                       (atom? structure-name)
                                       (repeat
                                        ;; (alt (atom? option-keyword)
                                        ;;      (parenthesized
                                        ;;       (scase
                                        ;;        ((:initial-offset)
                                        ;;         (atom? option-keyword)
                                        ;;         (skip-everything))
                                        ;;        (atom?
                                        ;;         ;; (:conc-name
                                        ;;         ;;  :copier
                                        ;;         ;;  :predicate
                                        ;;         ;;  :print-object
                                        ;;         ;;  :print-function
                                        ;;         ;;  :constructor
                                        ;;         ;;  :include)
                                        ;;         (atom? option-keyword)
                                        ;;         (scase
                                        ;;          (atom? (atom? func-name))
                                        ;;          (t))
                                        ;;         (skip-everything*)))))

                                        (alt (atom? option-keyword)
                                             (parenthesized
                                              (scase
                                               ;; options with 0 or 1 arguments
                                               ;; moreover, this argument is a function
                                               ((:conc-name
                                                 :copier
                                                 :predicate
                                                 :print-object
                                                 :print-function)
                                                (atom? option-keyword)
                                                (scase
                                                 (atom? (atom? func-name))
                                                 (t (skip-everything*))))
                                               ;; options with 1 arguments
                                               ((:initial-offset)
                                                (atom? option-keyword)
                                                (skip-everything))
                                               ((:constructor)
                                                (atom? option-keyword)
                                                (scase
                                                 (atom? (atom? func-name)
                                                        ;; argument list
                                                        (lambda-args constructor-arg))
                                                 (t (skip-everything*))))
                                               ((:include)
                                                (atom? option-keyword)
                                                (atom? structure-name)
                                                (repeat
                                                 (alt (atom? slot-name)
                                                      (parenthesized
                                                       (atom? slot-name)
                                                       (scase (atom?
                                                               ;; skip default value
                                                               (skip-everything)
                                                               (repeat (scase
                                                                        ((:type)
                                                                         (atom? option-keyword)
                                                                         (type slot-type))
                                                                        ((:read-only)
                                                                         (atom? option-keyword)
                                                                         (skip-everything))
                                                                        (t
                                                                         (skip-everything)))))
                                                              (t (skip-everything*)))))))

                                               ;; just heuristic
                                               (atom?
                                                (atom? option-keyword)
                                                (scase
                                                 (atom? (atom? func-name)
                                                        (skip-everything*))
                                                 (t)))))))))
                                 (optional-doc doc)
                                 (repeat
                                  (alt (atom? slot-name)
                                       (parenthesized
                                        (atom? slot-name)
                                        (scase (atom?
                                                ;; skip default value
                                                (skip-everything)
                                                (repeat (scase
                                                         ((:type)
                                                          (atom? option-keyword)
                                                          (type slot-type))
                                                         ((:read-only)
                                                          (atom? option-keyword)
                                                          (skip-everything))
                                                         (t
                                                          (skip-everything)))))
                                               (t (skip-everything*)))))))
                                ((head            ansi-lisp-keyword-face)
                                 (structure-name  ansi-lisp-defined-data-name-face)
                                 (option-keyword  ansi-lisp-keyword-face)
                                 (func-name       ansi-lisp-defined-name-face)
                                 (constructor-arg default)
                                 (doc             ansi-lisp-doc-face)
                                 (slot-name       ansi-lisp-defined-data-name-face)
                                 (slot-type       ansi-lisp-type-face))
                                :priority 10
                                :names-to-track (slot-name))

(sexpy-define-pattern-fontifier (let let* symbol-macrolet)
                                (parenthesized (atom? head)
                                               (parenthesized
                                                (repeat (parenthesized
                                                         (atom? bound-name)
                                                         (skip-everything))))
                                               (done))
                                ((head ansi-lisp-keyword-face)
                                 ;; supress any fontification of names being defined
                                 (bound-name default))
                                :priority 10
                                :names-to-track (bound-name))

(sexpy-define-pattern-fontifier (bind bind*)
                                (parenthesized (atom? head)
                                               (parenthesized
                                                (repeat (parenthesized
                                                         (alt (atom? bound-name)
                                                              (parenthesized
                                                               (repeat (atom? bound-name))))
                                                         (skip-everything))))
                                               (done))
                                ((head ansi-lisp-keyword-face)
                                 ;; supress any fontification of names being defined
                                 (bound-name default))
                                :priority 10
                                :names-to-track (bound-name))

(sexpy-define-pattern-fontifier (multiple-value-bind destructuring-bind)
                                (parenthesized (atom? head)
                                               (tree-atoms bound-name)
                                               (done))
                                ((head ansi-lisp-keyword-face)
                                 ;; supress any fontification of names being defined
                                 (bound-name default))
                                :priority 10
                                :names-to-track (bound-name))


(sexpy-define-pattern-fontifier (flet labels macrolet)
                                (parenthesized (atom? head)
                                               (parenthesized
                                                (repeat (parenthesized
                                                         (atom? bound-name)
                                                         (lambda-args+specifiers arg arg-specifier)
                                                         (skip-everything))))
                                               (done))
                                ((head ansi-lisp-keyword-face)
                                 ;; supress any fontification of names being defined
                                 (bound-name default)
                                 (arg default)
                                 (arg-specifier ansi-lisp-constant-face))
                                :priority 10
                                :names-to-track (bound-name))

(sexpy-define-pattern-fontifier (fn lambda)
                                (parenthesized (atom? head)
                                               (lambda-args+specifiers arg arg-specifier)
                                               (done))
                                ((head ansi-lisp-keyword-face)
                                 (arg default)
                                 (arg-specifier ansi-lisp-constant-face))
                                :priority 10
                                :names-to-track (arg))


(sexpy-define-pattern-fontifier (declare declaim)
                                (parenthesized (atom? head)
                                               (repeat
                                                (declaration-specifier decl-keyword affected-var type)))
                                ((head         ansi-lisp-keyword-face)
                                 (decl-keyword ansi-lisp-declaration-face)
                                 (affected-var default)
                                 (type         ansi-lisp-type-face)))

(sexpy-define-pattern-fontifier (proclaim)
                                (parenthesized (atom? head)
                                               (declaration-specifier decl-keyword affected-var type))
                                ((head         ansi-lisp-keyword-face)
                                 (decl-keyword ansi-lisp-declaration-face)
                                 (affected-var default)
                                 (type         ansi-lisp-type-face)))


(sexpy-define-pattern-fontifier (dolist dotimes)
                                (parenthesized
                                 (atom? head)
                                 (parenthesized
                                  (atom? iteration-variable)
                                  (done)))
                                ((head               ansi-lisp-keyword-face)
                                 (iteration-variable default))
                                :priority 10
                                :names-to-track (iteration-variable))

(sexpy-define-pattern-fontifier (do do*)
                                (parenthesized
                                 (atom? head)
                                 (parenthesized
                                  (repeat
                                   (parenthesized
                                    (atom? iteration-variable)
                                    (skip-everything*))))
                                 (done))
                                ((head               ansi-lisp-keyword-face)
                                 (iteration-variable default))
                                :priority 10
                                :names-to-track (iteration-variable))

(sexpy-define-pattern-fontifier (eval-when)
                                (parenthesized
                                 (atom? head)
                                 (parenthesized
                                  (repeat
                                   (scase
                                    ((compile
                                      load
                                      eval
                                      :compile-toplevel
                                      :load-toplevel
                                      :execute)
                                     (atom? situation)))))
                                 (done))
                                ((head      ansi-lisp-special-form-face)
                                 (situation ansi-lisp-defined-data-name-face)))

(sexpy-define-pattern-fontifier (maximum
                                 minimum
                                 curry
                                 sum
                                 average
                                 div
                                 div-ceil
                                 log2
                                 while
                                 take-while
                                 take
                                 drop
                                 take-by
                                 split-at
                                 split-pred
                                 with-output-file
                                 process-file-linewise
                                 trim-whitespace
                                 mapconcat
                                 array->vector
                                 copy-array
                                 bitwise-and
                                 bitwise-or
                                 bitwise-xor
                                 bitwise-not
                                 bitwise-shift-left
                                 bitwise-shift-right)
                                (parenthesized
                                 (atom? head)
                                 (done))
                                ((head ansi-lisp-keyword-face)))

(sexpy-define-pattern-fontifier (defsetf)
                                (parenthesized
                                 (atom? head)
                                 (atom? access-func)
                                 (scase
                                  (atom? (atom? update-func)
                                         (optional-doc doc))
                                  (list? (lambda-args+specifiers arg arg-specifier)
                                         (parenthesized
                                          (repeat
                                           (atom? store-variable)))
                                         (optional-doc doc)))
                                 (done))
                                ((head           ansi-lisp-keyword-face)
                                 (access-func    ansi-lisp-defined-name-face)
                                 (update-func    ansi-lisp-defined-name-face)
                                 (doc            ansi-lisp-doc-face)
                                 (arg            default)
                                 (arg-specifier  ansi-lisp-constant-face)
                                 (store-variable default)))

;; cond, defsetf



;;;;; tests

;; (defun* match-some-text (pattern text
;;                                  &key
;;                                  (start 1)
;;                                  (repeating nil)
;;                                  (limit nil))
;;   (with-temp-buffer
;;     (erase-buffer)
;;     (insert text)
;;     (goto-char start)
;;     (let ((pat (sexpy-preprocess-pattern pattern)))
;;       (multiple-value-bind (name-num-alist num-pattern)
;;           (sexpy-transform-pattern-to-numbers
;;            pat
;;            (sexpy-pattern-extract-names pat))
;;         (sexpy-match num-pattern
;;                      (sexpy-make-dict-for-pattern pat)
;;                      (or limit (point-max))
;;                      nil)))))

;;;;; end

(provide 'sexpy-highlight)

;; Local Variables:
;; lexical-binding: t
;; End:

;; sexpy-highlight.el ends here
