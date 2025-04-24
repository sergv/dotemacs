;; common.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  5 November 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl)
  (require 'subr-x)
  (require 'set-up-platform)
  (require 'macro-util))

(require 'common-small)
(require 'common-constants)
(require 'common-whitespace)
(require 'macro-util)
(require 'custom-predicates)

(require 'f)
(require 's)
(require 'dash)
(require 'v)

(defconst +undef+ 'common--undef
  "Undefined value not equal to anything but itself and not creatable
by any means other than direct referencing via ‘+undef’.")

(defun random-shuffle (vect)
  "Randoly shuffle vector VECT inplace with supply
of random numbers from RANDOM-GEN."
  (typep vect 'vector)
  (cl-loop
    for i downfrom (1- (length vect)) to 1
    ;; may yield i
    for j = (random (1+ i))
    do (cl-psetf (aref vect i) (aref vect j)
                 (aref vect j) (aref vect i)))
  vect)

(defun shuffle-lines (begin end)
  (interactive "r*")
  (save-excursion
    (goto-char begin)
    (setf begin (line-beginning-position))
    (goto-char end)
    (setf end (line-end-position))
    (let ((lines (list->vector
                  (split-string (buffer-substring-no-properties begin end)
                                "\n"
                                t))))

      (delete-region begin end)
      (when (= (char-after) ?\n)
        (delete-char 1))
      (random-shuffle lines)
      (goto-char begin)
      (dovector (line lines)
        (insert line)
        (insert "\n")))))

;;;; file utilities

(defsubst strip-trailing-slash (x)
  (directory-file-name x))

(defun path-concat (&rest args)
  "Connect paths with standard delimiter"
  (declare (pure t) (side-effect-free error-free))
  (mapconcat #'strip-trailing-slash args "/"))

(defun version-control-directory? (filepath)
  "Test whether FILEPATH contains version control directory as its subpart"
  (declare (pure nil) (side-effect-free t))
  (string-match-p (eval-when-compile
                    (concat "\\(?:/\\|^\\)\\(?:"
                            (regexp-opt +version-control-directories+)
                            "\\)\\(?:/\\|$\\)"))
                  filepath))

(defun common-string-prefix-length (xs ys ignore-case?)
  "Return the length of the common beginning of strings XS and YS.
If there’s none, return 0"
  (let ((common 0)
        (continue t)
        (i 0)
        (end (min (length xs) (length ys)))
        (case-fold-search ignore-case?))
    (while (and continue
                (< i end))
      (if (char-equal (aref xs i) (aref ys i))
          (progn
            (cl-incf i)
            (cl-incf common))
        (setf continue nil)))
    common))

(defun common-string-prefix (xs ys ignore-case?)
  "Return the common beginning of strings XS and YS. If there’s none, return
nil."
  (let ((len (common-string-prefix-length xs ys ignore-case?)))
    (unless (zerop len)
      (substring xs 0 len))))

(defun read-and-insert-filename (&optional abs-path?)
  "Read filename with completion and insert it at point. If ABS-PATH? is true
then insert absolute filepath, otherwise insert one relative to the current
`default-directory'. If `default-directory' is nil then insert absolute anyway."
  (interactive "P")
  (let* ((abs-path
          (expand-file-name
           (read-file-name
            ""
            nil
            ""
            nil
            nil
            (lambda (x) (or (file-directory-p x)
                       (file-exists-p x))))))
         (def-dir (when default-directory
                    (expand-file-name default-directory)))
         (path (if (and def-dir
                        (not abs-path?)
                        ;; If there’s no common prefix then no point going after relative
                        ;; path.
                        (let ((len (common-string-prefix-length abs-path
                                                                def-dir
                                                                (fold-platform-os-type nil t))))
                          ;; If match is longer than either "/" or "C:/"
                          (< (fold-platform-os-type 1 3) len)))
                   (file-relative-name abs-path def-dir)
                 abs-path))
         (output (if (and (eq major-mode 'org-mode)
                          (y-or-n-p "Insert link? "))
                     (concat "[[file:"
                             path
                             "][]]")
                   path)))
    (insert output)))

;;;

(defun read-string-no-default (prompt
                               &optional
                               initial-input
                               hist-m@%=!$+&^*z
                               default-value
                               inherit-input-method)
  "Similar to `read-string' but never includes
default into prompt."
  (read-string prompt
               initial-input
               hist-m@%=!$+&^*z
               default-value
               inherit-input-method))

(defun emacs-forget-buffer-process ()
  "Emacs will not query about this process when killing."
  (let ((p (get-buffer-process (current-buffer))))
    (when p
      (set-process-query-on-exit-flag p nil))))

(defsubst re-group-matchedp (n)
  "Return non-nil if Nth group matched."
  (declare (pure nil) (side-effect-free t))
  (match-beginning n))

(defalias 're-group-matched? #'re-group-matchedp)


(defun constantly (x)
  (declare (pure t) (side-effect-free t))
  (let ((tmp x))
    (lambda (&rest _y)
      tmp)))

;;; combinatorics

(defun factorial (x)
  (declare (pure t) (side-effect-free t))
  (if (= 0 x)
      1
    (* x (factorial (1- x)))))

(defun choose (n k)
  "Compute binomial coefficient."
  (declare (pure t) (side-effect-free t))
  (/ (factorial n) (* (factorial k)
                      (factorial (- n k)))))

(defun permutations (list)
  (declare (pure t) (side-effect-free t))
  (if (null list)
      (list nil)
    (mapcan (lambda (item)
              (mapcar (lambda (x)
                        (cons item x))
                      (permutations (remove item list))))
            list)))


(defun combinations (n k)
  "Construct all
 / n \\
 \\ k /
combinations"
  (declare (pure t) (side-effect-free t))
  (letrec ((collect
            (lambda (start end)
              (if (< start 0)
                  (list ())
                (cl-loop
                 for i from start to end
                 nconcing
                 (mapcar (lambda (rest)
                           (cons i rest))
                         (funcall collect (1- start) (1- i))))))))
    (funcall collect (1- k) (1- n))))

;;;

(defsubst parse-partial-sexp--inside-comment? (state)
  (elt state 4))

(defsubst parse-partial-sexp--comment-or-string-start (state)
  (elt state 8))

(defun point-inside-string? (&optional pos)
  "Return non-nil if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (save-excursion
    (let ((state (syntax-ppss (or pos (point)))))
      (elt state 3))))

(defun ppss-point-inside-comment? (&optional pos)
  "Return non-nil if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (save-excursion
    (let ((state (syntax-ppss (or pos (point)))))
      (parse-partial-sexp--inside-comment? state))))

(defun point-inside-comment? (&optional pos)
  "Return non-nil if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (save-excursion
    (let ((state (syntax-ppss (or pos (point)))))
      (elt state 4))))

(defun point-inside-string-or-comment? (&optional pos)
  "Return t if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (save-excursion
    (let ((state (syntax-ppss (or pos (point)))))
      (or (elt state 3)
          (elt state 4)))))

(defsubst point-not-inside-string-or-comment? (&optional pos)
  (declare (pure nil) (side-effect-free t))
  (not (point-inside-string-or-comment? pos)))

;;;

(defsubst assoc-value (key alist)
  (declare (pure t) (side-effect-free t))
  (cadr (assoc key alist)))

(defun keyword-arglist-get-value (key keyword-arglist &optional default)
  "Get value of keyword argument named KEY from KEYWORD-ARGLIST, with
structure like this (:arg1 value1 :arg2 value2 ... :argN valueN)"
  (declare (pure t) (side-effect-free t))
  (cond
    ((null keyword-arglist)
     default)
    ((eq? key (car keyword-arglist))
     (cadr keyword-arglist))
    (t
     (keyword-arglist-get-value key
                                (cddr keyword-arglist)
                                default))))

;;;

(defun invisible-buffer? (buf)
  "Returns t if buffer BUF should be regarded as invisible, see also
`*invisible-buffers*'."
  (declare (pure nil) (side-effect-free t))
  (let* ((name (cond
                 ((stringp buf)
                  buf)
                 ((bufferp buf)
                  (buffer-name buf))
                 (t
                  (error "wrong argument type - neither a string or a buffer: %s"
                         buf))))
         (len (length name)))
    (or (member name '("*Kill Ring*" "*Pp Eval Output*" "*buflist*" "*Async Shell Command*" "*Completions*" "*Ibuffer*" "*magit-process*" "*Help*"))
        (zerop len)
        (let ((first-char (aref name 0)))
          (or (eq first-char ?\s)
              (and (eq first-char ?#)
                   (> len 2)
                   (let ((last-char (aref name (1- len))))
                     (eq last-char ?#))))))))

(defun visible-buffers ()
  "Get list of buffers that are not invisible."
  (declare (pure nil) (side-effect-free t))
  (--filter (not (invisible-buffer? it)) (buffer-list)))

(defvar invisible-buffers-re
  (rx bol
      (or (or "*Kill Ring*"
              "*Pp Eval Output*"
              "*buflist*"
              "*Async Shell Command*"
              "*Completions*"
              "*Ibuffer*"
              "*magit-process*"
              "*Help*")
          (seq "#" (+ anything) "#")
          (seq " " (* anything)))
      eol))

;;;

(defun for-buffers-with-mode (mode func)
  (dolist (buf (buffer-list))
    (when (with-current-buffer buf
            (eq? major-mode mode))
      (funcall func buf))))

;;;

(defun hash-table-from-list (mk-key mk-value xs &optional cmp)
  (declare (pure t) (side-effect-free t))
  (let ((table (make-hash-table :test (or cmp #'equal))))
    (dolist (item xs)
      (puthash (funcall mk-key item) (funcall mk-value item) table))
    table))

(defun alist->hash-table (alist &optional cmp)
  "Translate alist of (<key> . <value>) pairs into hash-table."
  (declare (pure t) (side-effect-free t))
  (let ((table (make-hash-table :test (or cmp #'equal))))
    (dolist (item alist)
      (puthash (car item) (cdr item) table))
    table))

(defun alist->hash-table-with (comb alist &optional cmp)
  "Translate alist of (<key> . <value>) pairs into hash-table."
  (declare (pure t) (side-effect-free t))
  (let ((table (make-hash-table :test (or cmp #'equal))))
    (dolist (item alist)
      (let ((key (car item)))
        (puthash key (funcall comb (cdr item) (gethash key table)) table)))
    table))

(defun hash-table->alist (table)
  "Translate hash table into alist of (<key> . <value>) pairs."
  (declare (pure t) (side-effect-free t))
  (let ((result '()))
    (maphash (lambda (k v) (push (cons k v) result)) table)
    result))

(defun hash-table-keys (table)
  "Get list of keys of hash table."
  (declare (pure t) (side-effect-free t))
  (let ((result '()))
    (maphash (lambda (k _v) (push k result)) table)
    result))

(defun hash-table-values (table)
  "Get list of values of hash table."
  (declare (pure t) (side-effect-free t))
  (let ((result '()))
    (maphash (lambda (_k v) (push v result)) table)
    result))

(defun hash-table-keys-filter (pred table)
  "Get list of keys of hash table for entries that match PRED, i.e. PRED
returns true for key and value."
  (declare (pure t) (side-effect-free t))
  (let ((result '()))
    (maphash (lambda (k v) (when (funcall pred k v) (push k result))) table)
    result))

(defun hash-table-entries-matching-re (table re)
  "Return list of TABLE (key . value) pairs where keys match RE."
  (declare (pure t) (side-effect-free t))
  (let ((result nil))
    (maphash (lambda (k v)
               (when (string-match-p re k)
                 (push (cons k v) result)))
             table)
    result))

(defun hash-table-merge (table-main table-aux)
  "Add all entries from TABLE-AUX into TABLE-MAIN."
  (declare (pure nil) (side-effect-free nil))
  (let ((new (copy-hash-table table-main)))
    (maphash (lambda (k v)
               (puthash k v new))
             table-aux)
    new))

(defun hash-table-merge! (table-main table-aux)
  "Add all entries from TABLE-AUX into TABLE-MAIN."
  (declare (pure nil) (side-effect-free nil))
  (maphash (lambda (k v)
             (puthash k v table-main))
           table-aux))

(defun hash-table-merge-with! (comb-func table-main table-aux)
  "Add all entries from TABLE-AUX into TABLE-MAIN, combine entries present
in both tables with COMB-FUNC, which should take 3 arguments: key, value in
main table and value in aux table."
  (declare (pure nil) (side-effect-free nil))
  (maphash (lambda (k v)
             (if-let (v-main (gethash k table-main))
                 (puthash k (funcall comb-func k v-main v) table-main)
               (puthash k v table-main)))
           table-aux))

(defsubst hash-table-member-p (key table)
  (declare (pure t) (side-effect-free t))
  (let ((value-missing '#:value-missing))
    (not
     (eq value-missing
         (gethash key table value-missing)))))

;;;

(defun string-suffix? (string1 string2 &optional ignore-case)
  "Return t if STRING1 is a suffix of STRING2."
  (declare (pure t) (side-effect-free t))
  (and (<= (length string1) (length string2))
       (eq t (compare-strings string1 0 nil
                              string2 (- (length string2) (length string1)) nil
                              ignore-case))))

(cl-defun strip-string-prefix (prefix str &key (starting-at 0))
  "Remove (+ (length PREFIX) STARTING-AT) characters from start of STR."
  (declare (pure t) (side-effect-free t))
  (substring str (+ starting-at (length prefix))))

;;;

(defsubst file-modification-time (filename)
  "Return latest modification time of file FILENAME."
  (declare (pure nil) (side-effect-free t))
  (file-attribute-modification-time (file-attributes filename 'integer)))

(defsubst file-size (filename)
  "Return size of file FILENAME."
  (declare (pure nil) (side-effect-free t))
  (file-attribute-size (file-attributes filename 'integer)))

(if (executable-find "cmp")
    (defun different-files-fast? (file1 file2)
      "Return t if content of FILE1 and FILE2 differs and try to yield answer
faster than byte-by-byte comparison of respective file contents."
      (declare (pure nil) (side-effect-free t))
      (if (= (file-size file1) (file-size file2))
          (= 1 (call-process "cmp" nil nil nil file1 file2))
        t))
  (defun different-files-fast? (file1 file2)
    "Return t if content of FILE1 and FILE2 differs and try to yield answer
faster than byte-by-byte comparison of respecfive file contents."
    (declare (pure nil) (side-effect-free t))
    (if (= (file-size file1) (file-size file2))
        (string= (with-temp-buffer
                   (insert-file-contents file1)
                   (buffer-substring-no-properties (point-min) (point-max)))
                 (with-temp-buffer
                   (insert-file-contents file2)
                   (buffer-substring-no-properties (point-min) (point-max))))
      t)))

;;;

(defun to-linux-line-endings ()
  "Convert line endings in current buffer to linux ones (\\n)."
  (interactive "*")
  (set-buffer-file-coding-system 'utf-8-unix nil))

(defun to-windows-line-endings ()
  "Convert line endings in current buffer to windows ones (\\r\\n)."
  (interactive "*")
  (set-buffer-file-coding-system 'utf-8-dos nil))

;;;

(cl-defun map-files (f file-list &key dont-write)
  "For every file in FILE-LIST insert it's content into
buffer, call function f with that buffer as argument and
write buffer contents back into file if flag DONT-WRITE is nil."
  (dolist (file file-list)
    (with-temp-buffer
      (insert-file-contents file)
      (funcall f (current-buffer))
      (unless dont-write
        (write-region (point-min) (point-max) file)))))

;;;

(defun join-lines (lines &optional str)
  "Join list of strings with given STR that defaults to newline."
  (declare (pure t) (side-effect-free t))
  (mapconcat #'identity lines (or str "\n")))

(defun split-into-lines (str &optional keep-nulls)
  "Split string into list of lines."
  (declare (pure t) (side-effect-free t))
  (split-string str "\n" (not keep-nulls)))

(defsubst foldr (f init items)
  "F should take two arguments (item accum)."
  (cl-reduce f
             items
             :from-end t
             :initial-value init))

(defsubst foldl (f init items)
  "F should take two arguments (accum item)."
  (cl-reduce f
             items
             :initial-value init))

;;;

(defun generic/length (item)
  (declare (pure t) (side-effect-free t))
  (cond ((ring? item)
         (ring-length item))
        ((or (listp item)
             (vector? item)
             (stringp item))
         (length item))
        (t
         (error "Cannot determine generic length of item %s" item))))

(defun generic/member (item sequence)
  (declare (pure t) (side-effect-free t))
  (cond ((ring? sequence)
         (ring-member sequence item))
        ((listp sequence)
         (member item sequence))
        (t
         (error "Cannot determine generic membership of item %s in sequence %s"
                item
                sequence))))

;;;

(defun list< (a b)
  "Check whether list of integers A is lexicographically lesser than
integer list B."
  (declare (pure t) (side-effect-free t))
  (cond
    ((and a
          (not b))
     nil)
    ((and (not a)
          b)
     t)
    (t
     (let ((continue t)
           (result nil))
       (while (and continue
                   a
                   b)
         (let ((elem-a (car a))
               (elem-b (car b)))
           (cond ((< elem-a elem-b)
                  (setf continue nil
                        result t))
                 ((> elem-a elem-b)
                  (setf continue nil
                        result nil))
                 (t
                  (setf a (cdr a)
                        b (cdr b))))))
       result))))

(defun string-list< (a b)
  "Check whether list of strings A is lexicographically lesser than
strings list B."
  (declare (pure t) (side-effect-free t))
  (cond
    ((and a
          (not b))
     nil)
    ((and (not a)
          b)
     t)
    (t
     (let ((continue t)
           (result nil))
       (while (and continue
                   a
                   b)
         (let ((elem-a (car a))
               (elem-b (car b)))
           (cond ((string< elem-a elem-b)
                  (setf continue nil
                        result t))
                 ((string> elem-a elem-b)
                  (setf continue nil
                        result nil))
                 (t
                  (setf a (cdr a)
                        b (cdr b))))))
       result))))

(defsubst list= (a b)
  "Check whether list of integers A is equal to integer list B."
  (declare (pure t) (side-effect-free t))
  (equal a b))

;;;

(defun text-between-lines (start-line end-line)
  "Return string of text with properties between beginning of START-LINE and
end of END-LINE in current buffer."
  (declare (pure nil) (side-effect-free t))
  (cl-assert (< start-line end-line))
  (save-excursion
    (buffer-substring (progn (goto-line-dumb start-line)
                             (line-beginning-position))
                      (progn (goto-line-dumb end-line)
                             (line-end-position)))))

;;;

(setf completion-ignored-extensions
      (eval-when-compile
        (append (mapcar (lambda (x) (concat x "/")) +ignored-directories+)
                +ignored-file-extensions+))
      grep-find-ignored-files
      (eval-when-compile
        (append (list ".#*"
                      "*.exe"
                      "*.prof")
                (mapcar (lambda (x)
                          (concat "*" x))
                        +ignored-file-extensions+))))

;;;

(defun insert-current-date ()
  "Insert today's date as \"<Day Name>, <day> <Month name> <Year>\""
  (interactive "*")
  (insert (format-time-string "%A, %e %B %Y")))

;;;

;; may be useful someday
;; (defun search-property-change-with-cycling (direction start-pos prop prop-predicate)
;;   "DIRECTION should be either 'forward or 'backward
;; PROP-PREDICATE - predicate over property value"
;;   (let ((p (min (point-max)
;;                 (max (point-min)
;;                      (+ start-pos (if (eq? direction 'forward) +1 -1)))))
;;         (done nil))
;;     (while (not done)
;;       (setf p (funcall (if (eq? direction 'forward)
;;                          #'next-single-property-change
;;                          #'previous-single-property-change)
;;                        p
;;                        prop
;;                        (current-buffer)
;;                        (if (eq? direction 'forward)
;;                          (if (< p start-pos)
;;                            start-pos
;;                            nil)
;;                          (if (< p start-pos)
;;                            nil
;;                            start-pos))))
;;       (when (and p
;;                  (= p start-pos)
;;                  (not (= (point-min) start-pos))
;;                  (not (= (point-max) start-pos)))
;;         (error "Property %s satisfying predicate %s not found"
;;                prop
;;                prop-predicate))
;;       (cond
;;         ((and (eq? direction 'backward)
;;               (or (not p)
;;                   (<= p (point-min))))
;;          (setf p (point-max)))
;;         ((and (eq? direction 'forward)
;;               (or (not p)
;;                   (>= p (point-max))))
;;          (setf p (point-min)))
;;         (t
;;          (setf done (funcall prop-predicate (get-text-property
;;                                              p
;;                                              prop))))))
;;     p))

;;;

(defvar common/registered-filenames (make-hash-table :test #'equal)
  "Hashtable binding filename strings to themselves. Exists for memory
optimization purposes.")

(defun common/registered-filename (filename)
  (declare (pure nil) (side-effect-free t))
  (aif (gethash filename common/registered-filenames)
      it
    (progn
      (puthash filename filename common/registered-filenames)
      filename)))

;;;

(cl-defun pp-to-string* (obj &key (length nil) (depth nil))
  (let ((print-length length)
        (print-level depth))
    (pp-to-string obj)))

;;;

(defun get-region-string-no-properties ()
  "Get string currently selected by a region, or nil
if there's no region."
  (declare (pure nil) (side-effect-free t))
  (if (region-active-p)
      (buffer-substring-no-properties
       (region-beginning)
       (region-end))
    (error "Region not active")))

;;;

(defun find-first-matching (f xs)
  "Find first x among XS such that (F x) is non-nil, and
return pair (x (F x))."
  (cl-assert (listp xs))
  ;; Imperative and ugly but efficient.
  (let ((done nil)
        (y nil)
        (fy nil))
    (while (and (not done) xs)
      (let ((x (car-sure xs)))
        (when-let (tmp (funcall f x))
          (setf done t
                y x
                fy tmp)))
      (setf xs (cdr-sure xs)))
    (if done
        (values y fy)
      nil)))

;;;

(defun normalise-file-name (fname)
  "Normalize file name"
  (declare (pure t) (side-effect-free t))
  (let ((cmdline-normalized
         (command-line-normalize-file-name fname)))
    (strip-trailing-slash
     (fold-platform-os-type
      cmdline-normalized
      (replace-regexp-in-string "[\\]+" "/" cmdline-normalized)))))

;;; buffer, window and frame utils

(defun next-buffer (n)
  "Go to the buffer which is at the end of buffer list."
  (interactive "p")
  (dotimes (_ n)
    (unbury-buffer)))

(defun prev-buffer (n)
  "Go to the buffer which is at the top of buffer list behind
the current buffer."
  (interactive "p")
  (dotimes (_ n)
    (bury-buffer (current-buffer))
    (switch-to-buffer (other-buffer (current-buffer))))) ;dont forget about 0 here (??)


(defsubst next-w (n)
  "Go to next Nth window"
  (interactive "p")
  (other-window n))

(defsubst prev-w (n)
  "Go to previous Nth window"
  (interactive "p")
  (other-window (- n)))

(defun swap-buffers-in-windows (win-a win-b)
  "Swap buffers in windows WIN-A and WIN-B."
  (let* ((buf-a (window-buffer win-a))
         (pos-a (with-selected-window win-a
                  (with-current-buffer buf-a
                    (point))))
         (buf-b (window-buffer win-b))
         (pos-b (with-selected-window win-b
                  (with-current-buffer buf-b
                    (point)))))
    (if (eq buf-b buf-a)
        (progn
          (with-selected-window win-a
            (with-current-buffer buf-a
              (goto-char pos-b)))
          (select-window win-b)
          (with-selected-window win-b
            (with-current-buffer buf-b
              (goto-char pos-a))))
      (let ((dedicated-a? (window-dedicated-p win-a))
            (dedicated-b? (window-dedicated-p win-b)))
        (set-window-dedicated-p win-a nil)
        (set-window-dedicated-p win-b nil)
        (switch-to-buffer buf-b)
        (select-window win-b)
        (switch-to-buffer buf-a)
        (set-window-dedicated-p win-a dedicated-b?)
        (set-window-dedicated-p win-b dedicated-a?)))))

(defun call-n (n f x)
  "Call function F multiple times (N repetitions) feeding its output to itself. Initial input is X."
  (cl-assert (<= 0 n))
  (while (< 0 n)
    (setf x (funcall f x)
          n (- n 1)))
  x)

(defun swap-buffers-forward (&optional n)
  "Swap current buffer with next buffer"
  (interactive "p")
  (let* ((curr-win (selected-window))
         (next-win (call-n n
                            (lambda (win) (next-window win 0))
                            curr-win)))
    (if (eq curr-win next-win)
        (error "Not swapping window with itself")
      (swap-buffers-in-windows curr-win next-win))))

(defun swap-buffers-backward (&optional n)
  "Swap current buffer with previous buffer"
  (interactive "p")
  (let* ((curr-win (selected-window))
         (prev-win (call-n n
                            (lambda (win) (previous-window win 0))
                            curr-win)))
    (if (eq curr-win prev-win)
        (error "Not swapping window with itself")
      (swap-buffers-in-windows curr-win prev-win))))

(defun swap-buffers-forward-through-frames ()
  "Swap current buffer with selected buffer in the next frame."
  (interactive)
  (let* ((curr-win (selected-window))
         (next-frame (next-frame nil
                                 nil ;; exclude minibuffer-only frames
                                 ))
         (next-win (with-selected-frame next-frame
                     (selected-window))))
    (swap-buffers-in-windows curr-win next-win)
    (select-frame-set-input-focus next-frame)))

(defun swap-buffers-backward-through-frames ()
  "Swap current buffer with selected buffer in the previous frame."
  (interactive)
  (let* ((curr-win (selected-window))
         (prev-frame (previous-frame nil
                                     nil ;; exclude minibuffer-only frames
                                     ))
         (prev-win (with-selected-frame prev-frame
                     (selected-window))))
    (swap-buffers-in-windows curr-win prev-win)
    (select-frame-set-input-focus prev-frame)))

(defun next-f (n)
  "Switch to Nth previous frame."
  (interactive "p")
  (other-frame n))

(defun prev-f (n)
  "Switch to Nth previous frame."
  (interactive "p")
  (other-frame (- n)))


(defalias 'run-file-manager 'start-file-manager)

(defalias 'run-terminal-emulator 'start-terminal-emulator)
(defalias 'run-terminal 'start-terminal-emulator)

(defun gc-stats ()
  "Do garbage collection and pretty-print results for use in modeline."
  (let* ((stats (garbage-collect))
         (to-mb (lambda (x) (when x (/ x (* 1024 1024)))))
         (entry->mb (lambda (entry)
                      (when entry
                        (let ((size (second entry))
                              (used (third entry)))
                          (funcall to-mb
                                   (* size used))))))
         ;; (extract-used (lambda (x) (car-safe (cdr-safe (cdr-safe x)))))
         (mbytes-used (-sum (mapcar entry->mb stats))))
    (format "[%sMb/cons %sMb/vec %sMb/heap %sMb]"
            mbytes-used
            (funcall entry->mb (assoc 'conses stats))
            (funcall entry->mb (assoc 'vector-slots stats))
            (funcall entry->mb (assoc 'heap stats)))))

;;;;

(defun fontify-merge-markers ()
  (declare (pure nil) (side-effect-free nil))
  (font-lock-add-keywords
   nil
   '(("^<<<<<<< .*$" 0 'font-lock-warning-face t)
     ("^|||||||$" 0 'font-lock-warning-face t) ; "diff3" style
     ("^=======$" 0 'font-lock-warning-face t)
     ("^>>>>>>> .*$" 0 'font-lock-warning-face t))))

(defun kill-new-ignoring-duplicates (text)
  "Similar to `kill-new', but does not append TEXT to `kill-ring' if
topmost `kill-ring' item is equal to text (text properties are ignored)."
  (when (or (null kill-ring)
            (not (string= text (car kill-ring))))
    (kill-new text nil)))

;;;;

(defun save-buffer-if-modified ()
  (when-buffer-has-file
    (when (buffer-modified-p)
      (save-buffer))))

;;;;

(defun character-column-at-pos (pos)
  (declare (pure nil) (side-effect-free nil))
  (cl-assert (number-or-marker-p pos))
  (save-excursion
    (goto-char pos)
    (- pos (line-beginning-position))))

(defun current-character-column ()
  "Return current column in number of characters since the beginning of line.
Does well against `prettify-symbols-mode' and `compose-region'."
  (declare (pure nil) (side-effect-free nil))
  (- (point) (line-beginning-position)))

(defun move-to-character-column (col)
  "Move point to column COL."
  (declare (pure nil) (side-effect-free nil))
  (goto-char (min (+ (line-beginning-position) col) (line-end-position))))

(defun remove-buffer (&optional hard-kill? buffer-or-name)
  "Remove buffer completely bypassing all its prompt functions.
Save buffer if it has assigned file and this file exists on disk."
  (interactive "P")
  (let ((kill-buffer-query-functions nil)
        ;; Fool Emacs into thinking that buffer has no corresponding file
        ;; so that it won’t query to save it.
        (buffer-file-name (if hard-kill?
                              nil
                            buffer-file-name)))
    (when-buffer-has-file
      (when (file-exists? buffer-file-name)
        (save-buffer)))
    (kill-buffer buffer-or-name)))

(defun remove-buffer-and-window ()
  "Remove buffer and close it's window"
  (interactive)
  (remove-buffer)
  (delete-window))

(defun make-file-executable (file-name)
  "Make file FILE-NAME executable by adding all the executable bits to it's mode."
  (set-file-modes file-name (logior #o111 (file-modes file-name))))

(defun make-script-file-exec ()
  "Make buffer file executable if it's a shell script."
  (unless (file-executable-p buffer-file-name)
    (when (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              ;; first alternative - unix shell shebang
              ;; second alternative - emacs "shebang"
              (looking-at-p "^\\(?:#!\\|:;[ \t]*exec\\)")))
      (make-file-executable buffer-file-name)
      (unless (file-executable-p buffer-file-name)
        (shell-command (concat "chmod u+x \"" buffer-file-name "\"")))
      (message "Saved as script: %s" buffer-file-name))))

(defun count-lines-fixed (begin end)
  "Return line count in region like `count-lines' but don't
confuse when point is not at the beginning of line"
  (save-restriction
    (narrow-to-region begin end)
    (line-number-at-pos (point-max))))

(defun reindent-region (start end)
  "custom function that reindents region, differs from indent-region
 with silent behavior( i.e. no messages)"
  (save-excursion
    (let ((lnum 0)
          (lines (count-lines-fixed start end)))
      (goto-char start)
      (while (< lnum lines)
        (cl-incf lnum)
        (indent-for-tab-command)
        (forward-line 1)))))

(defun yank-and-reindent ()
  "Function pastes most recently yanked or killed text
ant reindents it."
  (interactive "*")
  (yank)
  (reindent-region (region-beginning) (region-end))
  (goto-char (region-end)))

(defun yank-previous ()
  (interactive "*")
  (yank-pop))

(defun yank-next ()
  (interactive "*")
  (yank-pop 1))

(defun yank-previous-and-reindent ()
  (interactive "*")
  (yank-previous)
  (reindent-region (region-beginning) (region-end))
  (goto-char (region-end)))

(defun yank-next-and-reindent ()
  (interactive "*")
  (yank-next)
  (reindent-region (region-beginning) (region-end))
  (goto-char (region-end)))


(defun delete-word (count)
  "Delete characters forward until encountering the end of a word.
With argument COUNT, do this that many times."
  (interactive "*p")
  (delete-region (point)
                 (progn
                   ;; (pseudovim-motion-fwd-word count)
                   (forward-word count)
                   (point))))

(defun delete-word* (count)
  "Delete characters backard until encountering the end of a word.
With argument COUNT, do this that many times."
  (interactive "*p")
  (delete-region (point)
                 (progn
                   (pseudovim-motion-fwd-WORD count)
                   (point))))

(defun backward-delete-word (count)
  "Delete characters backward until encountering the beginning of a word.
With argument COUNT, do this that many times."
  (interactive "*p")
  (delete-region (point)
                 (progn
                   ;; (pseudovim-motion-bwd-word count)
                   (backward-word count)
                   (point))))

(defun backward-delete-word* (count)
  "Delete characters backward until encountering the beginning of a word.
With argument COUNT, do this that many times."
  (interactive "*p")
  (delete-region (point)
                 (progn
                   (pseudovim-motion-bwd-WORD count)
                   (point))))

(defun delete-current-line ()
  "Delete line where point is currently positioned including
trailing newline"
  (beginning-of-line)
  (let ((start (point)))
    (while (and (not (eobp))
                (not (char= ?\n (char-after))))
      (forward-char 1))
    (unless (eobp)
      (forward-char 1))
    (delete-region start (point))))

(defsubst current-line ()
  "Return line point is currently on."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defsubst current-line-with-properties ()
  "Return line point is currently on."
  (buffer-substring (line-beginning-position)
                    (line-end-position)))

(defsubst skip-indentation-forward (&optional end-pos)
  "Skip whitespace that looks like indentation, but don't go
further that END-POS.

Returns the number of characters skipped."
  ;; (skip-chars-forward "[:blank:" end-pos)
  (skip-chars-forward " \t" end-pos))

(defsubst skip-to-indentation (&optional end-pos)
  "Move point to first non-whitespace character of line, but no
further than END-POS.

Return indentation size in number of characters (i.e. tabs count as 1)."
  (beginning-of-line)
  (skip-indentation-forward end-pos))

(defun indentation-size ()
  "Return indentation size for current line without moving point."
  (save-excursion
    (skip-to-indentation)
    (current-column)))

(defun current-line-indentation-str (&optional end-pos)
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (skip-indentation-forward end-pos)
      (let ((end (point)))
        (buffer-substring-no-properties start end)))))

(defun backward-line (&optional count)
  "Call `forward-line' in the opposite direction"
  (interactive)
  (forward-line (- (or count 1))))


(defsubst goto-line-dumb (line)
  "Set point at the beginning of line LINE counting from line 1 at
beginning of buffer. Does not cause \"Scan error: \"Unbalanced parentheses\"\" as
`goto-line' does."
  (goto-char (point-min))
  (forward-line (1- line)))

;;; rotate list functions, very old...

(defun rotate-entry-list (listvar)
  "Rotate list of any etries such that list '(X Y Z) becomes '(Y Z X)"
  (set listvar (let ((value (symbol-value listvar)))
                 (cond ((null value) nil)
                       ((null (cdr value)) value)
                       (t (let ((new-list (cdr value)))
                            (setcdr value nil)
                            (nconc new-list value)
                            new-list))))))

(defun rotate-entry-list-backward (listvar)
  "Rotate list of any etries such that list '(X Y Z) becomes '(Z X Y)"
  (set listvar (let ((value (symbol-value listvar)))
                 (cond ((null value) nil)
                       ((null (cdr value)) value)
                       (t (while (cddr value)
                            (setq value (cdr value)))
                          (let ((last-elem (cdr value)))
                            (setcdr last-elem (symbol-value listvar))
                            (setcdr value nil)
                            last-elem))))))

;;;;

(defvar-local inhibit-delete-trailing-whitespace nil
  "Whether function `delete-trailing-whitespace+' should do actual deletion.")

(defun toggle-delete-trailing-whitespace ()
  "Toggle `inhibit-delete-trailing-whitespace' option."
  (interactive)
  (if (setf inhibit-delete-trailing-whitespace
            (not inhibit-delete-trailing-whitespace))
      (message "Trailing whitespace will not be removed on save")
    (message "Trailing whitespace will be removed on save")))

(defun inhibit-delete-trailing-whitespace? ()
  "Function that says whether trailing whitespace should be deleted for current
buffer."
  (or inhibit-delete-trailing-whitespace
      (eq major-mode 'diff-mode)))

(defun delete-trailing-whitespace+ ()
  "This function removes spaces and tabs on every line after
last non-whitespace character."
  (unless (inhibit-delete-trailing-whitespace?)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (not (eobp))
          (let ((p (line-end-position)))
            (goto-char p)
            (unless (zerop (skip-chars-backward " \t"))
              (delete-region (point) p))
            (forward-line 1)))))))

;;;;

(defun mk-regexp-from-alts (alts)
  (declare (pure t) (side-effect-free t))
  (cl-assert (listp alts) nil "Invalid regexp alternatives: %s" alts)
  (when alts
    (mapconcat (lambda (x) (concat "\\(?:" x "\\)"))
               alts
               "\\|")))

(defun globs-to-regexp (globs)
  (declare (pure t) (side-effect-free t))
  (mk-regexp-from-alts (mapcar #'wildcard-to-regexp globs)))

(defun ci-looking-at (regexp)
  "Case-insensetive version of `looking-at'."
  (declare (pure nil) (side-effect-free nil))
  (let ((case-fold-search t))
    (looking-at regexp)))

(defun ci-looking-at-p (regexp)
  "Case-insensetive version of `looking-at-p'."
  (declare (pure nil) (side-effect-free t))
  (let ((case-fold-search t))
    (looking-at-p regexp)))

(defun buffer-visible-p (buf)
  (declare (pure nil) (side-effect-free t))
  (not (null (get-buffer-window buf t))))

;;;;

(defun preceded-by (char &optional pos)
  "Check if character before position POS (or current position if omitted) is
equal to CHAR."
  (let ((before (char-before pos)))
    (and before
         (eq before char))))

(defun preceded-by2 (char1 char2 &optional pos)
  "Check if two characters before position POS (or current position if omitted)
are CHAR1 and CHAR2 repsectively."
  (let ((before1 (char-before pos))
        (before2 (char-before (1- (or pos (point))))))
    (and before1
         before2
         (eq before1 char1)
         (eq before2 char2))))

(defsubst is-uppercase? (c)
  (declare (pure t) (side-effect-free t))
  (let ((case-fold-search nil))
    (char-equal c (upcase c))))

(defsubst is-lowercase? (c)
  (declare (pure t) (side-effect-free t))
  (let ((case-fold-search nil))
    (char-equal c (downcase c))))

(defun ci-re-for-literal (str)
  "Make regexp that case-insensitively matches STR, eg for \"foo\" produce \"[Ff][Oo][Oo]\"."
  (with-temp-buffer
    (dovector (c str)
      (insert "[" (upcase c) (downcase c) "]"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun notify (&rest args)
  "Like `message' but is quiet in noninteractive mode."
  (unless noninteractive
    (apply #'message args)))

(defun extended-max (a b)
  (if a
      (if b
          (max a b)
        a)
    (if b
        b
      nil)))

(defun extended-min (a b)
  (if a
      (if b
          (min a b)
        a)
    (if b
        b
      nil)))

(defun extended< (a b)
  "Operator `<' extended to numbers that may be nil."
  (if a
      (if b
          (< a b)
        nil ;; !(~nil < nil)
        )
    (if b
        t ;; nil < ~nil
      nil ;; !(nil < nil)
      )))

(defun extended<= (a b)
  "Operator `<=' extended to numbers that may be nil."
  (if a
      (if b
          (<= a b)
        nil ;; !(~nil <= nil)
        )
    t ;; either 'nil <= ~nil' or 'nil = nil'
    ))

(defun scroll-down-command-fast ()
  (interactive)
  (let ((completed nil))
    (unwind-protect
        (progn
          ;; Silence end of buffer message.
          (let ((noninteractive t))
            (scroll-down 500))
          (setf completed t))
      (unless completed
        (goto-char (point-min))))))

(defun scroll-up-command-fast ()
  (interactive)
  (let ((completed nil))
    (unwind-protect
        (progn
          ;; Silence beginning of buffer message.
          (let ((noninteractive t))
            (scroll-up 500))
          (setf completed t))
      (unless completed
        (goto-char (point-max))))))

;;

(defun-caching cached-executable-find (exe) cached-executable-find--reset-cache exe
  (executable-find exe))

;;

(defun expand-escape-sequences (str)
  (expand-escape-sequences! (copy-sequence str)))

(defun expand-escape-sequences! (str)
  "Expand escape sequences within STR:

\\\\ -> <backspace>
\\n  -> <newline>
\\r  -> <carriage-return>
\\t  -> <tab>

All other backslashes are preserved as is."
  (cl-assert (stringp str))
  (let ((limit (length str))
        (i 0)
        (r 0))
    (while (< i limit)
      (let ((c (aref str i))
            (j (+ i 1)))
        (if (and (char= c ?\\)
                 (< j limit))
            (pcase (aref str j)
              (`?n  (setf (aref str r) ?\n
                          i (+ i 2)))
              (`?r  (setf (aref str r) ?\r
                          i (+ i 2)))
              (`?t  (setf (aref str r) ?\t
                          i (+ i 2)))
              (`?\\ (setf (aref str r) ?\\
                          i (+ i 2)))
              (_    (setf (aref str r) c
                          i j)))
          (progn
            (setf (aref str r) c
                  i j)))
        (setf r (+ r 1))))
    (substring-no-properties str 0 r)))

;;

(defun shell-command-on-region-and-replace (start end command
                                                  &optional output-buffer
                                                  error-buffer display-error-buffer
                                                  region-noncontiguous-p)
  "Call `shell-command-on-region' with REPLACE set to T."
  (interactive (let (string)
                 (unless (region-active-p)
                   (user-error "Region is not active"))
                 ;; Do this before calling region-beginning
                 ;; and region-end, in case subprocess output
                 ;; relocates them while we are in the minibuffer.
                 (setq string (read-shell-command "Shell command on region: "))
                 ;; call-interactively recognizes region-beginning and
                 ;; region-end specially, leaving them in the history.
                 (list (region-beginning) (region-end)
                       string
                       current-prefix-arg
                       current-prefix-arg
                       shell-command-default-error-buffer
                       (region-noncontiguous-p))))
  (shell-command-on-region start end command
                           output-buffer t
                           error-buffer display-error-buffer
                           region-noncontiguous-p))

;;;;

;;;###autoload
(defun shrink-window-horizontally-fast ()
  (interactive)
  (shrink-window-horizontally 4))

;;;###autoload
(defun enlarge-window-horizontally-fast ()
  (interactive)
  (enlarge-window-horizontally 4))

;;;###autoload
(defun shrink-window-fast ()
  (interactive)
  (shrink-window 4))

;;;###autoload
(defun enlarge-window-fast ()
  (interactive)
  (enlarge-window 4))

(defun text-before-matches? (str)
  "Check if text before point is equal to STR, last character of which should come strictly
before point."
  (text-before-pos-matches? (point) str))

(defun text-before-pos-matches? (p str)
  "Check if text before point is equal to STR, last character of which should come strictly
before point."
  (let ((res t))
    (cl-loop
      for i from 0
      for j downfrom (1- (length str)) to 0
      while res
      do
      (setf res
            (awhen (char-before (- p i))
              (char= it (aref str j)))))
    res))

(defun text-before-matches1? (str)
  "Check if text before point is equal to STR, last character of which should be currently under point."
  (text-before-pos-matches1? (point) str))

(defun text-before-pos-matches1? (p str)
  "Check if text before point is equal to STR, last character of which should be currently under point."
  (let ((res t))
    (cl-loop
      for i from 0
      for j downfrom (1- (length str)) to 0
      while res
      do
      (setf res
            (awhen (char-after (- p i))
              (char= it (aref str j)))))
    res))

(defun text-after-matches? (str)
  "Check if text after point is equal to STR."
  (text-after-pos-matches? (point) str))

(defun text-after-pos-matches? (p str)
  "Check if text after point is equal to STR."
  (let ((res t))
    (cl-loop
      for i from 0 below (length str)
      while res
      do
      (setf res
            (awhen (char-after (+ p i))
              (char= it (aref str i)))))
    res))

;;;

(defun string<-safe (x y)
  "Like ‘string<’ but allows nils. nil is considered smaller that non-nil."
  (if (null x)
      (if (null y)
          nil
        t)
    (if (null y)
        nil
      (string< x y))))

(defun fixnum-<-safe (x y)
  "Like ‘<’ but allows nils. nil is considered smaller that non-nil."
  (if (null x)
      (if (null y)
          nil
        t)
    (if (null y)
        nil
      (< x y))))

(defun bool-< (x y)
  "Like ‘<’ but allows nils. nil is considered smaller that non-nil."
  (if (null x)
      (if (null y)
          nil
        t)
    nil))

;;

(defsubst cap-floor (cap floor value)
  "Ensure VALUE stays between CAP and FLOOR."
  (declare (indent 2))
  (min cap (max floor value)))

;;

(defun filter-elem (pred xs)
  "Combine ‘-filter’ and ‘member’.

Returns cons where car is filtered XS where PRED returned non-nil
and cdr is a boolean whether any element was let out."
  (let* ((res (cons nil nil))
         (tmp res)
         (any-omitted? nil))
    (while xs
      (let ((x (car xs)))
        (if (funcall pred x)
            (setf tmp
                  (setf (cdr tmp) (cons x nil)))
          (setf any-omitted? t)))
      (setf xs (cdr xs)))
    (cons (cdr res)
          any-omitted?)))

;;

(defun file-name-nondirectory-preserve-text-properties (x)
  (let ((nondir (file-name-nondirectory x)))
    (substring x (- (length x) (length nondir)))))

(defun set-string-face-property (new-face str)
  "Set \\='face property of string STR to NEW-FACE and return STR."
  (cl-assert (symbolp new-face))
  (cl-assert (stringp str))
  (put-text-property 0 (length str) 'face new-face str)
  str)

;;

(defun string-contains? (c str)
  "Return t if STR contains character C."
  (let* ((i 0)
         (len (length str))
         (continue (and c
                        (< i len)))
         (res nil))
    (while continue
      (if (eq c (aref str i))
          (setf continue nil
                res t)
        (setf i (+ i 1)
              continue (< i len))))
    res))

(defun append-plists-uniq (new-props old-props)
  "Append two property lists, return new list that has properties from both
OLD-PROPS and NEW-PROPS. Keys of OLD-PROPS that already have value in
NEW-PROPS will be ignored."
  (let* ((res (cons nil nil))
         (tmp res))
    (while old-props
      (let ((prop-name (car old-props)))
        (unless (plist-member new-props prop-name)
          (let ((old-val (cadr old-props)))
            (setf (cdr tmp) (cons prop-name (cons old-val nil))
                  tmp (cddr tmp))))
        (setf old-props (cddr old-props))))
    (setf (cdr tmp) new-props)
    (cdr res)))

(defun replace-match-insert-before-markers (newtext group)
  "Like ‘replace-match’ but inserts NEWTEXT before markers, instead of after."
  (cl-assert (stringp newtext))
  (cl-assert (numberp group))
  (let ((end (match-end group)))
    (goto-char end)
    (delete-region (match-beginning group) end)
    (insert-before-markers newtext)))

(provide 'common)

;; Local Variables:
;; End:

;; common.el ends here
