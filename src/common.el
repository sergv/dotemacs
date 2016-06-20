;; common.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  5 November 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'macro-util)
(require 'custom-predicates)
(require 'dash)

(defsubst remap-interval (a b c d x)
  "Remap x from [a, b] into [c, d]"
  (+ c
     (* (- x a)
        (/ (coerce (- d c) 'float)
           (coerce (- b a) 'float)))))

(defun make-random-gen (init)
  (let ((a 0)
        (b #x7fff)
        (seed init))
    (lambda (begin end)
      (setf seed (logand (1+ (* seed 69069))
                         b))
      (remap-interval a b begin end seed))))

(defun make-simple-random-generator ()
  (let* ((time (current-time))
         (a (first time))
         (b (second time))
         (microsec (third time))
         (gen (make-random-gen
                ;; make an obscure seed
                (+ (logxor a b)
                   ;; this is wery much like random noise
                   microsec
                   (emacs-pid)))
               ))
    (lambda ()
      (funcall gen 0.0 1.0))))

;; yeilds values in range [0..1)
(defun make-tausworthe-random-gen (seed1 seed2 seed3)
  (let ((2-to-32 (expt 2 32)))
    (let ((tausworthe
           (lambda (s a b c d)
             (mod (logxor (ash (logand s c) (- d))
                          (ash (logxor s
                                       (ash s (- a)))
                               b))
                  2-to-32)))
          (a seed1)
          (b seed2)
          (c seed3))
      (lambda ()
        (setf a (funcall tausworthe
                         a
                         13
                         19
                         4294967294
                         12)
              b (funcall tausworthe
                         b
                         2
                         25
                         4294967288
                         4)
              c (funcall tausworthe
                         c
                         3
                         11
                         4294967280
                         17))
        (/ (logxor a b c) (float 2-to-32))))))

(defun make-tausworthe-random-generator ()
  "Return tausworthe random generator obtained
by seeding `make-tausworthe-random-gen' with
current time and"
  (let* ((time (current-time))
         (a (first time))
         (b (second time))
         (microsec (third time))
         (c (+ (* 65536 a) b)))
    (make-tausworthe-random-gen
     (+ (min c microsec) 2)
     (+ (emacs-pid) 8)
     (+ (max c microsec) 16))))

(defparameter *random-gen*
  ;; NB on x32 systems hightest three bits will be zero
  ;; and (expt 2 32)/(ash 1 32) will be 0, so use
  ;; simpler generator that has no overflows
  (if (= 0 (ash 1 31))
    (make-simple-random-generator)
    (make-tausworthe-random-generator))
  "Global random generator")

(defun random-shuffle (vect random-gen)
  "Randoly shuffle vector VECT inplace with supply
of random numbers from RANDOM-GEN."
  (typep vect 'vector)
  (loop
    for i downfrom (1- (length vect)) to 1
    ;; may yield i
    for j = (round (* i (funcall random-gen)))
    do (psetf (aref vect i) (aref vect j)
              (aref vect j) (aref vect i)))
  vect)

(defun shuffle-lines (begin end)
  (interactive "r")
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
      (random-shuffle lines *random-gen*)
      (goto-char begin)
      (loop
        for line across lines
        do
        (insert line)
        (insert "\n")))))


;;;; file utilities

(defalias 'strip-trailing-slash 'directory-file-name)

(defun path-concat (&rest args)
  "Connect paths with standard delimiter"
  (mapconcat #'strip-trailing-slash args "/"))

(defun version-control-directory? (filepath)
  "Test whether FILEPATH contains version control directory as its subpart"
  (string-match-pure? (concat "\\(?:/\\|^\\)\\(?:"
                              (regexp-opt
                               *version-control-directories*)
                              "\\)\\(?:/\\|$\\)")
                      filepath))

(defun read-and-insert-filename (&optional nondir-only?)
  "Read filename with completion from user and insert it at point.
Of course directory names are also supported.

If NONDIR-ONLY? is specified then insert only nondirectory part will be
inserted."
  (interactive "P")
  (let* ((path (funcall (if nondir-only?
                          (comp #'file-name-nondirectory
                                #'strip-trailing-slash)
                          #'identity)
                        (expand-file-name
                         (ido-read-file-name
                          ""
                          nil
                          ""
                          nil
                          nil
                          (lambda (x) (or (file-directory-p x)
                                     (file-exists-p x)))))))
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

(defun* use-repl-modeline (&key (show-column t) (show-directory nil))
  "Set up `mode-line' for use in vairous repl."
  (setf mode-line-format
        `(" %[%b%] "
          ("(%m"
           mode-line-process
           ")")
          (:eval
           (when (buffer-narrowed?)
             "(Narrowed)"))
          " "
          (line-number-mode
           ("%l/"
            (:eval (number-to-string
                    (count-lines (point-min)
                                 (point-max))))
            "(%p)"))
          (column-number-mode
           (2 " %c"))
          ,@(when show-directory
              '("  "
                (:eval default-directory)))
          global-mode-string)))

(defun re-group-matchedp (n)
  "Return non-nil if Nth group matched."
  (match-beginning n))

(defalias 're-group-matched? #'re-group-matchedp)


(defun constantly (x)
  (let ((tmp x))
    (lambda (&rest y)
      (declare (ignore y))
      tmp)))

;;; combinatorics

(defun factorial (x)
  (if (= 0 x)
    1
    (* x (factorial (1- x)))))

(defun choose (n k)
  "Compute binomial coefficient."
  (/ (factorial n) (* (factorial k)
                      (factorial (- n k)))))

(defun permutations (list)
  (if (null list)
    (list nil)
    (-mapcat (lambda (item)
               (-map (lambda (x)
                       (cons item x))
                     (permutations (remove item list))))
             list)))


(defun combinations (n k)
  "Construct all
 / n \\
 \\ k /
combinations"
  (letrec ((collect
            (lambda (start end)
              (if (< start 0)
                (list ())
                (loop
                  for i from start to end
                  nconcing
                  (-map (lambda (rest)
                          (cons i rest))
                        (funcall collect (1- start) (1- i))))))))
    (funcall collect (1- k) (1- n))))

;;;

(defun point-inside-string? ()
  "Return t if point is positioned inside a string."
  (save-excursion
    (let ((state (syntax-ppss (point))))
      (elt state 3))))

(defun point-inside-comment? ()
  "Return t if point is positioned inside a string."
  (save-excursion
    (let ((state (syntax-ppss (point))))
      (elt state 4))))

(defun point-inside-string-or-comment? ()
  "Return t if point is positioned inside a string."
  (save-excursion
    (let ((state (syntax-ppss (point))))
      (or (elt state 3)
          (elt state 4)))))

(defsubst point-not-inside-string-or-comment? ()
  (not (point-inside-string-or-comment?)))

;;;

(defsubst assoc-value (key alist)
  (cadr (assoc key alist)))

(defun keyword-arglist-get-value (key keyword-arglist &optional default)
  "Get value of keyword argument named KEY from KEYWORD-ARGLIST, with
structure like this (:arg1 value1 :arg2 value2 ... :argN valueN)"
  (cond
    ((null? keyword-arglist)
     default)
    ((eq? key (car keyword-arglist))
     (cadr keyword-arglist))
    (t
     (keyword-arglist-get-value key
                                (cddr keyword-arglist)
                                default))))

;;;

(defparameter *invisible-buffers* '()
  "List of buffer name regexps than should not be visible in e.g. ibuffer,
tabbar, etc")

(defparameter invisible-buffers-re nil
  "Regexp that is synced with `*invisible-buffers*' variable.")

(defun add-invisible-buffer (buf-re)
  (assert (string? buf-re))
  (add-to-list '*invisible-buffers* buf-re)
  (let ((buf-re-with-group
         (concat "\\(?:" buf-re "\\)")))
    (setf invisible-buffers-re
          (if invisible-buffers-re
            (concat invisible-buffers-re
                    "\\|"
                    buf-re-with-group)
            buf-re-with-group))))

(defun invisible-buffer? (buf)
  "Returns t if buffer BUF should be regarded as invisible, see also
`*invisible-buffers*'."
  (cond ((or (string? buf)
             (buffer? buf))
         (string-match-pure? invisible-buffers-re
                             (if (string? buf)
                               buf
                               (buffer-name buf))))
        ;; ((string? buf)
        ;;  (-any? (lambda (re)
        ;;           (string-match-pure? re buf))
        ;;         *invisible-buffers*))
        ;; ((buffer? buf)
        ;;  (-any? (lambda (re)
        ;;           (string-match-pure? re (buffer-name buf)))
        ;;         *invisible-buffers*))
        (t
         (error "wrong argument type - not a string nor a buffer: %s"
                buf))))

(defun visible-buffers ()
  "Get list of buffers that are not invisible."
  (--filter (not (invisible-buffer? it)) (buffer-list)))

(add-invisible-buffer (rx
                       bol
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

(defun alist->hash-table (alist &optional cmp)
  "Translate alist of (<key> . <value>) pairs into hash-table."
  (let ((table (make-hash-table :test (or cmp #'equal))))
    (dolist (item alist)
      (puthash (car item) (cdr item) table))
    table))

(defun hash-table->alist (table)
  "Translate hash table into alist of (<key> . <value>) pairs."
  (let ((result '()))
    (maphash (lambda (k v) (push (cons k v) result)) table)
    result))

(defun hash-table-keys (table)
  "Get list of keys of hash table."
  (let ((result '()))
    (maphash (lambda (k v) (push k result)) table)
    result))

(defun hash-table-values (table)
  "Get list of values of hash table."
  (let ((result '()))
    (maphash (lambda (k v) (push v result)) table)
    result))

(defun hash-table-keys-filter (pred table)
  "Get list of keys of hash table for entries that match PRED, i.e. PRED
returns true for key and value."
  (let ((result '()))
    (maphash (lambda (k v) (when (funcall pred k v) (push k result))) table)
    result))

(defun hash-table-entries-matching-re (table re)
  "Return list of TABLE values whose keys match RE."
  (let ((result nil))
    (maphash (lambda (k v)
               (when (string-match-pure? re k)
                 (push v result)))
             table)
    result))

(defun hash-table-merge! (table-main table-aux)
  "Add all entries from TABLE-AUX into TABLE-MAIN."
  (maphash (lambda (k v)
             (puthash k v table-main))
           table-aux))

(defun hash-table-merge-with! (comb-func table-main table-aux)
  "Add all entries from TABLE-AUX into TABLE-MAIN, combine entries present
in both tables with COMB-FUNC, which should take 3 arguments: key, value in
main table and value in aux table."
  (maphash (lambda (k v)
             (if-let (v-main (gethash k table-main))
               (puthash k (funcall comb-func k v-main v) table-main)
               (puthash k v table-main)))
           table-aux))

;;;

;; this may be useful for something
;;
;; (defun make-ntree-node (value children)
;;   (cons 'ntree-node
;;         (cons value children)))
;;
;; (defun ntree-node? (item)
;;   (and (list? item) (not (null? item)) (eq? (car item) 'ntree-node)))
;;
;; (defun ntree-node-value (node)
;;   (assert (ntree-node? node))
;;   (cadr node))
;;
;; (defun ntree-node-children (node)
;;   (assert (ntree-node? node))
;;   (cddr node))
;;
;;
;; (defun print-ntree (node)
;;   (assert (ntree-node? node))
;;   (letrec ((print-node
;;              (lambda (node prefix)
;;                (insert prefix
;;                        (format "%s" (ntree-node-value node))
;;                        "\n")))
;;            (print-rec
;;              (lambda (node offset line-beg)
;;                (let* ((children (ntree-node-children node))
;;                       (last (car-safe (last children)))
;;                       (offset-delta 2)
;;                       (children-offset (+ offset offset-delta))
;;                       (children-line-beg
;;                         (concat line-beg
;;                                 "|"
;;                                 (make-string offset-delta ?\s))))
;;                  (mapc (lambda (child)
;;                          (funcall print-node child (concat line-beg "+--"))
;;                          (funcall print-rec
;;                                   child
;;                                   children-offset
;;                                   children-line-beg))
;;                        (butlast children))
;;                  (when last
;;                    (funcall print-node last (concat line-beg "`--"))
;;                    (funcall print-rec
;;                             last
;;                             children-offset
;;                             children-line-beg))))))
;;     (funcall print-node node "/")
;;     (funcall print-rec node 0 "")))
;;
;;
;; (print-ntree (make-ntree-node 'root
;;                               (list (make-ntree-node 'lvl1-1
;;                                                      (list (make-ntree-node 'lvl2-1 '())
;;                                                            (make-ntree-node 'lvl2-2 '())
;;                                                            (make-ntree-node 'lvl2-3 '())))
;;                                     (make-ntree-node 'lvl1-2 '())
;;                                     (make-ntree-node 'lvl1-3 '()))))
;;

;;;

(defun bisect (item items start end eq? less?)
  "Binary search. Returns index into vector ITEMS.
LESS? is predicate on items and elements of ITEMS.

START is inclusive and END is exclusive in ITEMS."
  ;; if you doubt the implementation and want to improve it make sure
  ;; tests do pass
  (let ((orig-start start)
        (orig-end end))
    (while (< start end)
      (let* ((mid (/ (+ end start) 2))
             (mid-item (aref items mid)))
        (cond ((funcall less? item mid-item)
               (setf end mid))
              ((funcall eq? item mid-item)
               (setf start mid
                     end mid))
              (t
               (setf start (+ mid 1))))))
    start))

(defun bisect-leftmost (item items start end eq? less?)
  "Similar to `bisect' but returns smallest index, idx, in ITEMS for which
\(funcall eq? item (aref items idx)) is true."
  (let ((idx (bisect item items start end eq? less?)))
    (while (and (> idx 0)
                (funcall eq? item (aref items (- idx 1))))
      (setf idx (- idx 1)))
    idx))

(defun bisect-rightmost (item items start end eq? less?)
  "Similar to `bisect' but returns largest index, idx, in ITEMS for which
\(funcall eq? item (aref items idx)) is true."
  (let ((idx (bisect item items start end eq? less?))
        (max-idx (- (length items) 1)))
    (while (and (< idx max-idx)
                (funcall eq? item (aref items (+ idx 1))))
      (setf idx (+ idx 1)))
    idx))

;;;

(defun string-suffix? (string1 string2 &optional ignore-case)
  "Return t if STRING1 is a suffix of STRING2."
  (and (<= (length string1) (length string2))
       (eq t (compare-strings string1 0 nil
                              string2 (- (length string2) (length string1)) nil
                              ignore-case))))

(defun* strip-string-prefix (prefix str &key (starting-at 0))
  "Remove (+ (length PREFIX) STARTING-AT) characters from start of STR."
  (substring str (+ starting-at (length prefix))))

;;;

(defsubst file-modification-time (filename)
  "Return latest modification time of file FILENAME."
  (nth 5 (file-attributes filename 'integer)))

(defsubst file-size (filename)
  "Return size of file FILENAME."
  (nth 7 (file-attributes filename 'integer)))

(if (executable-find "cmp")
  (defun different-files-fast? (file1 file2)
    "Return t if content of FILE1 and FILE2 differs and try to yield answer
faster than byte-by-byte comparison of respecfive file contents."
    (if (= (file-size file1) (file-size file2))
      (= 1 (call-process-shell-command "cmp" nil nil nil file1 file2))
      t))
  (defun different-files-fast? (file1 file2)
    "Return t if content of FILE1 and FILE2 differs and try to yield answer
faster than byte-by-byte comparison of respecfive file contents."
    (if (= (file-size file1) (file-size file2))
      (string=? (with-temp-buffer
                  (insert-file-contents file1)
                  (buffer-substring-no-properties (point-min) (point-max)))
                (with-temp-buffer
                  (insert-file-contents file2)
                  (buffer-substring-no-properties (point-min) (point-max))))
      t)))

;;;

(defun to-linux-line-endings ()
  "Convert line endings in current buffer to linux ones (\\n)."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix nil))

(defun to-windows-line-endings ()
  "Convert line endings in current buffer to windows ones (\\r\\n)."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos nil))

;;;

(defun insert-my-formatted-date ()
  "Insert today's date as \"<Day Name>, <day> <Month name> <Year>\""
  (interactive)
  (insert (format-time-string "%A, %e %B %Y" (current-time))))

(defalias 'insert-current-date #'insert-my-formatted-date)

;;;

(defun* map-files (f file-list &key dont-write)
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
  (mapconcat #'identity lines (or str "\n")))

(defun* split-into-lines (str &optional (omit-nulls t))
  "Split string into list of lines."
  (split-string str "\n" omit-nulls))

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
  (cond ((ring? item)
         (ring-length item))
        ((or (list? item)
             (vector? item)
             (string? item))
         (length item))))

(defun* generic/member (item sequence &key (test #'equal))
  (cond ((ring? sequence)
         (ring-member sequence item :test test))
        ((list? sequence)
         (cl-member item sequence :test test))
        (t
         (error "Not implemented yet"))))

;;;

(defun list< (a b)
  "Check whether list of integers A is lexicographically lesser than
integer list B."
  (let ((done nil)
        (result nil))
    (while (and (not done)
                (not (null? a))
                (not (null? b)))
      (cond ((< (first a) (first b))
             (setf done t
                   result t))
            ((> (first a) (first b))
             (setf done t
                   result nil))
            (t
             (setf a (rest a)
                   b (rest b)))))
    result))

(defsubst list= (a b)
  "Check whether list of integers A is equal to integer list B."
  (equal a b))

;;;

(defun text-between-lines (start-line end-line)
  "Return string of text with properties between beginning of START-LINE and
end of END-LINE in current buffer."
  (save-excursion
    (buffer-substring (progn (goto-line1 start-line)
                             (line-beginning-position))
                      (progn (goto-line1 end-line)
                             (line-end-position)))))

;;;

(defparameter *ignored-file-name-endings*
  (append
   '(".annot" ".cmi" ".cmxa" ".cma" ".cmx" ".cmo" ".o" ".hi" ".p_o" ".p_hi" ".prof_o" ".prof_hi" ".dyn_o" "~" ".bin" ".out" ".lbin" ".a" ".elc" ".glo" ".idx" ".lot" ".class" ".fasl" ".lo" ".la" ".gmo" ".mo" ".bbl" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".dex" ".gz" ".tar" ".bz2" ".xz" ".7z" ".ibc" ".agdai")
   (cond
     ((platform-os-type? 'linux)
      '(".so"))
     ((platform-os-type? 'windows)
      '(".dll" ".pdb" ".lib"))
     (t
      nil)))
  "List of file name endings to generally ignore.")

(defparameter *version-control-directories*
  '(".svn" ".git" ".hg" "_darcs")
  "List of directory names used by version-control systems.")

(defparameter *ignored-directories*
  (append *version-control-directories*)
  "List of directory names to generally ignore.")

(defparameter *ignored-directory-prefixes*
  '(".cabal-sandbox" ".stack-work")
  "List of directory names to generally ignore as a prefixes.")

(setf completion-ignored-extensions
      (append (-map (lambda (x) (concat x "/")) *ignored-directories*)
              *ignored-file-name-endings*)
      grep-find-ignored-files
      (append (list ".#*"
                    "*.exe"
                    "*.prof")
              (-map (lambda (x)
                      (concat "*" x))
                    *ignored-file-name-endings*)))

;;;

(defun insert-current-date ()
  (interactive)
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

(defparameter common/registered-filenames (make-hash-table :test #'equal)
  "Hashtable binding filename strings to themselves. Exists for memory
optimization purposes.")

(defun common/registered-filename (filename)
  (aif (gethash filename common/registered-filenames)
    it
    (progn
      (puthash filename filename common/registered-filenames)
      filename)))

;;;

(defun* pp-to-string* (obj
                       &key
                       (length nil) ;; print-length
                       (depth nil) ;; print-level
                       )
  (let ((print-length length)
        (print-depth depth))
    (pp-to-string obj)))

;;; indentation

(defun indent-to! (col)
  "Indent current line to exactly COL'th column with spaces."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (delete-region (line-beginning-position) (point))
    (dotimes (i col)
      (insert ?\s))))

(defun util/reindent-file (filename)
  "Load FILENAME contents, try to infer mode for it, reindent according
to mode and write new contents back to FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((buffer-file-name filename))
      (normal-mode)
      (when (memq major-mode '(fundamental-mode
                               text-mode))
        (error "cannot reliably infer mode for file %s" filename))
      (vim:indent)
      (write-region (point-min) (point-max) filename))))

(defun indent-relative-backwards ()
  "Indent backwards. Dual to `indent-relative'.
Clone of `haskell-simple-indent-backtab'."
  (interactive)
  (let ((current-point (point))
        (i 0)
        (x 0))
    (goto-char (line-beginning-position))
    (save-excursion
      (while (< (point) current-point)
        (indent-relative)
        (setq i (+ i 1))))
    (dotimes (x (- i 1))
      (indent-relative))))

(defconst +buffer-indent-temporary-filename+
  (concat temporary-file-directory "/indent-buffer.tmp")
  "Path to temporary file reserved for buffer indentation puproses.
See also `*mode-buffer-indent-function-alist*'.")

(defparameter *mode-buffer-indent-function-alist* nil
  "Alist of (major-mode . function) pairs, where functions should take 0
arguments and indent current buffer. See also `+buffer-indent-temporary-filename+'.")

(defun indent-whole-buffer ()
  "Indent whole buffer with `indent-region'."
  (indent-region (point-min) (point-max)))

(defun indent-relative-forward ()
  "Indent forwards similarly to `indent-relative'."
  (interactive)
  (indent-relative+ 'forward))

(defun indent-relative-backward ()
  "Indent backwards similarly to `indent-relative'."
  (interactive)
  (indent-relative+ 'backward))

;; inspired by indent-relative for v24.3.1
(defun indent-relative+ (direction)
  "Space out to under next indent point in previous nonblank line.
An indent point is a non-whitespace character following whitespace.
The following line shows the indentation points in this line.
    ^         ^    ^     ^   ^           ^      ^  ^    ^
If the previous nonblank line has no indent points beyond the
column point starts at, `tab-to-tab-stop' is done instead, unless
this command is invoked with a numeric argument, in which case it
does nothing.

See also `indent-relative-maybe'."
  (save-match-data
    (let ((start-column (current-column))
          indent)
      (save-excursion
        (beginning-of-line)
        (when (re-search-backward "^[^\n]" nil t)
          (let ((end (if (eq? direction 'forward)
                       (min (+ (line-end-position) 1)
                            (point-max))
                       (line-beginning-position))))
            (move-to-column start-column)
            ;; Is start-column inside a tab on this line?
            (when (> (current-column) start-column)
              (backward-char 1))
            (if (eq? direction 'forward)
              (progn
                (skip-chars-forward "^ \t" end)
                (skip-chars-forward " \t" end))
              (progn
                (skip-chars-backward " \t" end)
                (skip-chars-backward "^ \t" end)))
            (unless (= (point) end)
              (setf indent (current-column))))))
      (cond
        (indent
         (indent-to! indent)
         (move-to-column indent))
        ((eq? direction 'forward)
         (tab-to-tab-stop))))))

(defun tab-to-tab-stop-backward ()
  "Like `tab-to-tab-stop' but backwards."
  (interactive)
  (let ((nexttab (indent-next-tab-stop (current-column) t)))
    (delete-horizontal-space t)
    (indent-to nexttab)))

;;;; keeping window's previous buffers and switching to them

(defadvice set-window-buffer (before
                              notify-window-buffer-change
                              activate
                              compile)
  (let ((window (ad-get-arg 0)
                ;; (or (ad-get-arg 0)
                ;;     (selected-window))
                )
        (new-buffer (ad-get-arg 1)))
    (set-window-parameter window
                          'prev-buffers
                          (cons (window-buffer window)
                                (-filter #'buffer-live-p
                                         (window-parameter window 'prev-buffers))))))

(defun switch-to-prev-buffer-in-window ()
  "Switch to previous alive buffer for selected window, if there's one."
  (interactive)
  (let* ((window (selected-window))
         (prev-bufs (-filter #'buffer-live-p
                             (window-parameter window 'prev-buffers))))
    (if (null? prev-bufs)
      (error "no alive previous buffers to switch to")
      (switch-to-buffer (car prev-bufs)))))

;;;

(defun region-active? ()
  "Return t if region, either plain or vim's, is active."
  ;; TODO consider using mark-active
  (or (region-active-p)
      (run-if-fbound vim:visual-mode-p)))

(defun get-region-string-no-properties ()
  "Get string currently selected by a region, or nil
if there's no region."
  (if (region-active?)
    (buffer-substring-no-properties
     (region-beginning)
     (+ (if (run-if-fbound vim:visual-mode-p)
          1
          0)
        (region-end)))
    nil))

(defun get-region-bounds ()
  "Return pair of region bounds, (begin end), depending
on currently active vim highlight mode."
  (if (run-if-fbound vim:visual-mode-p)
    (cond
      ((eq? vim:visual-mode-type 'normal)
       (values (region-beginning) (region-end)))
      ((eq? vim:visual-mode-type 'linewise)
       (values (save-excursion
                 (goto-char (region-beginning))
                 (line-beginning-position))
               (save-excursion
                 (goto-char (region-end))
                 (line-end-position))))
      (t
       (values (region-beginning) (region-end))))
    (values (region-beginning) (region-end))))

;;;

(defun find-first-matching (f xs)
  "Find first x among XS such that (F x) is non-nil, and
return pair (x (F x))."
  ;; imperative and ugly but efficient
  (let ((done nil)
        (y nil)
        (fy nil))
    (while (and (not done)
                (not (null? xs)))
      (when-let (tmp (funcall f (car xs)))
        (setf done t
              y (car xs)
              fy tmp))
      (setf xs (cdr xs)))
    (if done
      (values y fy)
      nil)))

;;;

(defsubst cadr-safe (x)
  (car-safe (cdr-safe x)))

(defun normalize-file-name (fname)
  "Normalize file name"
  (let ((cmdline-normalized
         (command-line-normalize-file-name fname)))
    (if (memq system-type '(ms-dos windows-nt))
      (replace-regexp-in-string "[\\]+" "/" cmdline-normalized)
      cmdline-normalized)))

;;;

(defun abort-recursive-edit* (&optional on-no-recursive-edit)
  "Abort recursive edit or call ON-NO-RECURSIVE-EDIT if no recursive
edit is active."
  (condition-case err
      (abort-recursive-edit)
    (user-error (if err
                  (funcall on-no-recursive-edit)
                  (signal (car err) (cdr err))))))

;;; buffer, window and frame utils


(defun next-buffer (n)
  "Go to the buffer which is at the end of buffer list."
  (interactive "p")
  (dotimes (i n)
    (unbury-buffer)))

(defun prev-buffer (n)
  "Go to the buffer which is at the top of buffer list behind
the current buffer."
  (interactive "p")
  (dotimes (i n)
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
  (let* ((buf-a (current-buffer))
         (pos-a (with-selected-window win-a
                  (with-current-buffer buf-a
                    (point))))
         (buf-b (save-selected-window (select-window win-b t)
                                      (current-buffer)))
         (pos-b (with-selected-window win-b
                  (with-current-buffer buf-b
                    (point)))))
    (if (eq? buf-b buf-a)
      (progn
        (with-selected-window win-a
          (with-current-buffer buf-a
            (goto-char pos-b)))
        (select-window win-b)
        (with-selected-window win-b
          (with-current-buffer buf-b
            (goto-char pos-a))))
      (progn
        (switch-to-buffer buf-b)
        (select-window win-b)
        (switch-to-buffer buf-a)))))

(defun swap-buffers-forward ()
  "Swap current buffer with next buffer"
  (interactive)
  (let* ((curr-win (selected-window))
         (next-win (next-window curr-win 0)))
    (swap-buffers-in-windows curr-win next-win)))

(defun swap-buffers-backward ()
  "Swap current buffer with previous buffer"
  (interactive)
  (let* ((curr-win (selected-window))
         (prev-win (previous-window curr-win 0)))
    (swap-buffers-in-windows curr-win prev-win)))

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
         (entry->bytes (lambda (entry)
                         (when entry
                           (let ((size (second entry))
                                 (used (third entry)))
                             (* size used)))))
         (to-mb (lambda (x) (when x (/ x (* 1024 1024)))))
         ;; (extract-used (lambda (x) (car-safe (cdr-safe (cdr-safe x)))))
         (bytes-used (-sum (-map entry->bytes stats))))
    (format "[%sMb/cons %sMb/vec %sMb/heap %sMb]"
            (funcall to-mb bytes-used)
            (funcall (comp to-mb entry->bytes) (assoc 'conses stats))
            (funcall (comp to-mb entry->bytes) (assoc 'vector-slots stats))
            (funcall (comp to-mb entry->bytes) (assoc 'heap stats)))))

;;;;

(defun delete-if-with-action! (pred items on-deletion)
  "Delete items matching PRED from ITEMS list while applying ON-DELETION
to deleted items. ITEMS will be mutated in order to obtain result."
  (let ((tmp items)
        (prev nil))
    (while tmp
      (let ((item (car tmp)))
        (if (funcall pred item)
          ;; remove the item
          (let ((next-cons (cdr tmp)))
            (if (null? next-cons)
              ;; at the end of list - overwrite current cons
              (progn
                (if prev
                  (setf (cdr prev) nil)
                  (setf items nil))
                (setf tmp nil))
              (setf (car tmp) (car next-cons)
                    (cdr tmp) (cdr next-cons)))
            (funcall on-deletion item))
          ;; keep the item and move forward
          (setf prev tmp
                tmp (cdr tmp)))))
    items))

(defun fontify-merge-markers ()
  (font-lock-add-keywords
   nil
   '(("^<<<<<<< .*$" 0 'font-lock-warning-face t)
     ("^|||||||$" 0 'font-lock-warning-face t) ; "diff3" style
     ("^=======$" 0 'font-lock-warning-face t)
     ("^>>>>>>> .*$" 0 'font-lock-warning-face t))))

(defun kill-new-ignoring-duplicates (text)
  "Similar to `kill-new', but does not append TEXT to `kill-ring' if
topmost `kill-ring' item is equal to text."
  (when (or (null kill-ring)
            (not (string= text (car kill-ring))))
    (kill-new text nil)))

;;;;

(defun save-buffer-if-modified ()
  (if-buffer-has-file
    (when (buffer-modified-p)
      (save-buffer))))

;;;;

(defun current-column ()
  "Return current column - integer number."
  (- (point) (line-beginning-position)))

(defun remove-buffer (&optional buffer-or-name)
  "Remove buffer completely bypassing all its prompt functions.
Save buffer if it has assigned file and this file exists on disk."
  (interactive)
  (let ((old-functions kill-buffer-query-functions)
        (kill-buffer-query-functions nil))
    (if-buffer-has-file
      (when (file-exists? buffer-file-name)
        (save-buffer)))
    (kill-buffer buffer-or-name)
    (setq kill-buffer-query-functions old-functions)))

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
  (and (not (file-executable-p buffer-file-name))
       (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           ;; first alternative - unix shell shebang
           ;; second alternative - emacs "shebang"
           (looking-at-pure? "^\\(?:#!\\|:;[ \t]*exec\\)")))
       (make-file-executable buffer-file-name)
       (shell-command (concat "chmod u+x \"" buffer-file-name "\""))
       (message
        (concat "Saved as script: " buffer-file-name))))

(defun reindent-region (start end)
  "custom function that reindents region, differs from indent-region
 with silent behavior( i.e. no messages)"
  (save-excursion
    (let ((lnum 0)
          (lines (count-lines start end)))
      (goto-char start)
      (while (< lnum lines)
        (incf lnum)
        (indent-for-tab-command)
        (forward-line 1)))))

(defun yank-and-reindent ()
  "Function pastes most recently yanked or killed text
ant reindents it."
  (interactive)
  (yank)
  (reindent-region (region-beginning) (region-end))
  (goto-char (region-end)))

(defun yank-previous ()
  (interactive)
  (yank-pop))

(defun yank-next ()
  (interactive)
  (yank-pop 1))

(defun yank-previous-and-reindent ()
  (interactive)
  (yank-previous)
  (reindent-region (region-beginning) (region-end))
  (goto-char (region-end)))

(defun yank-next-and-reindent ()
  (interactive)
  (yank-next)
  (yank-pop 1)
  (reindent-region (region-beginning) (region-end))
  (goto-char (region-end)))


(defun delete-word (count)
  "Delete characters forward until encountering the end of a word.
With argument COUNT, do this that many times."
  (interactive "p")
  (delete-region (point)
                 (progn
                   ;; (vim-mock:motion-fwd-word count)
                   (forward-word arg)
                   (point))))

(defun delete-word* (count)
  "Delete characters backard until encountering the end of a word.
With argument COUNT, do this that many times."
  (interactive "p")
  (delete-region (point)
                 (progn
                   (vim-mock:motion-fwd-WORD count)
                   (point))))

(defun backward-delete-word (count)
  "Delete characters backward until encountering the beginning of a word.
With argument COUNT, do this that many times."
  (interactive "p")
  (delete-region (point)
                 (progn
                   ;; (vim-mock:motion-bwd-word count)
                   (backward-word count)
                   (point))))

(defun backward-delete-word* (count)
  "Delete characters backward until encountering the beginning of a word.
With argument COUNT, do this that many times."
  (interactive "p")
  (delete-region (point)
                 (progn
                   (vim-mock:motion-bwd-WORD count)
                   (point))))

(defsubst whitespace-char? (char)
  (or (= char ?\n)
      (= char ?\r)
      (= ?\s (char-syntax char))))

(defsubst whitespace-char-p (char)
  (or (char= char ?\s)
      (char= char ?\n)
      (char= char ?\t)))

(defalias 'whitespace-charp 'whitespace-char-p)

(defun delete-whitespace-forward ()
  "Delete whitespaces forward until non-whitespace
character found"
  (interactive)
  (while (and (not (bobp))
              (whitespace-char? (char-after))
              (not (get-char-property (1- (point)) 'read-only)))
    (delete-char 1)))

(defun delete-whitespace-backward ()
  "Delete whitespaces backward until non-whitespace
character found"
  (interactive)
  (while (and (not (bobp))
              (whitespace-char? (char-before))
              (not (get-char-property (1- (point)) 'read-only)))
    (delete-char -1)))

(defun delete-current-line ()
  "Delete line where point is currently positioned including
trailing newline"
  (beginning-of-line)
  (while (and (not (eobp))
              (not (char= ?\n (char-after))))
    (delete-char 1))
  (unless (eobp)
    (delete-char 1)))

(defsubst current-line ()
  "Return line point is currently on."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defsubst current-line-with-properties ()
  "Return line point is currently on."
  (buffer-substring (line-beginning-position)
                    (line-end-position)))

(defsubst skip-to-indentation ()
  "Move point to first non-whitespace character of line,
lighter than `back-to-indentation'."
  (beginning-of-line nil)
  (skip-syntax-forward " " (line-end-position))
  (backward-prefix-chars))

(defun indentation-size ()
  "Return indentation size for current line."
  (save-excursion
    (skip-to-indentation)
    (current-column)))

(defsubst count-lines1 (begin end)
  "Return line count in region like `count-lines' but don't
confuse when point is not at the beginning of line"
  (+ (count-lines begin end)
     (if (equal (current-column) 0)
       1
       0)))

(defun backward-line (&optional count)
  "Call `forward-line' in the opposite direction"
  (interactive)
  (forward-line (- (or count 1))))



(defun trim-whitespace (str)
  "Trim leading and tailing whitespace from STR."
  (when str
    (save-match-data
      (let ((s (if (symbolp str) (symbol-name str) str)))
        (replace-regexp-in-string "\\(?:^[ \t\v\f\n]*\\|[ \t\v\f\n]*$\\)" "" s)))))

(defun remove-whitespace (str)
  "Remove all occurences of various whitespace characters from string."
  (save-match-data
    (let ((s (if (symbolp str)
               (symbol-name str)
               str)))
      (replace-regexp-in-string "[ \t\v\f\n]+" "" s))))

(defun trim-whitespace-left (str)
  "Trim leading whitespace from STR."
  (when str
    (save-match-data
      (let ((s (if (symbolp str) (symbol-name str) str)))
        (replace-regexp-in-string "^[ \t\v\f\n]*" "" s)))))

(defun trim-whitespace-right (str)
  "Trim trailing whitespace from STR."
  (when str
    (save-match-data
      (let ((s (if (symbolp str) (symbol-name str) str)))
        (replace-regexp-in-string "[ \t\v\f\n]*$" "" s)))))


(defsubst goto-line1 (line)
  "Set point at the beginning of line LINE counting from line 1 at
beginning of buffer. Does not cause \"Scan error: \"Unbalanced parentheses\"\" as
`goto-line' does."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun file-contents-matches-re (filename re)
  "Return t if file FILENAME exists and it contents matches RE."
  (when (file-exists? filename)
    (save-match-data
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (when (search-forward-regexp re nil t)
          t)))))

;;;;

;; Originally from stevey, adapted to support moving to a new directory.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (unless buffer-file-name
       (error "Buffer '%s' is not visiting a file" (buffer-name)))
     (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                     buffer-file-name))))))
  (when (equal new-name "")
    (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                   (expand-file-name (file-name-nondirectory buffer-file-name)
                                     new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (when (file-exists-p buffer-file-name)
    (rename-file buffer-file-name new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
      (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
    (message "Renamed to %s" new-name)))

(defun copy-file-and-open (new-name)
  "Copy current file to NEW-NAME and open it."
  (interactive
   (progn
     (unless buffer-file-name
       (error "Buffer '%s' is not visiting a file" (buffer-name)))
     (list (read-file-name (format "Copy %s to: " (file-name-nondirectory
                                                   buffer-file-name))))))
  (when (equal new-name "")
    (error "Aborted copy"))
  (setf new-name (if (file-directory-p new-name)
                   (expand-file-name (file-name-nondirectory
                                      buffer-file-name)
                                     new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (when (file-exists-p buffer-file-name)
    (copy-file buffer-file-name new-name 1 nil t t)
    (find-file new-name)
    (message "Copied to %s" new-name)))

(defun delete-file-or-directory (name)
  "Delete NAME if it's either file or directory."
  (interactive)
  (let ((entity (strip-trailing-slash
                 (expand-file-name
                  (read-file-name "File or directory to delete: "
                                  default-directory
                                  ""
                                  t)))))
    (cond
      ((file-directory-p entity)
       (when (and (directory-files dir
                                   nil
                                   directory-files-no-dot-files-regexp
                                   t)
                  (y-or-n-p "Directory not empty, really delete? "))
         (delete-directory entity t)))
      ((file-regular-p entity)
       (delete-file entity))
      (t
       (error "Name %s designates neither file nor directory")))))

;;; rotate list functions, very old...

(defun rotate-entry-list (listvar)
  "Rotate list of any etries such that list '(X Y Z) becomes '(Y Z X)"
  (set listvar (let ((value (symbol-value listvar)))
                 (cond ((null? value) nil)
                       ((null? (cdr value)) value)
                       (t (let ((new-list (cdr value)))
                            (setcdr value nil)
                            (nconc new-list value)
                            new-list))))))

(defun rotate-entry-list-backward (listvar)
  "Rotate list of any etries such that list '(X Y Z) becomes '(Z X Y)"
  (set listvar (let ((value (symbol-value listvar)))
                 (cond ((null? value) nil)
                       ((null? (cdr value)) value)
                       (t (while (cddr value)
                            (setq value (cdr value)))
                          (let ((last-elem (cdr value)))
                            (setcdr last-elem (symbol-value listvar))
                            (setcdr value nil)
                            last-elem))))))

;;;;

(defvar-local inhibit-delete-trailing-whitespace nil
  "Whether function `delete-trailing-whitespace+' should do actual deletion.")

(defun toggle-inhibit-delete-trailing-whitespace ()
  "Toggle `inhibit-delete-trailing-whitespace' option."
  (interactive)
  (if (setf inhibit-delete-trailing-whitespace
            (not inhibit-delete-trailing-whitespace))
    (message "Inhibition enabled")
    (message "Inhibition disabled")))

(defun inhibit-delete-trailing-whitespace? ()
  "Function that says whether trailing whitespace should be deleted for current
buffer."
  (or inhibit-delete-trailing-whitespace
      (eq? major-mode 'diff-mode)))

(defun delete-trailing-whitespace+ ()
  "This function removes spaces and tabs on every line after
last non-whitespace character."
  (unless (inhibit-delete-trailing-whitespace?)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" nil t)
          (delete-region (match-beginning 0) (match-end 0)))))))

;;; tabbar stuff

;; Some useful abstractions to move based on
;; symbols representing direction

(defsubst direction-to-num (dir)
  "Translate direction symbol to numeric representation suitable
for passing to Emacs native functions."
  (cond
    ((eq dir 'forward)
     1)
    ((eq dir 'backward)
     -1)
    (t
     nil)))

(defun* move-by-line (direction &optional (count 1))
  "Move COUNT lines in specified direction, which could
have 'forward or 'backward value."
  (forward-line (* count (direction-to-num direction))))

(defun* move-by-line-backward (direction &optional (count 1))
  "Move COUNT lines backwards in specified direction, which could
have 'forward or 'backward value."
  (backward-line (* count (direction-to-num direction))))

(defsubst char= (a b)
  (char-equal a b))

(defun cadr-safe (x)
  (car-safe (cdr-safe x)))

(defun cddr-safe (x)
  (cdr-safe (cdr-safe x)))

;;;;

(defsubst string->symbol (str)
  (intern str))

(defsubst symbol->string (sym)
  (symbol-name sym))

(defsubst char->string (char)
  (char-to-string char))

(defsubst string->char (str)
  (string-to-char str))


(defsubst number->string (n)
  (number-to-string n))

(defsubst string->number (str)
  (string-to-number str))


(defsubst string->list (str)
  (coerce str 'list))

(defsubst list->string (items)
  (coerce items 'string))

(defsubst vector->list (str)
  (coerce str 'list))

(defsubst list->vector (items)
  (coerce items 'vector))

(defsubst int-vector->string (v)
  "Convernt vector of integers to string."
  (coerce v 'string))

(defsubst char=? (a b)
  (char-equal a b))

;;;;

(defsubst first-safe (x)
  (car-safe x))

(defsubst rest-safe (x)
  (cdr-safe x))

(defmacro more-clojure/comp-impl (functions
                                  fallback-function
                                  use-apply-for-last-func)
  "Optimization trick to expand chains of composed functions instead of
using loop as in `more-clojure/comp'. But if expansion could not be done (e.g.
some of FUNCTIONSS is an expression that is expected to be evaluated right
where comp is called) then FALLBACK-FUNCTION will be used."
  (block cannot-optimize
    (let* ((args-var (gensym "args-var"))
           (strip-quotation
            (lambda (x)
              (if (and (list? x)
                       (not (null? x))
                       (memq (first x) '(quote function)))
                (first (rest x))
                x)))
           (make-call
            (lambda (expr last-arg use-apply funcs)
              (let ((call-form (if use-apply 'apply 'funcall)))
                (pcase expr
                  (`(,(or `function `quote) ,func)
                   (if (and (not use-apply)
                            (symbol? func))
                     `(,func ,last-arg)
                     `(,call-form ,expr ,last-arg)))
                  (`(,(or `partial `apply-partially) ,func . ,args)
                   (let ((f (funcall strip-quotation func)))
                     (if (and (not use-apply)
                              (symbol? f))
                       `(,f ,@args ,last-arg)
                       `(,call-form ,func ,@args ,last-arg))))
                  (`(partial-first ,func . ,args)
                   (let ((f (funcall strip-quotation func)))
                     (if (and (not use-apply)
                              (symbol? f))
                       `(,f ,last-arg ,@args)
                       `(,call-form ,func ,last-arg ,@args))))
                  (_
                   (cl-return-from cannot-optimize
                     `(,(funcall strip-quotation fallback-function)
                       ,@functions))))))))
      (letrec ((iter
                (lambda (funcs)
                  (funcall make-call
                           (first funcs)
                           (if (not (null? (rest funcs)))
                             (funcall iter (rest funcs))
                             args-var)
                           (and (null? (rest funcs))
                                use-apply-for-last-func)
                           funcs))))
        `(lambda
           ,(if use-apply-for-last-func
              (list &rest ,args-var)
              (list args-var))
           ,(funcall iter functions))))))

(defun more-clojure/comp (f &rest funcs)
  "Fallback function composition routine."
  (let ((functions (reverse (cons f funcs))))
    (lambda (arg)
      (let ((result (funcall (first functions) arg)))
        (dolist (func (rest functions))
          (setf result (funcall func result)))
        result))))

(defmacro comp (f &rest funcs)
  "More or less intelligent creator of function compositions that can
optimize away common use cases."
  `(more-clojure/comp-impl ,(cons f funcs)
                           more-clojure/comp
                           nil))

(defun more-clojure/comp* (f &rest funcs)
  "Fallback function composition routine, creates lambdas in runtime."
  (let ((functions (reverse (cons f funcs))))
    (lambda (arg)
      (let ((result (apply (first functions) arg)))
        (dolist (func (rest functions))
          (setf result (funcall func result)))
        result))))

(defmacro comp* (f &rest funcs)
  "Similar to `comp' but uses `apply' for last function."
  `(more-clojure/comp-impl ,(cons f funcs)
                           more-clojure/comp*
                           t))

(defalias 'partial #'apply-partially)

(defun partial-first (f &rest args)
  "Just like `partial' but adds ARGS at the end of argument list when
F will be called."
  (lambda (&rest more-args)
    (apply f (append more-args args))))

;;;;

(defun open-buffer-as-pdf ()
  "Open current buffer's pdf file, if any, in suitable pdf viewer program
(e.g. okular for linux)."
  (interactive)
  (let ((doc-name (concat (file-name-sans-extension buffer-file-name) ".pdf")))
    (if (file-exists? doc-name)
      (start-process-shell-command "okular - tex preview"
                                   nil
                                   (concat "okular"
                                           " "
                                           (shell-quote-argument doc-name)))
      (error "No pdf file found"))))

;;;;

(defun mk-regexp-from-alts (alts)
  (when alts
    (mapconcat (lambda (x) (concat "\\(?:" x "\\)"))
               alts
               "\\|")))

(defun globs-to-regexp (globs)
  (mk-regexp-from-alts (-map #'wildcard-to-regexp globs)))

(defun ci-looking-at (regexp)
  (let ((case-fold-search t))
    (looking-at regexp)))

(defun ci-looking-at-p (regexp)
  (let ((case-fold-search t))
    (looking-at-p regexp)))

;;;;

;; Heavy autoloads

(autoload 'shell-command+ "common-heavy" nil t)
(autoload 'find-filename-in-tree-recursive "common-heavy" nil t)
(autoload 'extract-unicode "common-heavy")
(autoload 'input-unicode "common-heavy" nil t)
(autoload 'merge-emacs-configs "common-heavy")
(autoload 'merge-emacs-configs-default "common-heavy" nil t)
(autoload 'remove-tabs "common-heavy" nil t)
(autoload 'start-file-manager "common-heavy" nil t)
(autoload 'start-terminal-emulator "common-heavy" nil t)
(autoload 'remove-duplicates-from-sorted-list-by "common-heavy" nil nil)
(autoload 'rm "common-heavy" nil t)
(autoload 'transpose-windows "common-heavy" nil t)
(autoload 'narrow-to-region-indirect "common-heavy" nil t)
(autoload 'fontify-conflict-markers "common-heavy")
(autoload 'resolve-obs-or-rel-filename "common-heavy")
(autoload 'remove-duplicates-sorted "common-heavy")
(autoload 'remove-duplicates-sorting "common-heavy")
(autoload 'remove-duplicates-hashing "common-heavy")

(autoload 'find-rec-do "find-files")
(autoload 'find-rec "find-files")
(autoload 'find-rec* "find-files")

(provide 'common)

;; Local Variables:
;; End:

;; common.el ends here
