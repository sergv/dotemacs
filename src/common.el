;; common.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  5 November 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'cl-lib))

(require 'macro-util)
(require 'custom-predicates)

(require 'f)
(require 's)
(require 'dash)
(require 'v)


(autoload 'if-let "subr-x" nil nil 'macro)

(defsubst remap-interval (a b c d x)
  "Remap x from [a, b] into [c, d]"
  (declare (pure t) (side-effect-free error-free))
  (+ c
     (* (- x a)
        (/ (cl-coerce (- d c) 'float)
           (cl-coerce (- b a) 'float)))))

(defun make-random-gen (init)
  (declare (pure t) (side-effect-free error-free))
  (let ((a 0)
        (b #x7fff)
        (seed init))
    (lambda (begin end)
      (setf seed (logand (1+ (* seed 69069))
                         b))
      (remap-interval a b begin end seed))))

(defun make-simple-random-generator ()
  (declare (pure t) (side-effect-free error-free))
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
  (declare (pure t) (side-effect-free error-free))
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

(defvar *random-gen*
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
  (declare (pure t) (side-effect-free error-free))
  (mapconcat #'strip-trailing-slash args "/"))

(defun version-control-directory? (filepath)
  "Test whether FILEPATH contains version control directory as its subpart"
  (declare (pure nil) (side-effect-free t))
  (string-match-p (concat "\\(?:/\\|^\\)\\(?:"
                          (regexp-opt
                           +version-control-directories+)
                          "\\)\\(?:/\\|$\\)")
                  filepath))

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
         (path (if (and default-directory
                        (not abs-path?))
                   (file-relative-name abs-path default-directory)
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

(defun re-group-matchedp (n)
  "Return non-nil if Nth group matched."
  (declare (pure nil) (side-effect-free t))
  (match-beginning n))

(defalias 're-group-matched? #'re-group-matchedp)


(defun constantly (x)
  (declare (pure t) (side-effect-free t))
  (let ((tmp x))
    (lambda (&rest y)
      (declare (ignore y))
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
  (declare (pure t) (side-effect-free t))
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

(defun point-inside-string? (&optional pos)
  "Return t if point is positioned inside a string."
  (save-excursion
    (let ((state (syntax-ppss (or pos (point)))))
      (elt state 3))))

(defun point-inside-comment? (&optional pos)
  "Return t if point is positioned inside a string."
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
    ((null? keyword-arglist)
     default)
    ((eq? key (car keyword-arglist))
     (cadr keyword-arglist))
    (t
     (keyword-arglist-get-value key
                                (cddr keyword-arglist)
                                default))))

;;;

(defvar *invisible-buffers* '()
  "List of buffer name regexps than should not be visible in e.g. ibuffer,
tabbar, etc")

(defvar invisible-buffers-re nil
  "Regexp that is synced with `*invisible-buffers*' variable.")

(defun add-invisible-buffer (buf-re)
  (declare (pure nil) (side-effect-free nil))
  (cl-assert (string? buf-re))
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
  (declare (pure nil) (side-effect-free t))
  (cond ((or (stringp buf)
             (bufferp buf))
         (string-match-p invisible-buffers-re
                         (if (string? buf)
                             buf
                           (buffer-name buf))))
        (t
         (error "wrong argument type - neither a string or a buffer: %s"
                buf))))

(defun visible-buffers ()
  "Get list of buffers that are not invisible."
  (declare (pure nil) (side-effect-free t))
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
  (declare (pure t) (side-effect-free t))
  (let ((table (make-hash-table :test (or cmp #'equal))))
    (dolist (item alist)
      (puthash (car item) (cdr item) table))
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
    (maphash (lambda (k v) (push k result)) table)
    result))

(defun hash-table-values (table)
  "Get list of values of hash table."
  (declare (pure t) (side-effect-free t))
  (let ((result '()))
    (maphash (lambda (k v) (push v result)) table)
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

(defconst +tar-regexp+
  (rx "."
      (or "tgz"
          "t7z"
          "tbz"
          "tbz2"
          "txz"
          "taz"
          "tar"
          "tar.gz"
          "tar.bz2"
          "tar.7z"
          "tar.xz"
          "tar.lz"
          "tar.lzip")
      eos))

(defconst +archive-regexp+
  (rx "."
      (or "tgz"
          "t7z"
          "tbz"
          "tbz2"
          "txz"
          "taz"
          "tar"
          "tar.gz"
          "tar.bz2"
          "tar.7z"
          "tar.xz"
          "tar.lz"
          "tar.lzip"

          "arj"
          "lzh"
          "zip"
          "z"
          "Z"
          "gz"
          "bz"
          "deb"
          "rpm"
          "lzip"
          "lz"
          "xz")
      eos))

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
;;   (cl-assert (ntree-node? node))
;;   (cadr node))
;;
;; (defun ntree-node-children (node)
;;   (cl-assert (ntree-node? node))
;;   (cddr node))
;;
;;
;; (defun print-ntree (node)
;;   (cl-assert (ntree-node? node))
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
  (declare (pure t) (side-effect-free t))
  ;; if you doubt the implementation and want to improve it make sure
  ;; tests do pass
  (cl-assert (< start end))
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
  (declare (pure t) (side-effect-free t))
  (let ((idx (bisect item items start end eq? less?)))
    (while (and (> idx 0)
                (funcall eq? item (aref items (- idx 1))))
      (setf idx (- idx 1)))
    idx))

(defun bisect-rightmost (item items start end eq? less?)
  "Similar to `bisect' but returns largest index, idx, in ITEMS for which
\(funcall eq? item (aref items idx)) is true."
  (declare (pure t) (side-effect-free t))
  (let ((idx (bisect item items start end eq? less?))
        (max-idx (- (length items) 1)))
    (while (and (< idx max-idx)
                (funcall eq? item (aref items (+ idx 1))))
      (setf idx (+ idx 1)))
    idx))

;;;

(defun string-suffix? (string1 string2 &optional ignore-case)
  "Return t if STRING1 is a suffix of STRING2."
  (declare (pure t) (side-effect-free t))
  (and (<= (length string1) (length string2))
       (eq t (compare-strings string1 0 nil
                              string2 (- (length string2) (length string1)) nil
                              ignore-case))))

(defun* strip-string-prefix (prefix str &key (starting-at 0))
  "Remove (+ (length PREFIX) STARTING-AT) characters from start of STR."
  (declare (pure t) (side-effect-free t))
  (substring str (+ starting-at (length prefix))))

;;;

(defsubst file-modification-time (filename)
  "Return latest modification time of file FILENAME."
  (declare (pure nil) (side-effect-free t))
  (nth 5 (file-attributes filename 'integer)))

(defsubst file-size (filename)
  "Return size of file FILENAME."
  (declare (pure nil) (side-effect-free t))
  (nth 7 (file-attributes filename 'integer)))

(if (executable-find "cmp")
    (defun different-files-fast? (file1 file2)
      "Return t if content of FILE1 and FILE2 differs and try to yield answer
faster than byte-by-byte comparison of respective file contents."
      (declare (pure nil) (side-effect-free t))
      (if (= (file-size file1) (file-size file2))
          (= 1 (call-process-shell-command "cmp" nil nil nil file1 file2))
        t))
  (defun different-files-fast? (file1 file2)
    "Return t if content of FILE1 and FILE2 differs and try to yield answer
faster than byte-by-byte comparison of respecfive file contents."
    (declare (pure nil) (side-effect-free t))
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
        ((or (list? item)
             (vector? item)
             (string? item))
         (length item))
        (t
         (error "Cannot determine generic length of item %s" item))))

(defun generic/member (item sequence test)
  (declare (pure t) (side-effect-free t))
  (cond ((ring? sequence)
         (ring-member sequence item :test test))
        ((list? sequence)
         (cl-member item sequence :test test))
        (t
         (error "Cannot determine generic membership of item %s in sequence %s"
                item
                sequence))))

;;;

(defun list< (a b)
  "Check whether list of integers A is lexicographically lesser than
integer list B."
  (declare (pure t) (side-effect-free t))
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
  (declare (pure t) (side-effect-free t))
  (equal a b))

;;;

(defun text-between-lines (start-line end-line)
  "Return string of text with properties between beginning of START-LINE and
end of END-LINE in current buffer."
  (declare (pure nil) (side-effect-free t))
  (save-excursion
    (buffer-substring (progn (goto-line-dumb start-line)
                             (line-beginning-position))
                      (progn (goto-line-dumb end-line)
                             (line-end-position)))))

;;;

(defconst +build-products-extensions+
  (append
   '(".cmi" ".cmxa" ".cma" ".cmx" ".cmo"
     ".o" ".obj" ".hi" ".chi" ".p_o" ".p_hi" ".prof_o" ".prof_hi" ".dyn_o" ".dyn_hi"
     ".a"
     ".mix" ".tix"
     ".fasl" ".lo" ".la" ".gmo" ".mo"
     ".pyc" ".pyo"
     ".class" ".dex"
     ".ibc" ".agdai"
     ".elc")
   (fold-platform-os-type
    nil
    '(".pdb" ".lib")))
  "List of file name endings to generally ignore.")

(defconst +ignored-file-extensions+
  (append
   +build-products-extensions+
   '(".annot"
     "~" ".bin" ".out" ".lbin" ".elc" ".glo" ".idx" ".lot"
     ".bbl" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps"
     ".fns" ".kys" ".pgs" ".tps" ".vrs"
     ".gz" ".tar" ".bz2" ".xz" ".7z")
   (fold-platform-os-type
    '(".so")
    '(".dll")))
  "List of file name endings to generally ignore.")

(defconst +version-control-directories+
  '(".svn" ".git" ".hg" "_darcs" ".pijul")
  "List of directory names used by version-control systems.")

(defconst +ignored-directories+
  (append +version-control-directories+
          (list "dist" "node_modules" ".HTF" ".idea"))
  "List of directory names to generally ignore.")

(defconst +ignored-directory-prefixes+
  '(".cabal-sandbox" ".stack-work" "dist-")
  "List of directory names to generally ignore as a prefixes.")

(setf completion-ignored-extensions
      (eval-when-compile
        (append (-map (lambda (x) (concat x "/")) +ignored-directories+)
                +ignored-file-extensions+))
      grep-find-ignored-files
      (eval-when-compile
        (append (list ".#*"
                      "*.exe"
                      "*.prof")
                (-map (lambda (x)
                        (concat "*" x))
                      +ignored-file-extensions+))))

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

(defun* pp-to-string* (obj
                       &key
                       (length nil) ;; print-length
                       (depth nil)  ;; print-level
                       )
  (let ((print-length length)
        (print-depth depth))
    (pp-to-string obj)))

;;;; keeping window's previous buffers and switching to them

(defadvice set-window-buffer (before
                              notify-window-buffer-change
                              activate
                              compile)
  (let ((window (ad-get-arg 0))
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

;; (defadvice region-active-p (around
;;                             region-active-p/take-vim-visual-mode-into-account
;;                             activate
;;                             compile)
;;
;;   ad-do-it
;;   (or
;;       (vim:visual-mode-p)))
;;
;; (defadvice use-region-p (around
;;                          use-region-p/take-vim-visual-mode-into-account
;;                          activate
;;                          compile)
;;   (or ad-do-it
;;       (vim:visual-mode-p)))

(defun get-region-string-no-properties ()
  "Get string currently selected by a region, or nil
if there's no region."
  (declare (pure nil) (side-effect-free t))
  (if (region-active-p)
      (buffer-substring-no-properties
       (region-beginning)
       (region-end))
    (error "Region not active")))

(defmacro with-region-bounds (start end &rest body)
  "Call BODY with variables START and END bound to current region bounds."
  (declare (indent 2))
  (cl-assert (symbolp start))
  (cl-assert (symbolp end))
  `(progn
     (unless (region-active-p)
       (error "Region not active"))
     (let ((,start nil)
           (,end nil))
       (if (and (vim:visual-mode-p)
                (eq? vim:visual-mode-type 'linewise))
           (setf ,start (save-excursion
                          (goto-char (region-beginning))
                          (line-beginning-position))
                 ,end (save-excursion
                        (goto-char (region-end))
                        (line-end-position)))
         (setf ,start (region-beginning)
               ,end (region-end)))
       (setf end (min end (point-max)))
       ,@body)))

(defun get-region-bounds ()
  "Return pair of region bounds, (begin . end), depending
on currently active vim highlight mode."
  (declare (pure nil) (side-effect-free t))
  (with-region-bounds start end
    (cons start end)))

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
  (declare (pure t) (side-effect-free t))
  (car-safe (cdr-safe x)))

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
         (mbytes-used (-sum (-map entry->mb stats))))
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
topmost `kill-ring' item is equal to text."
  (when (or (null kill-ring)
            (not (string= text (car kill-ring))))
    (kill-new text nil)))

;;;;

(defun save-buffer-if-modified ()
  (when-buffer-has-file
    (when (buffer-modified-p)
      (save-buffer))))

;;;;

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
  (and (not (file-executable-p buffer-file-name))
       (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           ;; first alternative - unix shell shebang
           ;; second alternative - emacs "shebang"
           (looking-at-p "^\\(?:#!\\|:;[ \t]*exec\\)")))
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

(defun delete-whitespace-forward ()
  "Delete whitespaces forward until non-whitespace
character found"
  (interactive)
  (let ((start (point)))
    (while (and (not (eobp))
                (whitespace-char? (char-after))
                (not (get-char-property (1- (point)) 'read-only)))
      (forward-char 1))
    (delete-region start (point))))

(defun delete-whitespace-backward ()
  "Delete whitespaces backward until non-whitespace
character found. Returns t if any whitespace was actually deleted."
  (interactive)
  (let ((any-deleted? nil)
        (start (point)))
    (while (and (not (bobp))
                (whitespace-char? (char-before))
                (not (get-char-property (1- (point)) 'read-only)))
      (forward-char -1)
      (setf any-deleted? t))
    (when any-deleted?
      (delete-region (point) start))
    any-deleted?))

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
further that END-POS."
  (skip-syntax-forward " " (or end-pos (line-end-position)))
  (backward-prefix-chars))

(defsubst skip-to-indentation (&optional end-pos)
  "Move point to first non-whitespace character of line, but no
further than END-POS."
  (beginning-of-line nil)
  (skip-indentation-forward end-pos))

(defun indentation-size ()
  "Return indentation size for current line."
  (save-excursion
    (skip-to-indentation)
    (current-column)))

(defun current-line-indentation-str (&optional end-pos)
  (save-excursion
    (beginning-of-line nil)
    (let ((start (point)))
      (skip-indentation-forward end-pos)
      (let ((end (point)))
        (buffer-substring-no-properties start end)))))

(defsubst count-lines-dumb (begin end)
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
        (replace-regexp-in-string "\\(?:\\`[ \t\v\f\r\n]*\\|[ \t\v\f\r\n]*\\'\\)" "" s)))))

(defun remove-whitespace (str)
  "Remove all occurences of various whitespace characters from string."
  (save-match-data
    (let ((s (if (symbolp str)
                 (symbol-name str)
               str)))
      (replace-regexp-in-string "[ \t\v\f\r\n]+" "" s))))

(defun trim-whitespace-left (str)
  "Trim leading whitespace from STR."
  (when str
    (save-match-data
      (let ((s (if (symbolp str) (symbol-name str) str)))
        (replace-regexp-in-string "\\`[ \t\v\f\r\n]*" "" s)))))

(defun trim-whitespace-right (str)
  "Trim trailing whitespace from STR."
  (when str
    (save-match-data
      (let ((s (if (symbolp str) (symbol-name str) str)))
        (replace-regexp-in-string "[ \t\v\f\n\r]*\\'" "" s)))))


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

(defsubst char= (a b)
  (declare (pure t) (side-effect-free t))
  (char-equal a b))

(defsubst cadr-safe (x)
  (declare (pure t) (side-effect-free t))
  (car-safe (cdr-safe x)))

(defsubst cddr-safe (x)
  (declare (pure t) (side-effect-free t))
  (cdr-safe (cdr-safe x)))

;;;;

(defsubst string->symbol (str)
  "Convert string STR to symbol."
  (declare (pure nil) (side-effect-free nil))
  (intern str))

(defsubst symbol->string (sym)
  "Convert symbol SYM to string."
  (declare (pure t) (side-effect-free t))
  (symbol-name sym))

(defsubst char->string (char)
  (declare (pure t) (side-effect-free t))
  (char-to-string char))

(defsubst string->char (str)
  (declare (pure t) (side-effect-free t))
  (string-to-char str))


(defsubst number->string (n)
  (declare (pure t) (side-effect-free t))
  (number-to-string n))

(defsubst string->number (str)
  (declare (pure t) (side-effect-free t))
  (string-to-number str))


(defsubst string->list (str)
  (declare (pure t) (side-effect-free t))
  (append str nil))

(defsubst list->string (items)
  (declare (pure t) (side-effect-free t))
  (concat items))

(defsubst vector->list (x)
  (declare (pure t) (side-effect-free t))
  (append x nil))

(defsubst list->vector (items)
  (declare (pure t) (side-effect-free t))
  (vconcat items))

(defsubst int-vector->string (v)
  "Convernt vector of integers to string."
  (declare (pure t) (side-effect-free t))
  (concat v))

(defsubst char=? (a b)
  (declare (pure t) (side-effect-free t))
  (char-equal a b))

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
  (mk-regexp-from-alts (-map #'wildcard-to-regexp globs)))

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

(defmacro fold-direction (direction if-forward if-backward)
  (declare (indent 1))
  `(pcase ,direction
     (`forward  ,if-forward)
     (`backward ,if-backward)
     (_         (error "Invalid direction: %s" ,direction))))

(defun wrap-search-around
    (direction search-action &optional not-found-message)
  "Wrap SEARCH-ACTION in a buffer. SEARCH-ACTION should be a
function of 0 arguments that performs the search, moves the point
and returns a boolean, t when something was found and nil
otherwise. An example SEARCH-ACTION is `re-search-forward' with
some regexp.

DIRECTION must be a symbol, either 'forward or 'backward.
"
  (cl-assert (memq direction '(forward backward)))
  (let ((p (point)))
    (or (funcall search-action)
        (let ((bound
               (fold-direction direction
                 (point-min)
                 (point-max))))
          (goto-char bound)
          (if (funcall search-action)
              (progn
                (message "Wrapped at %s"
                         (fold-direction direction "bottom" "top"))
                t)
            (progn
              (message (or not-found-message "Nothing found"))
              (goto-char p)
              nil))))))

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

(defun notify (&rest args)
  "Like `message' but is silence in noninteractive mode."
  (unless noninteractive
    (apply #'message args)))

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
  (scroll-down 500))

(defun scroll-up-command-fast ()
  (interactive)
  (scroll-up 500))

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
"
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
            (`?n  (setf (aref str r) ?\n)
                  (setf i (+ i 2)))
            (`?r  (setf (aref str r) ?\r)
                  (setf i (+ i 2)))
            (`?t  (setf (aref str r) ?\t)
                  (setf i (+ i 2)))
            (`?\\ (setf (aref str r) ?\\)
                  (setf i (+ i 2)))
            (_    (setf (aref str r) c)
                  (setf i j)))
          (progn
            (setf (aref str r) c)
            (setf i j)))
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

;;

(provide 'common)

;; Local Variables:
;; End:

;; common.el ends here
