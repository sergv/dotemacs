;; common.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  5 November 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'custom)
(require 'custom-predicates)
(require 'more-scheme)
(require 'macro-util)

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

(defun* find-rec (path
                  &key
                  (filep (lambda (p) t))
                  (dirp  (lambda (p) nil))
                  (do-not-visitp
                   (lambda (p)
                     (version-control-directory?
                      (file-name-nondirectory p)))))
  "Collect files and/or directories under PATH recursively.

Collect files and directories which satisfy FILEP and
DIRP respectively in directories which don't satisfy DO-NOT-VISITP.
By default, version-control specific directories are omitted, e.g. .git etc.

All predicates are called with full absolute paths."
  (when (stringp filep)
    (setf filep
          (let ((regexp-local filep))
            (lambda (p) (string-match-p regexp-local p)))))
  (when (stringp dirp)
    (setf dirp
          (let ((regexp-local dirp))
            (lambda (p) (string-match-p regexp-local p)))))
  (when (stringp do-not-visitp)
    (setf do-not-visitp
          (let ((regexp-local do-not-visitp))
            (lambda (p) (string-match-p regexp-local p)))))

  (letrec ((collect-rec
            (lambda (path accum)
              (cond
                ((and (not (funcall do-not-visitp path))
                      (file-directory-p path))
                 (let ((result (if (funcall dirp path)
                                 (cons path accum)
                                 accum)))
                   (dolist (entry (directory-files path
                                                   t ;; produce full names
                                                   directory-files-no-dot-files-regexp
                                                   t ;; don't sort
                                                   ))
                     (cond
                       ((file-directory-p entry)
                        (setf result (funcall collect-rec entry result)))
                       ((funcall filep entry)
                        (push entry result))))
                   result))
                ((funcall filep path)
                 (cons path accum))
                (t
                 accum)))))
    (funcall collect-rec path nil)))

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
                         (ido-read-file-name "" nil ""))))
         (output (if (and (eq major-mode 'org-mode)
                          (y-or-n-p "Insert link? "))
                   (concat "[[file:"
                           path
                           "][]]")
                   path)))
    (insert output)))

(autoload 'find-filename-in-tree-recursive "common-heavy" "" t)

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
                (:eval default-directory))))))

(defun re-group-matchedp (n)
  "Return non-nil if Nth group matched."
  (match-beginning n))

(defalias 're-group-matched? #'re-group-matchedp)


(defun constantly (x)
  (let ((tmp x))
    (lambda (&rest y)
      (declare (ignore y))
      tmp)))


(redefun read-passwd (prompt &optional confirm default)
  "Read a password, prompting with PROMPT, and return it.
If optional CONFIRM is non-nil, read the password twice to make sure.
Optional DEFAULT is a default password to use instead of empty input.

This function echoes `.' for each character that the user types.

The user ends with RET, LFD, or ESC.  DEL or C-h rubs out.
C-y yanks the current kill.  C-u kills line.
C-g quits; if `inhibit-quit' was non-nil around this function,
then it returns nil if the user types C-g, but `quit-flag' remains set.

Once the caller uses the password, it can erase the password
by doing (clear-string STRING)."
  (with-local-quit
    (if confirm
      (let (success)
        (while (not success)
          (let ((first (read-passwd prompt nil default))
                (second (read-passwd "Confirm password: " nil default)))
            (if (equal first second)
              (progn
                (and (arrayp second) (clear-string second))
                (setq success first))
              (and (arrayp first) (clear-string first))
              (and (arrayp second) (clear-string second))
              (message "Password not repeated accurately; please start over")
              (sit-for 1))))
        success)
      (let ((pass nil)
            ;; Copy it so that add-text-properties won't modify
            ;; the object that was passed in by the caller.
            (prompt (copy-sequence prompt))
            (c 0)
            (echo-keystrokes 0)
            (cursor-in-echo-area t)
            (message-log-max nil)
            (stop-keys (list 'return ?\r ?\n ?\e))
            (rubout-keys (list 'backspace ?\b ?\177)))
        (add-text-properties 0 (length prompt)
                             minibuffer-prompt-properties prompt)
        (while (progn (message "%s%s"
                               prompt
                               (make-string (length pass) ?.))
                      (setq c (read-key))
                      (not (memq c stop-keys)))
          (clear-this-command-keys)
          (cond ((memq c rubout-keys)   ; rubout
                 (when (> (length pass) 0)
                   (let ((new-pass (substring pass 0 -1)))
                     (and (arrayp pass) (clear-string pass))
                     (setq pass new-pass))))
                ((eq c ?\C-g) (keyboard-quit))
                ((not (numberp c)))
                ((= c ?\C-u)            ; kill line
                 (and (arrayp pass) (clear-string pass))
                 (setq pass ""))
                ((or (= c ?\C-y)
                     (= c ?\M-y)
                     (= c ?\C-p)
                     (= c ?\M-p))       ; yank
                 (let* ((str (condition-case nil
                                 (current-kill 0)
                               (error nil)))
                        new-pass)
                   (when str
                     (setq new-pass
                           (concat pass
                                   (substring-no-properties str)))
                     (and (arrayp pass) (clear-string pass))
                     (setq c ?\0)
                     (setq pass new-pass))))
                ((characterp c)         ; insert char
                 (let* ((new-char (char-to-string c))
                        (new-pass (concat pass new-char)))
                   (and (arrayp pass) (clear-string pass))
                   (clear-string new-char)
                   (setq c ?\0)
                   (setq pass new-pass)))))
        (message nil)
        (or pass default "")))))

;;;; some ring functions

(defun* ring-delete-all (item ring &key (test #'equal))
  "Remove all elements of RING for which TEST invoked with ITEM returns t."
  ;; elements should be removed starting from the end
  (dotimes (i (ring-length ring))
    (let ((j (- (ring-length ring) (1+ i))))
      (when (funcall test item (ring-ref ring j))
        (ring-remove ring j)))))

(defun* ring-member (item ring &key (test #'equal))
  (let ((i 0)
        (member nil))
    (while (and (< i (ring-length ring))
                (not member))
      (setf member (funcall test
                            item
                            (ring-ref ring i)))
      (incf i))
    member))

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
    (mapcan (lambda (item)
              (map (lambda (x)
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
                  (map (lambda (rest)
                         (cons i rest))
                       (funcall collect (1- start) (1- i))))))))
    (funcall collect (1- k) (1- n))))

(defun* sum (seq &key (key #'identity) (start 0) (end nil))
  (reduce #'+
          seq
          :initial-value 0
          :key key
          :start start
          :end end))

;;;

(defun point-inside-string-or-comment? ()
  "Return t if point is positioned inside a string."
  (save-excursion
    (let* ((end (point))
           (begin (line-beginning-position)))
      (when begin
        (let ((state (parse-partial-sexp begin end)))
          (or (elt state 3)
              (elt state 4)))))))

(defun point-not-inside-string-or-comment? ()
  (not (point-inside-string-or-comment?)))

;;;

(defun assoc-value (key alist)
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

(autoload 'extract-unicode "common-heavy")
(autoload 'input-unicode "common-heavy" "" t)

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
        ;;  (any? (lambda (re)
        ;;          (string-match-pure? re buf))
        ;;        *invisible-buffers*))
        ;; ((buffer? buf)
        ;;  (any? (lambda (re)
        ;;          (string-match-pure? re (buffer-name buf)))
        ;;        *invisible-buffers*))
        (t
         (error "wrong argument type - not a string nor a buffer: %s"
                buf))))

(defun visible-buffers ()
  "Get list of buffers that are not invisible."
  (filter (lambda (buf)
            (not (invisible-buffer? buf)))
          (buffer-list)))

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
  (mapc func
        (filter (lambda (buf)
                  (with-current-buffer buf
                    (eq? major-mode mode)))
                (buffer-list))))

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

(autoload 'merge-emacs-configs "common-heavy")
(autoload 'merge-emacs-configs-default "common-heavy" "" t)

;;;

(autoload 'remove-tabs "common-heavy" "" t)

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

(defmacro map (func xs &rest sequences)
  (if (null? sequences)
    `(mapcar ,func ,xs)
    `(apply #'cl-mapcar ,func ,xs ,@sequences)))

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

(defun sep-by (sep items)
  "Place SEP betweet elements in ITEMS list."
  (assert (list? items))
  (when items
    (let* ((res (cons nil nil))
           (tmp res))
      (while items
        (setf (car tmp) (car items))
        (when (cdr items)
          (setf (cdr tmp) (cons nil nil)
                tmp (cdr tmp)
                (car tmp) sep
                (cdr tmp) (cons nil nil)
                tmp (cdr tmp)))
        (setf items (cdr items)))
      res)))

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
         (ring-member item sequence :test test))
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
   '(".annot" ".cmi" ".cmxa" ".cma" ".cmx" ".cmo" ".o" ".hi" ".p_o" ".p_hi" ".prof_o" ".prof_hi" "~" ".bin" ".out" ".lbin" ".a" ".elc" ".glo" ".idx" ".lot" ".class" ".fasl" ".lo" ".la" ".gmo" ".mo" ".bbl" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".dex" ".gz" ".tar.gz" ".tar" ".bz2" ".tar.bz2" ".xz" ".tar.xz" ".7z" ".tar.7z")
   (cond
     ((platform-os-type? 'linux)
      '(".so" ))
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
  '(".cabal-sandbox")
  "List of directory names to generally ignore as a prefixes.")

(setf completion-ignored-extensions
      (append (map (lambda (x) (concat x "/")) *ignored-directories*)
              *ignored-file-name-endings*)
      grep-find-ignored-files
      (append (list ".#*"
                    "*.exe"
                    "*.prof")
              (map (lambda (x)
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
optimization reasons.")

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
              (begin
                (skip-chars-forward "^ \t" end)
                (skip-chars-forward " \t" end))
              (begin
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
                                (filter #'buffer-live-p
                                        (window-parameter window 'prev-buffers))))))

(defun switch-to-prev-buffer-in-window ()
  "Switch to previous alive buffer for selected window, if there's one."
  (interactive)
  (let* ((window (selected-window))
         (prev-bufs (filter #'buffer-live-p
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
  (if (and (run-if-fbound vim:visual-mode-p)
           (not (eq? vim:visual-mode-type 'block)))
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
       (error "Invalid vim:visual-mode-type: %s" vim:visual-mode-type)))
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
  "Swap buffers in window WIN-A and WIN-B."
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
      (begin
        (with-selected-window win-a
          (with-current-buffer buf-a
            (goto-char pos-b)))
        (select-window win-b)
        (with-selected-window win-b
          (with-current-buffer buf-b
            (goto-char pos-a))))
      (begin
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

(defun swap-buffers-forward-through-frame ()
  "Swap current buffer with selected buffer in the next frame."
  (interactive)
  (let* ((curr-win (selected-window))
         (next-frame (next-frame nil
                                 nil ;; exclude minibuffer-only frames
                                 ))
         (next-win (with-selected-frame next-frame
                     (selected-window))))
    (swap-buffers-in-windows curr-win next-win)))

(defun swap-buffers-backward-through-frames ()
  "Swap current buffer with selected buffer in the previous frame."
  (interactive)
  (let* ((curr-win (selected-window))
         (prev-frame (previous-frame nil
                                     nil ;; exclude minibuffer-only frames
                                     ))
         (prev-win (with-selected-frame prev-frame
                     (selected-window))))
    (swap-buffers-in-windows curr-win prev-win)))

(defun next-f (n)
  "Switch to Nth previous frame."
  (interactive "p")
  (other-frame n))

(defun prev-f (n)
  "Switch to Nth previous frame."
  (interactive "p")
  (other-frame (- n)))


(autoload 'start-file-manager "common-heavy" "" t)
(autoload 'start-terminal-emulator "common-heavy" "" t)

(defalias 'run-file-manager 'start-file-manager)

(defalias 'run-terminal-emulator 'start-terminal-emulator)
(defalias 'run-terminal 'start-terminal-emulator)

(autoload 'remove-duplicates-from-sorted-list-by "common-heavy" "" nil)

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
         (bytes-used (sum (map entry->bytes stats))))
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

(provide 'common)

;; Local Variables:
;; End:


;; common.el ends here
