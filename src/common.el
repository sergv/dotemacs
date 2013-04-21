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

(defalias 'defconstant 'defconst)

(defvar *ex-commands-re-cache* nil
  "This variable contains optimized regexp that matches
currently defined ex commands. Should be updated with
`ex-commands-re-cache-update' when new ex commands being defined.")


(defun ex-commands-re-cache-update ()
  "Updates `*ex-commands-re-cache*' with current ex-commands."
  (setf *ex-commands-re-cache*
        (concat "\\("
                (regexp-opt (map #'car vim:ex-commands))
                "\\)\\(!\\)?")))



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
  (multiple-value-bind (a b microsec)
      (current-time)
    (let ((c (+ (* 65536 a) b)))
      (make-tausworthe-random-gen
       (+ (min c microsec) 2)
       (+ (emacs-pid) 8)
       (+ (max c microsec) 16)))))

(defvar *tausworthe-random-gen* (make-tausworthe-random-generator)
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
      (random-shuffle lines *tausworthe-random-gen*)
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

(defalias 'paths-concat 'path-concat)



(defun* get-directory-contents (dir &key (full t))
  (remove-if (lambda (x) (member (file-name-nondirectory x) '("." "..")))
             (directory-files dir full)))

(defun version-control-directory? (filename)
  (string-match-pure? (rx (or "SCCS"
                              "RCS"
                              "CVS"
                              "MCVS"
                              ".svn"
                              ".git"
                              ".hg"
                              ".bzr"
                              "_MTN"
                              "_darcs"
                              "{arch}")
                          (? "/"))
                      filename))

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
                ((and (file-directory-p path)
                      (not (funcall do-not-visitp path)))
                 (reduce (lambda (acc p)
                           (funcall collect-rec p acc))
                         (get-directory-contents path :full t)
                         :initial-value (if (funcall dirp path)
                                            (cons path accum)
                                            accum)))
                ((funcall filep path)
                 (cons path accum))
                (t
                 accum)))))
    (nreverse (funcall collect-rec path nil))))

(defun read-and-insert-filename ()
  "Read filename with completion from user and insert it at point.
Of course directory names are also supported."
  (interactive)
  (if (and (eq major-mode 'org-mode)
           (y-or-n-p "Insert link? "))
      (insert "[[file:"
              (expand-file-name (read-file-name "" nil ""))
              "][]]")
      (insert (expand-file-name (read-file-name "" nil "")))))

(defun find-filename-in-tree-recursive ()
  "Read filename regexp and try to find it in current dir's tree or in trees
obtained by following upward in filesystem"
  (interactive)
  (let* ((filename-re (read-string "filename regexp: " ""))
         (path (reverse (split-string (aif (buffer-file-name (current-buffer))
                                        (file-name-directory it)
                                        (expand-file-name default-directory))
                                      "/"
                                      t)))
         (subdirs-visited '())
         (found? nil)
         (files nil) ;; found files
         )
    (letrec ((path-join (lambda (path)
                          (concat "/" (join-lines (reverse path) "/")))))
      (while (and (not found?)
                  (not (null? path)))
        (let ((subdir (funcall path-join path)))
          (message "searching in %s" subdir)
          (setf files
                (find-rec subdir
                          :filep
                          (lambda (p)
                            (string-match-pure? filename-re
                                                (file-name-nondirectory p)))
                          :do-not-visitp
                          (lambda (p)
                            (or (version-control-directory?
                                 (file-name-nondirectory p))
                                (any? (lambda (subdir)
                                        (string-prefix? subdir p))
                                      subdirs-visited)))))
          (when (not (null? files))
            (setf found? t))
          (push subdir subdirs-visited)
          (setf path (cdr path))))
      (if found?
          (progn
            (assert (not (null? files)))
            (if (= 1 (length files))
                (find-file (car files))
                (select-start-selection files
                                        :on-selection
                                        (lambda (idx)
                                          (select-exit)
                                          (find-file (nth idx files)))
                                        :predisplay-function
                                        (lambda (x)
                                          (concat "file: " (file-name-nondirectory x) "\n"
                                                  x "\n"))
                                        :preamble-function
                                        (lambda ()
                                          "Multiple files found\n\n")
                                        :separator-function
                                        (lambda () ""))))
          (error "No file found for \"%s\" regexp" filename-re)))))

;;;;

(defun completing-read-vanilla (prompt
                                collection
                                &optional
                                predicate
                                require-match
                                initial-input
                                hist-m@%=!$+&^*z
                                def
                                inherit-input-method)
  "Perform `completing-read' of icicles using vanilla completion and remove any text
properties from the result."
  (let ((icicle-TAB-completion-methods '(vanilla))
        (icicle-hide-common-match-in-Completions-flag nil))
    (let ((result (completing-read prompt
                                   collection
                                   predicate
                                   require-match
                                   initial-input
                                   hist-m@%=!$+&^*z
                                   def
                                   inherit-input-method)))
      (set-text-properties 0 (length result) nil result)
      result)))



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

(defun re-group-matched? (n)
  "Return non-nil if Nth group matched."
  (match-beginning n))


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
            (cond ((memq c rubout-keys) ; rubout
                   (when (> (length pass) 0)
                     (let ((new-pass (substring pass 0 -1)))
                       (and (arrayp pass) (clear-string pass))
                       (setq pass new-pass))))
                  ((eq c ?\C-g) (keyboard-quit))
                  ((not (numberp c)))
                  ((= c ?\C-u)          ; kill line
                   (and (arrayp pass) (clear-string pass))
                   (setq pass ""))
                  ((or (= c ?\C-y)
                       (= c ?\M-y)
                       (= c ?\C-p)
                       (= c ?\M-p))     ; yank
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
                  ((characterp c)       ; insert char
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

;;;;; combinatorics

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


(defun split-at (n list)
  "Return two elements: list of first N elements of LIST and
LIST sans it's first N elements."
  (cond
    ((null list)
     (values nil nil))
    ((= n 0)
     (values nil list))
    (t
     (bind (((head tail) (split-at (1- n) (cdr list))))
           (values (cons (car list) head) tail)))))


(defun* sum (seq &key (key #'identity) (start 0) (end nil))
  (reduce #'+
          seq
          :initial-value 0
          :key key
          :start start
          :end end))


;;;;

(defun point-inside-string-or-comment? ()
  "Return t if point is positioned inside a string."
  (save-excursion
    (let* ((end (point))
           (begin (line-beginning-position)))
      (when begin
        (let ((state (parse-partial-sexp begin end)))
          (or (elt state 3)
              (elt state 4)))))))


;;;;

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

;;;;

(defun string-trim-whitespace (str)
  "Trim whitespaces from string."
  (let ((find-bound
         (lambda (from-end)
           (position-if-not (lambda (char) (member* char (string->list " \t\n\r\v\f")
                                                    :test #'char=))
                            str :from-end from-end))))
    (let ((start (funcall find-bound nil))
          (end (funcall find-bound t)))
      (if start
          (subseq str start (1+ end))
          ""))))

;;;;

(defun extract-unicode ()
  ;; note - every single bit of this function is made to let this function
  ;; work as fast as it can an as large buffers as possible
  ;; (on 2k lines performance is acceptable)
  (let ((filter*
         (lambda (pred items)
           (let ((result nil))
             (dolist (item items)
               (if (funcall pred item)
                   (push item result)))
             result))))
    (let ((chars (string->list (buffer-substring-no-properties (point-min)
                                                               (point-max)))))
      (sort (remove-duplicates (funcall filter* (lambda (c) (< 127 c)) chars)) '<))))

(defun input-unicode ()
  (interactive)
  (let* ((symbs (map 'char->string (extract-unicode)))
         (symb (completing-read-vanilla "> " symbs)))
    (remove-text-properties 0 (length symb) '(font-lock-face nil) symb)
    (insert symb)))

;;;;

(defvar *invisible-buffers* '()
  "List of buffer name regexps than should not be visible in e.g. ibuffer,
tabbar, etc")

(defun add-invisible-buffer (buf-re)
  (assert (string? buf-re))
  (add-to-list '*invisible-buffers* buf-re))

(defun invisible-buffer? (buf)
  "Returns t if buffer BUF should be regarded as invisible, see also
`*invisible-buffers*'."
  (cond ((or (string? buf)
             (buffer? buf))
         (string-match-pure? (join-lines *invisible-buffers* "\\|")
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
        (else
         (error "wrong argument type - not a string nor a buffer: %s"
                buf))))

(defun visible-buffers ()
  "Get list of buffers that are not invisible."
  (filter (lambda (buf)
            (not (invisible-buffer? buf)))
          (buffer-list)))

(add-invisible-buffer "^\\*Completions\\*$")
(add-invisible-buffer "^#.+#$")
(add-invisible-buffer "^\\*Ibuffer\\*$")
(add-invisible-buffer "^ .*$")

;;;;

(defun for-buffers-with-mode (mode func)
  (for-each func
            (filter (lambda (buf)
                      (with-current-buffer buf
                        (eq? major-mode mode)))
                    (buffer-list))))

;;;;

(defun hash-table->alist (table)
  "Translate hash table into alist of (<key> . <value>) pairs."
  (let ((result '()))
    (maphash (lambda (k v) (push (cons k v) result)) table)
    result))

;;;;

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

;;;;

(defun bisect (item items start end eq? less?)
  "Binary search. Returns index into vector ITEMS.
LESS? is predicate on elements of ITEMS."
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
    (if (and (<= orig-start start)
             (< start orig-end))
        start
        nil)))

;;;;

(defun string-suffix? (string1 string2 &optional ignore-case)
  "Return t if STRING1 is a suffix of STRING2."
  (and (<= (length string1) (length string2))
       (eq t (compare-strings string1 0 nil
                              string2 (- (length string2) (length string1)) nil
                              ignore-case))))

;;;;

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

;;;;

(defun merge-emacs-configs (new-config-dir curr-config-dir)
  "Merge changes from NEW-CONFIG-DIR into CURR-CONFIG-DIR by successively calling
ediff on files that differ and saving files in CURR-CONFIG-DIR that were updated
while executing ediff.

Use like this to pick changes that will go into CURR-CONFIG-DIR:
\(merge-emacs-configs \"/home/sergey/emacs.new\" \"/home/sergey/emacs\"\)."
  (setf new-config-dir (strip-trailing-slash new-config-dir)
        curr-config-dir (strip-trailing-slash curr-config-dir))
  (let ((ignored-files-re (concat "^.*"
                                  (regexp-opt *ignored-file-name-endings*)
                                  "$"))
        (ignored-dirs-re (concat "\\(?:^\\|/\\)"
                                 (regexp-opt *ignored-directories*)
                                 "/.*$")))
    (dolist (p (map (lambda (p)
                      (file-relative-name p new-config-dir))
                    (find-rec new-config-dir
                              :filep
                              (lambda (p)
                                (let ((fname (file-name-nondirectory p)))
                                  (and (or (string-match-pure? "^.*\\.el$"
                                                               fname)
                                           (string-match-pure? "^.*/?scripts/.*$"
                                                               p))
                                       ;; emacs locks?
                                       (not (string-match-pure? "^\\.#.*"
                                                                fname))
                                       ;; various binary files
                                       (not (string-match-pure? ignored-files-re
                                                                fname))
                                       (not (string-match-pure? ignored-dirs-re
                                                                fname))))))))
      (let* ((new  (concat new-config-dir "/" p))
             (curr (concat curr-config-dir "/" p)))
        (message "Files %s and %s" new curr)
        (condition-case err
            (progn
              (assert (file-exist? new))
              (if (file-exist? curr)
                (if (different-files-fast? new curr)
                  (let ((new-buf  (find-file-noselect new))
                        (curr-buf (find-file-noselect curr)))
                    (ediff-diff-files-recursive-edit new curr :read-only nil)
                    (kill-buffer new-buf)
                    (with-current-buffer curr-buf
                      (save-buffer))
                    (redisplay t))
                  (progn
                    (message "Files %s and %s are the same, skipping" new curr)))
                (when (y-or-n? (format "Copy %s to %s?" new curr))
                  (copy-file new curr nil t t t))))
          (error
           (message "Error occurred while processing files %s and %s:\n%s"
                    new
                    curr
                    err)))))))

(defun merge-emacs-configs-default ()
  "Merge from +emacs-config-path+/tmp/emacs into `+emacs-config-path+'."
  (interactive)
  (let ((current-conf-dir +emacs-config-path+)
        (new-conf-dir (concat +emacs-config-path+ "/tmp/emacs")))
    (assert (file-directory? current-conf-dir))
    (if (not (file-directory? new-conf-dir))
      (error "Config under %s not found" new-conf-dir)
      (merge-emacs-configs new-conf-dir current-conf-dir))))

;;;;

(defun remove-tabs (start end)
  "Replace all tab characters in region between START and END with
number of spaces equal to `tab-width'."
  (interactive "r")
  (save-excursion
    (save-match-data
      (goto-char end)
      (save-excursion
        (unless (re-search-backward "\t" start t)
          (error "No tabs found")))
      (let ((str (make-string tab-width ?\s)))
        (while (re-search-backward "\t" start t)
          (replace-match str))))))

;;;;

(defconst +buffer-indent-temporary-filename+
  (concat temporary-file-directory "/indent-buffer.tmp")
  "Path to temporary file reserved for buffer indentation puproses.
See also `*mode-buffer-indent-function-alist*'.")

(defvar *mode-buffer-indent-function-alist* nil
  "Alist of (major-mode . function) pairs, where functions should take 0
arguments and indent current buffer. See also `+buffer-indent-temporary-filename+'.")

;;;;

(defun to-linux-line-endings ()
  "Convert line endings in current buffer to linux ones (\\n)."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix nil))

;;;;

(defun insert-my-formatted-date ()
  "Insert today's date as \"<Day Name>, <day> <Month name> <Year>\""
  (interactive)
  (insert (format-time-string "%A, %e %B %Y" (current-time))))

;;;;

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

;;;;

(defun join-lines (lines &optional str)
  "Join list of strings with given STR that defaults to newline."
  (mapconcat #'identity lines (or str "\n")))

(defun* split-into-lines (str &optional (omit-nulls t))
  "Split string into list of lines."
  (split-string str "\n" omit-nulls))

(defun map (func xs &rest sequences)
  (if (null? sequences)
      (mapcar func xs)
      (apply cl-mapcar func xs sequences)))

(defun foldr (f init items)
  "F should take two arguments (item accum)."
  (cl-reduce f
             items
             :from-end t
             :initial-value init))

(defun foldl (f init items)
  "F should take two arguments (accum item)."
  (cl-reduce f
             items
             :initial-value init))

;;;;

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
        (else
         (error "Not implemented yet"))))

;;;;

(defun indent-whole-buffer ()
  "Indent whole buffer with `indent-region'."
  (indent-region (point-min) (point-max)))

;;;;

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
            (else
             (setf a (rest a)
                   b (rest b)))))
    result))

(defsubst list= (a b)
  "Check whether list of integers A is equal to integer list B."
  (equal a b))

;;;;

(defun text-between-lines (start-line end-line)
  "Return string of text with properties between beginning of START-LINE and
end of END-LINE in current buffer."
  (save-excursion
    (buffer-substring (progn (goto-line1 start-line)
                             (line-beginning-position))
                      (progn (goto-line1 end-line)
                             (line-end-position)))))

;;;;

(defvar *ignored-file-name-endings*
  '(".annot" ".cmi" ".cmxa" ".cma" ".cmx" ".cmo" ".o" "~" ".bin" ".out" ".exe" ".prof" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".hi")
  "List of file name endings to generally ignore.")


(defvar *ignored-directories*
  '(".svn" ".hg" ".git" ".bzr" "CVS" "_darcs" "_MTN")
  "List of directory names to generally ignore.")

(setf completion-ignored-extensions
      (append (map (lambda (x) (concat x "/")) *ignored-directories*)
              *ignored-file-name-endings*))

;;;;

(provide 'common)

;; Local Variables:
;; End:


;; common.el ends here
