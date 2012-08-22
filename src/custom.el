
;; (eval-when-compile
;;  (require 'cl))

(require 'cl)

(defmacro if-buffer-has-file (&rest body)
  "Execute BODY if current buffer is assigned to file"
  (declare (indent defun))
  `(when (buffer-file-name)
     ,@body))

(defmacro if-has-makefile-command (&rest body)
  "Execute BODY if current file is listed in some makefile
in the same directory the current file is."
  `(if-buffer-has-file
    (let* ((fname (file-name-nondirectory buffer-file-name))
           (fname-re (concat "\\<" fname)))
      (when (some #'(lambda (makefile)
                      (file-contents-matches-re makefile fname-re))
                  '("makefile" "Makefile" "MAKEFILE"))
        ,@body))))

(defmacro defvar-buffer-local (var &optional default)
  `(progn
     (defvar ,var)
     (make-variable-buffer-local ',var)
     (set-default ',var ,default)))

(defmacro* defvar-loc (var
                       &optional
                       (default nil)
                       (doc "Defined using `defvar-loc'"))
  "Just like `defvar' but makes VAR buffer-local."
  `(progn
     (defvar ,var nil
       ,doc)
     (make-variable-buffer-local ',var)
     (set-default ',var ,default)))

;; doc string highlighting
(put 'defvar-loc 'doc-string-elt 3)
(font-lock-add-keywords 'emacs-lisp-mode '("defvar-loc"))



(defmacro def-keys-for-map (mode-map key-command-list)
  `(def-keys-for-map1 ,mode-map
       ,(if (symbolp key-command-list)
            key-command-list
            (loop
                for ((kbd key) command) in (eval key-command-list)
                collect (list (eval key) command)))))

(defun current-column ()
  "Return current column - integer number."
  (- (point) (line-beginning-position)))

(defun quoted? (x)
  (eq 'quote (car-safe x)))

(defmacro def-keys-for-map1 (mode-map key-command-list)
  (declare (indent nil))
  (labels ((def-key (map key command)
             `(define-key ,map
                  ,(eval `(kbd ,key))
                ,(cond
                  ((and (listp command)
                        (or (eq 'function (car command))
                            (eq 'quote (car command))))
                   command)
                  ((and (listp command)
                        (eq 'lambda (car command)))
                   (list 'function command))
                  (t
                   (list 'quote command))))))
    (let ((bindings
           (loop
               for map in (cond
                            ((quoted? mode-map)
                             (eval mode-map))
                            ((listp mode-map)
                             mode-map)
                            (t (list mode-map)))
               append
               (loop
                   for (key command) in (if (or (quoted? key-command-list)
                                                (symbolp key-command-list))
                                            (eval key-command-list)
                                            key-command-list)
                   collect (def-key map key command)))))
      (unless bindings
        (error "No keys bound for %S using following key-command-list %S"
               mode-map
               key-command-list))
      `(prog1 nil
         ,@bindings))))

(defmacro def-keys-for-map2 (mode-map &rest key-command-list)
  (declare (indent nil))
  `(def-keys-for-map1 ,mode-map ,key-command-list))

(defun remove-buffer (&optional buffer-or-name)
  "Remove buffer completely bypassing all its prompt functions.
Save buffer if it has assigned file and this file exists on disk."
  (interactive)
  (let ((old-functions kill-buffer-query-functions)
        (kill-buffer-query-functions nil))
    (if-buffer-has-file
     (when (file-existp (buffer-file-name))
       (save-buffer)))
    (kill-buffer buffer-or-name)
    (setq kill-buffer-query-functions old-functions)))

(defun remove-buffer-and-window ()
  "Remove buffer and close it's window"
  (interactive)
  (remove-buffer)
  (delete-window))

(defun make-script-file-exec ()
  "Make buffer file executable if it's a shell script."
  (and (not (file-executable-p buffer-file-name))
       (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (looking-at-p "^#!")))
       (shell-command (concat "chmod u+x \"" buffer-file-name "\""))
       (message
        (concat "Saved as script: " buffer-file-name))))

(defun dired-single-up-directory ()
  (interactive)
  (dired-single-buffer ".."))


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


(defsubst whitespace-char-p (char)
  (or (char= char ?\s)
      (char= char ?\n)
      (char= char ?\t)))

(defalias 'whitespace-charp 'whitespace-char-p)

(defun delete-whitespaces-forward ()
  "Delete whitespaces forward until non-whitespace
character found"
  (interactive)
  (while (and (not (eobp))
              (whitespace-char-p (char-after)))
    (delete-char 1)))

(defun delete-whitespaces-backward ()
  "Delete whitespaces backward until non-whitespace
character found"
  (interactive)
  (while (and (not (bobp))
              (whitespace-char-p (char-before)))
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

(defsubst skip-to-indentation ()
  "Move point to first non-whitespace character of line,
lighter than `back-to-indentation'."
  (beginning-of-line nil)
  (skip-syntax-forward " " (line-end-position))
  (backward-prefix-chars))

(defsubst count-lines1 (begin end)
  "Return line count in region like `count-lines' but don't
confuse when point is not at the beginning of line"
  (+ (count-lines begin end)
     (if (equal (current-column) 0)
         1
         0)))

(defsubst backward-line (count)
  "Call `forward-line' in the opposite direction"
  (forward-line (- count)))



(defun trim-whitespaces (str)
  "Trim leading and tailing whitespace from STR."
  (when str
    (save-match-data
      (let ((s (if (symbolp str) (symbol-name str) str)))
        (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))))


(defsubst goto-line1 (line)
  "Set point at the beginning of line LINE counting from line 1 at
beginning of buffer. Dont causes Scan error: \"Unbalanced parentheses\" as
in `goto-line'"
  (goto-char (point-min))
  (forward-line (1- line)))


(defun dired-prompt-and-do-query-replace-regexp (re str)
  (interactive "Mregexp: \nMreplacement string: ")
  (dired-do-query-replace-regexp re str))


(defun util:pwd (&optional insert)
  "If called without prefix argument then show current
working directory, otherwise insert absolute path to
current working directory at point."
  (interactive (list current-prefix-arg))
  (let ((dir (expand-file-name default-directory)))
    (if insert
        (insert dir)
        (message "Directory %s" dir))))

;; abandon old and non-flexible pwd function
;; (fset 'pwd 'util:pwd)

(defun file-contents-matches-re (filename re)
  "Return t if file FILENAME exists and it contents matches RE."
  (when (file-exists-p filename)
    (save-match-data
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (when (search-forward-regexp re nil t)
          t)))))

(defmacro run-if-fbound (func)
  `(and (fboundp (quote ,func))
        (,func)))


(defun util:flatten (xs)
  "Transform list XS that possibly consists of nested list
into flat list"
  (if (listp xs)
      (mapcan (lambda (x) (util:flatten x)) xs)
      (list xs)))

;;;;

(defalias 'file-exist-p 'file-exists-p)
(defalias 'file-existp 'file-exists-p)



;; Originally from stevey, adapted to support moving to a new directory.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (let ((fname (buffer-file-name)))
     (unless fname
       (error "Buffer '%s' is not visiting a file" (buffer-name)))
     (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                     fname))))))
  (when (equal new-name "")
    (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                   (expand-file-name (file-name-nondirectory
                                      (buffer-file-name))
                                     new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (when (file-exists-p (buffer-file-name))
    (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
      (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
    (message "Renamed to %s" new-name)))

(defun copy-file-and-open (new-name)
  "Copy curretn file to NEW-NAME and open it."
  (interactive
   (let ((fname (buffer-file-name)))
     (unless fname
       (error "Buffer '%s' is not visiting a file" (buffer-name)))
     (list (read-file-name (format "Copy %s to: " (file-name-nondirectory
                                                   fname))))))
  (when (equal new-name "")
    (error "Aborted copy"))
  (setf new-name (if (file-directory-p new-name)
                   (expand-file-name (file-name-nondirectory
                                      (buffer-file-name))
                                     new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (when (file-exists-p (buffer-file-name))
    (copy-file (buffer-file-name) new-name 1 nil t t)
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

;;;; compilation info facility

(defvar *compile-caller-info* nil
  "Alist containing information about buffer, major mode etc.
from where current compile command was invoked. Should be cleared
up by functions in compilation-finish-functions.")

(defadvice compilation-start (before
                              compilation-start-store-info
                              activate
                              compile)
  "Record information about caller of compile command into
`*compile-caller-info*'"
  (setq *compile-caller-info* `((mode . ,major-mode)
                                (compile-command . ,compile-command)
                                (buffer . ,(current-buffer)))))

;;;;

(defun start-nautilus ()
  "Start nautilus in folder associated with current buffer."
  (interactive)
  (if (buffer-file-name)
    (async-shell-command (concat "nautilus "
                                 (shell-quote-argument
                                  (file-name-directory (buffer-file-name))))
                         nil
                         nil)
    (async-shell-command (concat "nautilus "
                                 (shell-quote-argument
                                  default-directory))
                         nil
                         nil)))

(defalias 'nautilus 'start-nautilus)
(defalias 'run-nautilus 'start-nautilus)
(defalias 'open-nautilus 'start-nautilus)

;;;; rotate list functions, very old...

(defun rotate-entry-list (listvar)
  "Rotate list of any etries such that list '(X Y Z) becomes '(Y Z X)"
  (set listvar (let ((value (symbol-value listvar)))
                 (cond ((null value) nil)
                       ((equal (length value) 1) value)
                       (t (let ((new-list (cdr value)))
                            (setcdr value nil)
                            (nconc new-list value)
                            new-list))))))

(defun rotate-entry-list-backward (listvar)
  "Rotate list of any etries such that list '(X Y Z) becomes '(Z X Y)"
  (set listvar (let ((value (symbol-value listvar)))
                 (cond ((null value) nil)
                       ((equal (length value) 1) value)
                       (t (while (cddr value)
                            (setq value (cdr value)))
                          (let ((last-elem (cdr value)))
                            (setcdr last-elem (symbol-value listvar))
                            (setcdr value nil)
                            last-elem))))))


;;;;

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

;;;;

(defun delete-trailing-whitespace+ ()
  "This function removes spaces and tabs on every line after
last non-whitespace character."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
        (replace-match "")))))

;;;; tabbar stuff

(defun swap-elements (i j xs)
  "Swaps elements at positions I and J in list XS. Returns new list."
  (if (or (null xs)
          (= 1 (length xs))
          (= i j))
      xs
      (if (< j i)
          (swap-elements j i xs)
          (when (and (< i (length xs))
                     (< j (length xs)))
              (nconc (subseq xs 0 i)
                     (list (nth j xs))
                     (subseq xs (1+ i) j)
                     (list (nth i xs))
                     (subseq xs (1+ j)))))))

(defun init (xs)
  "Return all but last elements of XS."
  (nreverse (cdr-safe (reverse xs))))

(defun move-element-left (i xs)
  "Moves element on Ith position in list XS to I-1'th position, or
appends it to XS tail if I = 0."
  (if (>= i (length xs))
      nil
      (if (= i 0)
          (append (cdr xs)
                  (list (car xs)))
          (swap-elements (1- i) i xs))))

(defun move-element-right (i xs)
  "Moves element on Ith position in list XS to I+1'th position, or
appends it to XS head if I = (length XS) - 1."
  (if (>= i (length xs))
      nil
      (if (= i (1- (length xs)))
          (cons (car (last xs))
                (init xs))
          (message "false")
          (swap-elements (1+ i) i xs))))

(defun tabbar-move-selected-tab-left ()
  (interactive)
  (let* ((tabset    (tabbar-current-tabset t))
         (selected  (tabbar-selected-tab tabset))
         (tabs      (tabbar-tabs tabset))
         (len       (1- (length tabs)))
         (sel-index (position selected tabs))
         (tabs1     (move-element-left sel-index tabs)))
    (set tabset tabs1)
    (tabbar-set-template tabset nil)
    (tabbar-click-on-tab selected)))

(defun tabbar-move-selected-tab-right ()
  (interactive)
  (let* ((tabset    (tabbar-current-tabset t))
         (selected  (tabbar-selected-tab tabset))
         (tabs      (tabbar-tabs tabset))
         (len       (1- (length tabs)))
         (sel-index (position selected tabs))
         (tabs1     (move-element-right sel-index tabs)))
    (set tabset tabs1)
    (tabbar-set-template tabset nil)
    (tabbar-click-on-tab selected)))




(defun vector-member (elem vec)
  (let ((found nil)
        (i 0))
    (while (and (< i (length vec))
                (not found))
      (setq found (equal (aref vec i) elem)
            i (1+ i)))
    found))



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

(defun* move-by-char (direction &optional (count 1))
  (forward-char (* count (direction-to-num direction))))

(defun* move-by-char-backward (direction &optional (count 1))
  (backward-char (* count (direction-to-num direction))))

(defsubst char= (a b)
  (char-equal a b))

(require 'custom-predicates)

;; Local Variables:
;; lexical-binding: nil
;; End:

(provide 'custom)
