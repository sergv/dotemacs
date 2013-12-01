;; custom.el --- -*- lexical-binding: nil; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago (since august inception)
;; Description:

(eval-when-compile (require 'cl-lib))
(require 'macro-util)

(defun current-column ()
  "Return current column - integer number."
  (- (point) (line-beginning-position)))

(defun quoted? (x)
  (eq 'quote (car-safe x)))

(defun remove-buffer (&optional buffer-or-name)
  "Remove buffer completely bypassing all its prompt functions.
Save buffer if it has assigned file and this file exists on disk."
  (interactive)
  (let ((old-functions kill-buffer-query-functions)
        (kill-buffer-query-functions nil))
    (if-buffer-has-file
      (when (file-exists? (buffer-file-name))
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

(defun delete-whitespace-forward ()
  "Delete whitespaces forward until non-whitespace
character found"
  (interactive)
  (while (and (not (eobp))
              (whitespace-char-p (char-after)))
    (delete-char 1)))

(defun delete-whitespace-backward ()
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

(defsubst backward-line (&optional count)
  "Call `forward-line' in the opposite direction"
  (forward-line (- (or count 1))))



(defun trim-whitespace (str)
  "Trim leading and tailing whitespace from STR."
  (when str
    (save-match-data
      (let ((s (if (symbolp str) (symbol-name str) str)))
        (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))))

(defun trim-whitespace-left (str)
  "Trim leading whitespace from STR."
  (when str
    (save-match-data
      (let ((s (if (symbolp str) (symbol-name str) str)))
        (replace-regexp-in-string "^[[:space:]\n]*" "" s)))))

(defun trim-whitespace-right (str)
  "Trim tailing whitespace from STR."
  (when str
    (save-match-data
      (let ((s (if (symbolp str) (symbol-name str) str)))
        (replace-regexp-in-string "[[:space:]\n]*$" "" s)))))


(defsubst goto-line1 (line)
  "Set point at the beginning of line LINE counting from line 1 at
beginning of buffer. Does not cause \"Scan error: \"Unbalanced parentheses\"\" as
`goto-line' does."
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
  (when (file-exists? filename)
    (save-match-data
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (when (search-forward-regexp re nil t)
          t)))))


(defun util:flatten (xs)
  "Transform list XS that possibly consists of nested list
into flat list"
  (if (listp xs)
    (mapcan (lambda (x) (util:flatten x)) xs)
    (list xs)))

;;;;

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

;;;;

(defvar custom/exec-with-directory-runners
  (let ((tbl (make-hash-table :test #'equal))
        (standard-starter
         (lambda (exec dir)
           (async-shell-command (join-lines (list exec
                                                  (shell-quote-argument dir))
                                            " ")))))
    (puthash "thunar" standard-starter tbl)
    (puthash "nautilus" standard-starter tbl)
    (puthash "exo-open"
             (lambda (exec dir)
               (shell-command (join-lines (list exec
                                                "--launch"
                                                "TerminalEmulator"
                                                "--working-directory"
                                                (shell-quote-argument dir))
                                          " ")))
             tbl)
    (puthash "konsole"
             (lambda (exec dir)
               (async-shell-command (join-lines (list exec
                                                      "--workdir"
                                                      (shell-quote-argument dir))
                                                " ")))
             tbl)
    (puthash "xfce4-terminal"
             (lambda (exec dir)
               (async-shell-command (join-lines (list exec
                                                      "--default-working-directory"
                                                      (shell-quote-argument dir))
                                                " ")))
             tbl)

    tbl)
  "Definitions of various executables that can be started in particular folder.")

(defun custom/run-first-matching-exec (execs)
  (assert (all? (lambda (exec)
                  (not (null? (gethash exec custom/exec-with-directory-runners))))
                execs))
  (let ((dir (expand-file-name
              (if (buffer-file-name)
                (file-name-directory (buffer-file-name))
                default-directory))))
    (letrec ((iter
              (lambda (execs)
                (when (not (null? execs))
                  (aif (executable-find (car execs))
                    (funcall (gethash (car execs)
                                      custom/exec-with-directory-runners)
                             it
                             dir)
                    (funcall iter (cdr execs)))))))
      (funcall iter execs))))

(defun start-file-manager ()
  "Start suitable file manager in folder associated with current buffer."
  (interactive)
  (custom/run-first-matching-exec
   (append (when (platform-os-type? 'windows)
             '("explorer"))
           '("thunar" "nautilus"))))

(defalias 'open-file-manager 'start-file-manager)
(defalias 'run-file-manager 'start-file-manager)
(defalias 'file-manager 'start-file-manager)
(defalias 'thunar 'start-file-manager)
(defalias 'nautilus 'start-file-manager)

(defun start-terminal-emulator ()
  "Start suitable terminal emulator in folder associated with current buffer."
  (interactive)
  (custom/run-first-matching-exec '("xfce4-terminal"
                                    "exo-open"
                                    "konsole"
                                    ;; "gnome-terminal"
                                    )))

(defalias 'run-terminal-emulator 'start-terminal-emulator)

;;;; rotate list functions, very old...

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

(defun delete-trailing-whitespace+ ()
  "This function removes spaces and tabs on every line after
last non-whitespace character."
  (unless inhibit-delete-trailing-whitespace
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" nil t)
          (replace-match ""))))))

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

(defsubst char= (a b)
  (char-equal a b))

(defun cadr-safe (x)
  (car-safe (cdr-safe x)))

(defun cddr-safe (x)
  (cdr-safe (cdr-safe x)))

;;;;

(defun open-buffer-as-pdf ()
  "Open current buffer's pdf file, if any, in suitable pdf viewer program
(e.g. okular for linux)."
  (interactive)
  (let ((doc-name (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
    (if (file-exists? doc-name)
      (start-process-shell-command "okular - tex preview"
                                   nil
                                   (concat "okular"
                                           " "
                                           (shell-quote-argument doc-name)))
      (error "No pdf file found"))))

;;;;

(require 'custom-predicates)

;; Local Variables:
;; End:

(provide 'custom)

;; custom.el ends here
