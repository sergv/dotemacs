;; common-heavy.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 18 May 2013
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'set-up-platform)
  (require 'macro-util)

  (defvar org-element-all-objects)
  (defvar org-element-all-elements))

(declare-function org-element-parse-buffer "org-element")
(declare-function org-element-map "org-element")
(declare-function org-element-put-property "org-element")
(declare-function json-encode "json")
(declare-function dired-get-filename "dired")
(declare-function ffap-guesser "ffap")
(declare-function search-property "search-prop")

(require 'dash)
(require 'common-constants)
(require 'custom-predicates)
(require 'macro-util)

;;;###autoload
(defun find-filename-in-tree-recursive (&optional case-sensetive)
  "Read filename regexp and try to find it in current dir's tree or in trees
obtained by following upward in filesystem. Do case-sensitive name matches
if CASE-SENSETIVE is t."
  (interactive (list current-prefix-arg))
  (let* ((filename-re (read-string-no-default "filename regexp: " ""))
         (path (reverse (split-string (aif buffer-file-name
                                          (file-name-directory it)
                                        (expand-file-name default-directory))
                                      "/"
                                      t)))
         (subdirs-visited '())
         (found? nil)
         (files nil) ;; found files
         (case-fold-search (not case-sensetive)))
    (letrec ((path-join (lambda (path)
                          (concat
                           (fold-platform-os-type
                            "/"
                            nil)
                           (join-lines path "/")))))
      (while (and (not found?)
                  (not (null path)))
        (let ((subdir (funcall path-join (reverse path))))
          (message "searching in %s" subdir)
          (setf files
                (find-rec subdir
                          :filep
                          (lambda (p)
                            (string-match-p filename-re
                                            (file-name-nondirectory p)))
                          :do-not-visitp
                          (lambda (p)
                            (or (version-control-directory?
                                 (file-name-nondirectory p))
                                (--any? (string-prefix? it p)
                                        subdirs-visited)))))
          (when (not (null files))
            (setf found? t))
          (push subdir subdirs-visited)
          (setf path (cdr path))))
      (if found?
          (progn
            (cl-assert (not (null files)))
            (if (= 1 (length files))
                (find-file (car files))
              (select-mode-start-selection
               files
               :after-init #'select-mode-setup
               :on-selection
               (lambda (_idx file selection-type)
                 (select-mode-exit)
                 (funcall
                  (pcase selection-type
                    (`same-window  #'find-file)
                    (`other-window #'find-file-other-window))
                  file))
               :item-show-function
               (lambda (x)
                 (concat "file: " (file-name-nondirectory x) "\n"
                         x "\n"))
               :preamble "Multiple files found\n\n"
               :separator nil)))
        (error "No file found for \"%s\" regexp" filename-re)))))

;;;

;;;###autoload
(defun extract-unicode ()
  ;; note - every single bit of this function is made to let this function
  ;; work as fast as it can an as large buffers as possible
  ;; (on 2k lines performance is acceptable)
  (let ((chars (string->list (buffer-substring-no-properties (point-min)
                                                             (point-max)))))
    (remove-duplicates-sorting (--filter (< 127 it) chars) #'= #'<)))

;;;###autoload
(defun input-unicode ()
  (interactive)
  (let* ((symbs (-map #'char->string (extract-unicode)))
         (symb (ivy-completing-read "> " symbs)))
    (remove-text-properties 0 (length symb) '(font-lock-face nil) symb)
    (insert symb)))

(defun org-to-json (buf)
  "Export the current Org-mode buffer as JSON to the supplied PATH."
  (with-current-buffer buf
    (let ((tree (org-element-parse-buffer)))
      (org-element-map tree
          (append org-element-all-objects org-element-all-elements)
        (lambda (el)
          (org-element-put-property el :parent nil)
          (org-element-put-property el :structure nil)))
      (json-encode tree))))

;;;

;;;###autoload
(defun merge-emacs-configs (new-config-dir curr-config-dir)
  "Merge changes from NEW-CONFIG-DIR into CURR-CONFIG-DIR by successively calling
ediff on files that differ and saving files in CURR-CONFIG-DIR that were updated
while executing ediff.

Use like this to pick changes that will go into CURR-CONFIG-DIR:
\(merge-emacs-configs \"/home/sergey/emacs.new\" \"/home/sergey/emacs\"\)."
  (setf new-config-dir (strip-trailing-slash new-config-dir)
        curr-config-dir (strip-trailing-slash curr-config-dir))
  (let ((ignored-files-re
         (eval-when-compile
           (concat "^.*" (regexp-opt +ignored-file-extensions+) "\\'")))
        (ignored-dirs-re (eval-when-compile
                           (concat "\\(?:^\\|/\\)"
                                   (regexp-opt +ignored-directories+)
                                   "/.*$"))))
    (dolist (p (--map (file-relative-name it new-config-dir)
                      (find-rec new-config-dir
                                :filep
                                (lambda (p)
                                  (let ((fname (file-name-nondirectory p)))
                                    (and (or (string-match-p "^.*\\.el$"
                                                             fname)
                                             (string-match-p "^.*/?scripts/.*$"
                                                             p)
                                             ;; yasnippet snippets
                                             (string-match-p "^.*/?snippets/.*$"
                                                             p))
                                         ;; emacs locks?
                                         (not (string-match-p "^\\.#.*"
                                                              fname))
                                         ;; various binary files
                                         (not (string-match-p ignored-files-re
                                                              fname))
                                         (not (string-match-p ignored-dirs-re
                                                              fname))))))))
      (let* ((new  (concat new-config-dir "/" p))
             (curr (concat curr-config-dir "/" p)))
        (message "Files %s and %s" new curr)
        (condition-case err
            (progn
              (cl-assert (file-exists? new))
              (if (file-exists? curr)
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
                (when (y-or-n-p (format "Copy %s to %s?" new curr))
                  (copy-file new curr nil t t t))))
          (error
           (message "Error occurred while processing files %s and %s:\n%s"
                    new
                    curr
                    err)))))))

;;;###autoload
(defun merge-emacs-configs-default ()
  "Merge from +emacs-config-path+/tmp/emacs into `+emacs-config-path+'."
  (interactive)
  (let ((current-conf-dir +emacs-config-path+)
        (new-conf-dir (concat +emacs-config-path+ "/tmp/emacs")))
    (cl-assert (file-directory? current-conf-dir))
    (if (not (file-directory? new-conf-dir))
        (error "Config under %s not found" new-conf-dir)
      (merge-emacs-configs new-conf-dir current-conf-dir))))

;;;

;;;###autoload
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

;;;

;;;###autoload
(defun patch-whitespace-only-change? (patch)
  "Check whether given PATCH in unified format represents whitespace-only change."
  (save-match-data
    (let* ((lines (split-string patch
                                "\n"
                                ;; keep nulls to reproduce patch exactly when
                                ;; lines are recombined
                                nil))
           (make-filter (lambda (first-char)
                          (lambda (line)
                            (or (= 0 (length line))
                                (let ((c (aref line 0)))
                                  ;; detect space since diffs use
                                  ;; first character - it may be either
                                  ;; + (add), - (removal) or space (no change)
                                  (or (char= c ?\s)
                                      (char= c first-char)))))))
           (cleanup-diff-line (lambda (line)
                                (if (= 0 (length line))
                                    line
                                  (remove-whitespace (substring line 1)))))
           (old (join-lines (-map cleanup-diff-line
                                  (-filter (funcall make-filter ?-)
                                           lines))
                            "\n"))
           (new (join-lines (-map cleanup-diff-line
                                  (-filter (funcall make-filter ?+)
                                           lines))
                            "\n")))
      (string=? old new))))

;;;###autoload
(defun remove-duplicates-from-sorted-list-by (xs eq-pred)
  "Remove consecutive elements of xs for which eq-pred returns t. Produce new list without duplicates."
  (when xs
    (let* ((prev (car xs))
           (ys (cdr xs))
           (res (cons prev nil))
           (tmp res))
      ;; prev is assumed to always be added to result
      (while ys
        (let ((curr (car ys)))
          (unless (funcall eq-pred prev curr)
            (setf prev curr)
            (setf tmp (setcdr-sure tmp (cons prev nil))))
          (setf ys (cdr ys))))
      res)))

;;;

(cl-defstruct (exec-spec
               (:conc-name exec-spec--))
  path
  args)

(defvar custom--known-executables
  (let ((tbl (make-hash-table :test #'equal)))
    (fold-platform-os-type
     (progn
       (puthash "dolphin"
                (make-exec-spec
                 :path "dolphin"
                 :args '("--new-window"))
                tbl)
       (puthash "thunar"
                (make-exec-spec
                 :path "thunar"
                 :args nil)
                tbl)
       (puthash "nautilus"
                (make-exec-spec
                 :path "nautilus"
                 :args nil)
                tbl)
       (puthash "exo-open"
                (make-exec-spec
                 :path "exo-open"
                 :args '("--launch"
                         "TerminalEmulator"
                         "--working-directory"))
                tbl)
       (puthash "konsole"
                (make-exec-spec
                 :path "konsole"
                 :args '("--workdir"))
                tbl)
       (puthash "xfce4-terminal"
                (make-exec-spec
                 :path "xfce4-terminal"
                 :args '("--default-working-directory"))
                tbl)
       (puthash "mate-terminal"
                (make-exec-spec
                 :path "mate-terminal"
                 :args '())
                tbl))
     (progn
       (puthash "explorer"
                (make-exec-spec
                 :path "C:\\Windows\\explorer.exe"
                 :args nil)
                tbl)))
    tbl)
  "Definitions of various executables that can be started in particular folder.")

;;;###autoload
(defun custom--run-first-matching-exec (execs)
  (let ((dir (expand-file-name
              (aif buffer-file-name
                  (file-name-directory it)
                default-directory))))
    (cl-block 'found
      (dolist (exec execs)
        (let ((exec-spec (gethash exec custom--known-executables)))
          (if exec-spec
              (let ((path (exec-spec--path exec-spec))
                    (args (exec-spec--args exec-spec)))
                (cl-assert (stringp path) nil "Invalid executabel path: %s" path)
                (cl-assert (-every-p #'stringp args) nil "Invalid executable args: %s" args)
                (when (or (cached-executable-find exec)
                          (and (file-name-absolute-p (exec-spec--path exec-spec))
                               (file-exists-p (exec-spec--path exec-spec))))
                  (async-shell-command (concat (join-lines (cons path args) " ")
                                               " "
                                               (shell-quote-argument dir)))
                  (cl-return-from 'found)))
            (error "No specification found for exec-spec %s" exec))
          (when (cached-executable-find exec)
            (cl-return-from found
              (funcall (gethash (car execs)
                                custom--known-executables)
                       dir))))))))

;;;###autoload
(defun start-file-manager ()
  "Start suitable file manager in folder associated with current buffer."
  (interactive)
  (save-window-excursion
    (custom--run-first-matching-exec
     (fold-platform-os-type
      (eval-when-compile
        (-filter #'executable-find '("thunar" "dolphin" "nautilus")))
      '("explorer")))))

;;;###autoload
(defun start-terminal-emulator ()
  "Start suitable terminal emulator in folder associated with current buffer."
  (interactive)
  (save-window-excursion
    (custom--run-first-matching-exec
     (eval-when-compile
       (-filter #'executable-find '("konsole" "mate-terminal" "xfce4-terminal" "exo-open"
                                    ;; "gnome-terminal"
                                    ))))))

;;;

;;;###autoload
(defun shell-command+ (command &optional output-buffer error-buffer)
  "Just like `shell-command' but asks to remove current buffer if its file does
not exist after command is finished."
  (interactive (list
                (read-shell-command "Shell command: " nil nil
                                    (let ((filename
                                           (when (and (not buffer-file-name)
                                                      (eq major-mode 'dired-mode))
                                             (dired-get-filename nil t))))
                                      (and filename
                                           (file-relative-name filename))))
                current-prefix-arg
                shell-command-default-error-buffer))
  (let ((buf (current-buffer)))
    (shell-command command output-buffer error-buffer)
    (when (and buffer-file-name
               (not (file-exists? buffer-file-name))
               (y-or-n-p (format "Kill buffer %s?" (buffer-name buf))))
      (kill-buffer buf))))

(defun rm-on-file-and-kill-buffer-afterwards
    (path buf-name on-directory on-file)
  (cond
    ((file-directory-p path)
     (when (and (directory-files path nil nil t)
                (y-or-n-p "Directory not empty, really delete? "))
       (funcall on-directory path)))
    ((file-regular-p path)
     (let ((buf (find-buffer-visiting path)))
       (funcall on-file path)
       (when (and (buffer-live-p buf)
                  (string=
                   (with-current-buffer buf
                     (expand-file-name buffer-file-name))
                   path)
                  (y-or-n-p (format "Kill buffer %s?" (buffer-name buf))))
         (kill-buffer buf))))
    (t
     (error "Path of buffer %s does not exist: %s" buf-name path))))

;;;###autoload
(defun rm (path)
  "Remove file or directory."
  (interactive (list (let ((insert-default-directory nil))
                       (expand-file-name
                        (read-file-name "File or directory to delete: "
                                        nil
                                        (awhen buffer-file-name
                                          (file-name-nondirectory it))
                                        t)))))

  (rm-on-file-and-kill-buffer-afterwards
   path
   (buffer-name)
   (lambda (path) (delete-directory path t))
   #'delete-file))

;;;###autoload
(defun transpose-windows ()
  "From http://www.emacswiki.org/emacs/ToggleWindowSplit."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))
    (error "Must have exactly 2 windows to transpose")))

;;;###autoload
(defun fontify-conflict-markers! (&optional mode)
  "Fontify conflict markers produced by VCS systemts with warning face for MODE.
If MODE is nil - fontify in current buffer."
  (font-lock-add-keywords
   mode
   `((,(rx bol
           (or "|||||||"
               "======="
               (seq (or "<<<<<<<"
                        ">>>>>>>")
                    " "
                    (* any)))
           (? ?\r)
           ?\n)
      0 'warning t))))

;;;###autoload
(defun resolve-to-abs-path (path &optional dir)
  "Try to come up with an absolute filename that refers to
existing file. If PATH is relative then try resolving it against DIR."
  (resolve-to-abs-path-lax path dir #'error))

;;;###autoload
(defun resolve-to-abs-path-lax (path &optional dir on-err)
  "Try to come up with an absolute filename that refers to
existing file. If PATH is relative then try resolving it against DIR."
  (cond
    ((or (file-exists-p path)
         (file-directory-p path))
     (normalise-file-name
      (if (file-name-absolute-p path)
          path
        (expand-file-name path dir))))
    ((file-name-absolute-p path)
     (funcall (or on-err #'ignore)
              "Non-existing absolute file name: %s, probably something went wrong"
              path))
    (t
     (let ((abs-path (normalise-file-name (expand-file-name path dir))))
       (if (or (file-exists-p abs-path)
               (file-directory-p abs-path))
           abs-path
         (funcall (or on-err #'ignore)
                  "File/directory does not exist: %s"
                  abs-path))))))

;;;###autoload
(defun file-name-all-parents (path)
    (let* ((prev "")
           (res (cons nil nil))
           (tmp res))
      (while (and path
                  (not (string= prev path))
                  (not (string= path "")))
        (setf tmp (setcdr-sure tmp (cons path nil))
              prev path
              path (file-name-directory (strip-trailing-slash path))))
      (cdr res)))

;;;###autoload
(defun remove-duplicates-sorted! (xs eq-func)
  "Remove duplicates from sorted list in linear time."
  (when xs
    (cl-assert (consp xs))
    (let ((ys (comp-hint-cons xs)))
      (while ys
        (let ((rest (cdr-sure ys)))
          (if (and rest
                   (funcall eq-func
                            (car-sure ys)
                            (car-sure rest)))
              (setcdr-sure ys (cdr-sure rest))
            (setf ys rest)))))
    xs))

;;;###autoload
(defun remove-duplicates-sorting (xs eq-func comparison)
  "Sort XS using COMPARISON function and remove duplicates from the result
using EQ-FUNC to determine equal elements."
  (cl-assert (listp xs))
  (remove-duplicates-sorted! (sort xs comparison) eq-func))

;;;###autoload
(defun remove-duplicates-hashing (xs eq-func)
  "Remove duplicates from the XS using EQ-FUNC hash-table equality
test function to determine equal elements."
  (cl-assert (consp xs))
  (let* ((tbl (make-hash-table :test eq-func))
         (res (cons nil nil))
         (tmp res))
    (dolist (x xs)
      (unless (gethash x tbl)
        (puthash x t tbl)
        (setf tmp (setcdr-sure tmp (cons x nil)))))
    (cdr res)))

;;;###autoload
(defun remove-duplicates-by-hashing-projections (project eq-func xs)
  "Remove duplicates from the XS by hashing results of applying
PROJECT. EQ-FUNC will be used as hash-table comparison."
  (cl-assert (or (consp xs) (null xs)))
  (when xs
    (let* ((tbl (make-hash-table :test eq-func))
           (first (car xs))
           (res (cons first nil))
           (tmp res))
      (puthash (funcall project first) t tbl)
      (dolist (x (cdr xs))
        (let ((proj (funcall project x)))
          (unless (gethash proj tbl)
            (puthash proj t tbl)
            (setf tmp (setcdr-sure tmp (cons x nil))))))
      res)))

;;;

;;;###autoload
(cl-defun insert-info-format-template (&key
                                       start
                                       end ;; takes list of variable names in order defined by user
                                       format ;; inserts user input and appropriate format specifier
                                       (reindent-at-end #'ignore)
                                       (quote-message #'identity))
  (let* ((beginning (point))
         (var-list nil)
         (insert-message
          (lambda (_is-initial-insertion? user-input)
            (insert (funcall quote-message user-input))))
         (insert-variable
          (lambda (is-initial-insertion? user-input)
            (unless is-initial-insertion?
              (insert ", "))
            (funcall format user-input)
            (push user-input var-list))))
    (insert-info-template
     :start start
     :end (lambda ()
            (funcall end (reverse var-list))
            (save-excursion
              (goto-char beginning)
              (funcall reindent-at-end)))
     :insert-continuation #'ignore
     :insert-message insert-message
     :insert-variable insert-variable)))

;;;###autoload
(cl-defun insert-info-template (&key
                                start
                                end
                                insert-continuation
                                insert-message
                                insert-variable)
  (cl-assert (functionp start))
  (cl-assert (functionp end))
  (cl-assert (functionp insert-continuation))
  (cl-assert (functionp insert-message))
  (cl-assert (functionp insert-variable))
  (let ((user-input nil)
        (is-initial-insertion? t)
        (prev-was-message? nil)
        (is-message?
         (lambda (x)
           (and (not (zerop (length x)))
                (or (char= ?\s (aref x 0))
                    (char= ?\t (aref x 0))))))
        (prompt-user
         (lambda ()
           (read-string-no-default "Variable or message starting with space: "
                                   nil
                                   nil
                                   ""))))
    (funcall start)
    (while (and (setf user-input (funcall prompt-user))
                (not (string= user-input "")))
      (let* ((current-is-message? (funcall is-message? user-input))
             (should-merge-messages? prev-was-message?))
        (unless is-initial-insertion?
          (funcall insert-continuation
                   should-merge-messages?))
        (if current-is-message?
            (funcall insert-message
                     is-initial-insertion?
                     (replace-regexp-in-string "^[ \t]" "" user-input))
          (funcall insert-variable
                   is-initial-insertion?
                   user-input))
        (setf prev-was-message? current-is-message?))
      (setf is-initial-insertion? nil))
    (funcall end)))

;;;;

;; Nested hash tables that allow to aggregate data differently.
(cl-defstruct (nested-hash-tables
               (:constructor make--nested-hash-tables)
               (:conc-name nested-hash-tables/))
  data        ;; chain of hash-tables
  field-specs ;; list of (<lamda to get key value> <comparison-pred>) entries
  )

;;;###autoload
(defun mk-nested-hash-tables (field-specs)
  (cl-assert (and field-specs
                  (listp field-specs)
                  (--any? (and (listp it)
                               (functionp (car it))
                               (functionp (cadr it)))
                          field-specs)))
  (make--nested-hash-tables
   :data (make-hash-table :test (cadr (car field-specs)))
   :field-specs field-specs))

;;;###autoload
(defun nested-hash-tables/add-kv! (key value hash-tables)
  (let ((table (nested-hash-tables/data hash-tables)))
    (cl-loop
      for spec-entry on (nested-hash-tables/field-specs hash-tables)
      do
      (let* ((spec (car spec-entry))
             (get-key (car spec))
             (next-spec (cdr spec-entry))
             (current-level-key (funcall get-key key))
             (next-value
              (if next-spec
                  (or (gethash current-level-key table)
                      (make-hash-table :test (cadr spec)))
                value)))
        (puthash current-level-key
                 next-value
                 table)
        (setf table next-value))))
  hash-tables)

;;;###autoload
(defun nested-hash-tables/add! (key hash-tables)
  (nested-hash-tables/add-kv! key key hash-tables))

;;;###autoload
(defun nested-hash-tables/maphash (f hash-tables)
  (let ((user-value-depth
         (length (nested-hash-tables/field-specs hash-tables))))
    (letrec ((handle-data
              (lambda (depth)
                (lambda (key value)
                  (if (= depth user-value-depth)
                      (funcall f key value)
                    (maphash (funcall handle-data (+ depth 1))
                             value))))))
      (maphash (funcall handle-data 1)
               (nested-hash-tables/data hash-tables)))))

;;;###autoload
(defun nested-hash-tables->alist (hash-tables)
  (let ((result nil))
    (nested-hash-tables/maphash (lambda (k v)
                                  (push (cons k v) result))
                                hash-tables)
    result))

;;;

;;;###autoload
(defun copy-buffer-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (if-let ((filename (if (equal major-mode 'dired-mode)
                         default-directory
                       (buffer-file-name))))
      (progn
        (kill-new filename)
        (message "Copied buffer filename '%s' to the clipboard." filename))
    (let ((dir default-directory))
      (kill-new dir)
      (message "Copied buffer default directory '%s' to the clipboard." dir))))

;;;###autoload
(defun split-by-spaces-with-quotes (command quote-chars backslash-quote)
  "Split command by space, while taking quotation, as defined by
list of characters QUOTE-CHARS, into account. Strips quotes
around individual arguments. Adjacent strings are concatenated,
only space is a separator."
  (let ((result nil)
        ;; NB have to collect word as a list of characters instead of
        ;; taking substrings of the original big string because of
        ;; quoting. I.e. some backslashes may disappear so substrings
        ;; won’t work.
        (word nil)
        (found? nil)
        (string-start nil)
        (i 0)
        (len (length command)))
    (while (< i len)
      (let ((c (aref command i)))
        (if string-start
            (if (and backslash-quote
                     (eq c ?\\))
                (let ((next (aref command (+ i 1))))
                  (if (and (eq string-start backslash-quote)
                           (eq next backslash-quote))
                      (progn
                        (push next word)
                        (cl-incf i 2))
                    (progn
                      (push c word)
                      (cl-incf i))))
              (progn
                (if (eq c string-start)
                    (setf string-start nil)
                  (push c word))
                (cl-incf i)))
          (progn
            (cond
              ((eq c ?\s)
               (when found?
                 (push (list->string (nreverse word)) result)
                 (setf word nil
                       found? nil)))
              ((memq c quote-chars)
               (setf string-start c
                     found? t))
              (t
               (setf found? t)
               (push c word)))
            (cl-incf i)))))
    (when found?
      (push (list->string (nreverse word)) result))
    (nreverse result)))

;;;###autoload
(defun split-shell-command-into-arguments (cmd)
  "Split command by space, while taking quotation into account. Strips quotes
around individual arguments."
  (split-by-spaces-with-quotes cmd '(?\" ?\') ?\"))

;;;###autoload
(defun make-percentage-reporter (total-units percent-increment-to-report on-next-increment)
  "Construct function that takes number of units of work done and invokes ON-NEXT-INCREMENT
when next PERCENT-INCREMENT-TO-REPORT amount of units was reported.

The ON-NEXT-INCREMENT function should take 1 argument - units of work done.
Units of work done will be no greater than total-units.

I.e. if there are 100 total units of work and reporting is done on 5% increments
then ON-NEXT-INCREMENT function will be called 20 times with
0, 5, 10, 15, ..., 90, 95, 100 values."
  (cl-assert (< 0 total-units))
  (cl-assert (< 0 percent-increment-to-report))
  (cl-assert (functionp on-next-increment))
  (let* ((float-total (float total-units))
         (increments-to-report (* percent-increment-to-report
                                  (/ float-total 100)))
         (reported-increments 0)
         (done-units 0))
    (lambda (new-units)
      (let ((new-done-units (+ done-units new-units)))
        (when (< total-units new-done-units)
          (error "With current update, %s, the number of units accumulated, %s, will be more than initial total %s"
                 new-units
                 new-done-units
                 total-units))
        (while (<= (* reported-increments increments-to-report) new-done-units)
          (funcall on-next-increment
                   (* reported-increments increments-to-report))
          (cl-incf reported-increments))
        (setf done-units new-done-units)))))

;;;###autoload
(defun make-standard-progress-reporter (total-units unit-name)
  "Make progress reporter, similar to `make-percentage-reporter', that would
print progress on each 5% of units processed."
  (make-percentage-reporter
   total-units
   5
   (lambda (number-of-units-processed)
     (let ((message-log-max nil))
       (message "Processed %.0f%% of %s - %s out of %s"
                (* 100 (/ (float number-of-units-processed) total-units))
                unit-name
                (truncate number-of-units-processed)
                total-units)))))

;;;

;;;###autoload
(defun re-search-generic-matching (direction predicate regexp &optional bound noerror)
  "Repeatedly calls to `re-search-forward' or
`re-search-backward' depending on DIRECTION until PREDICATE
returns t."
  (let ((search-func
         (fold-direction-at-runtime direction
           #'re-search-forward
           #'re-search-backward))
        (done nil)
        (found nil))
    (while (not done)
      (let ((res (funcall search-func regexp bound noerror)))
        (setf found (and res
                         (funcall predicate))
              done (or (not res)
                       found))))
    found))

;;;

(defun pp-macro (macro-def)
  "Pretty-print macro definition into elisp characters.

E.g.
\"[,('[\" -> [?\C-e ?\[ ?\C-s ?, ?\( ?\n ?' ?\[]
"
  (cl-assert (stringp macro-def))
  (concat "["
          (mapconcat (lambda (c)
                       (cond
                         ((eq c ?\n)
                          "?\\n")
                         ((eq c ?\r)
                          "?\\r")
                         ((eq c ?\t)
                          "?\\t")
                         (t
                          (let ((ppc (pp-char c))
                                (modifier-re "\\([CM]-\\)"))
                            (concat "?"
                                    (when (memq c '(?\( ?\) ?\[ ?\] ?\{ ?\}))
                                      "\\")
                                    (replace-regexp-in-string
                                     modifier-re
                                     "\\\\\\1"
                                     ppc))))))
                     (string->list macro-def)
                     " ")
          "]"))

(defun pp-char (char)
  "Pretty-print charatrec as e.g. \"C-e\", \"[\", \",\", etc."
  (let ((multibyte-p enable-multibyte-characters))
    (if multibyte-p
        (if (< char 128)
            (single-key-description char)
          (string-to-multibyte
           (char-to-string char)))
      (single-key-description char))))

;;;

;;;###autoload
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
              (if (null next-cons)
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

;;;

(defvar buffer-pdf-viewer "okular")

;;;###autoload
(defun open-buffer-as-pdf ()
  "Open current buffer's pdf file, if any, in suitable pdf viewer program
\(e.g. okular for linux\)."
  (interactive)
  (let ((doc-name (concat (file-name-sans-extension buffer-file-name) ".pdf")))
    (if (file-exists-p doc-name)
        (start-process
         "Pdf preview"
         nil
         buffer-pdf-viewer
         doc-name
         doc-name)
      (error "Pdf file found: %s" doc-name))))

;;;

;;;###autoload
(defun mouse-open-file-at-point-other-window (event)
  (interactive "e")
  (mouse-set-point event)
  (let ((filename (ffap-guesser)))
    (if filename
        (if (file-exists-p filename)
            (find-file-other-window filename)
          (error "File does not exist: %s" filename))
      (error "Failed to find a file path around point"))))

;;;

;;;###autoload
(defun circular-jump-forward (regex jump-to-end)
  "Jump forward between regexp matches with wraparound."
  (let ((found? nil)
        (orig-pos (point)))
    (save-match-data
      ;; This rather complicated check checks for case of first prompt in
      ;; the buffer.
      (when (or (= 1 (forward-line 1))
                (eobp)
                (progn
                  (beginning-of-line)
                  (not (setf found? (re-search-forward regex nil t)))))
        (goto-char (point-min))
        (setf found? (re-search-forward regex nil t)))
      (if found?
          (progn
            (goto-char (if jump-to-end (match-end 0) (match-beginning 0)))
            t)
        (progn
          (goto-char orig-pos)
          (forward-line 1)
          nil)))))

;;;###autoload
(defun circular-jump-backward (regex jump-to-end)
  "Jump backward between regexp matches with wraparound."
  (let ((found? nil)
        (orig-pos (point)))
    (save-match-data
      (when (or (= -1 (forward-line -1))
                (bobp)
                (progn
                  (end-of-line)
                  (not (setf found? (re-search-backward regex nil t)))))
        (goto-char (point-max))
        (setf found? (re-search-backward regex nil t)))
      (if found?
          (progn
            (goto-char (if jump-to-end (match-end 0) (match-beginning 0)))
            t)
        (progn
          (goto-char orig-pos)
          (forward-line -1)
          nil)))))

(defun text-property-jump-forward (property value cycle? jump-to-end)
  "Jump forward between text property PROPERTY with value VALUE with wraparound."
  (let ((p (search-property 'forward cycle? property value)))
    (if (and p
             ;; Only do jump to end if we have moved on initial search.
             jump-to-end)
        (goto-char (or (next-single-property-change (point)
                                                    property
                                                    (current-buffer)
                                                    (point-max))
                       (point-max)))
      p)))

(defun text-property-jump-backward (property value cycle? jump-to-end)
  "Jump backward between text property PROPERTY with value VALUE with wraparound."
  (let ((p (search-property 'backward cycle? property value)))
    (if (and p
             ;; Only do jump to end if we have moved on initial search.
             jump-to-end)
        (goto-char (or (next-single-property-change (point)
                                                    property
                                                    (current-buffer)
                                                    (point-max))
                       (point-max)))
      p)))

;;;

;; Originally from stevey, adapted to support moving to a new directory.
;;;###autoload
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
  (let ((dir (file-name-directory new-name)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
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

;;;###autoload
(defun copy-file-and-open (new-name)
  "Copy current file to NEW-NAME and open it."
  (interactive
   (progn
     (unless buffer-file-name
       (error "Buffer '%s' is not visiting a file" (buffer-name)))
     (list (read-file-name (format "Copy %s to: " (file-name-nondirectory
                                                   buffer-file-name))))))
  (when (equal new-name "")
    (error "No name provided, aborting"))
  (setf new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        buffer-file-name)
                                       new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (when (file-exists-p buffer-file-name)
    (make-directory (file-name-directory new-name) t)
    (copy-file buffer-file-name new-name 1 nil t t)
    (find-file new-name)
    (message "Copied to %s" new-name)))

;;;

(defgroup common
  nil
  "My utilities"
  :group 'tools)

(defface evaporate-region-face
  '((t :foreground "#666666"))
  "Face for text that will evaporate when modified/overwritten."
  :group 'common-heavy)

(defun evaporate-region (beg end)
  "Make the region evaporate when typed over."
  (interactive "r")
  (let ((o (make-overlay beg end nil nil nil)))
    (overlay-put o 'face 'evaporate-region-face)
    (overlay-put o 'priority 2)
    (overlay-put o 'modification-hooks '(evaporate-region--modification-hook))
    (overlay-put o 'insert-in-front-hooks '(evaporate-region--insert-before-hook))
    (overlay-put o 'insert-behind-hooks '(evaporate-region--insert-behind-hook))))

(defun evaporate-region--modification-hook (o changed beg end &optional _len)
  "Remove the overlay after a modification occurs."
  (let ((inhibit-modification-hooks t))
    (when (and changed
                (overlay-start o))
      (evaporate-region--delete-text o beg end)
      (delete-overlay o))))

(defun evaporate-region--insert-before-hook (o changed beg end &optional _len)
  "Remove the overlay before inserting something at the start."
  (let ((inhibit-modification-hooks t))
    (when (and (not changed)
               (overlay-start o))
      (evaporate-region--delete-text o beg end)
      (delete-overlay o))))

(defun evaporate-region--insert-behind-hook (o changed beg end &optional _len)
  "Remove the overlay when calling backspace at the end.."
  (let ((inhibit-modification-hooks t))
    (when (and (not changed)
               (overlay-start o))
      (evaporate-region--delete-text o beg end)
      (delete-overlay o))))

(defun evaporate-region--delete-text (o _beg _end)
  "Delete the text associated with the evaporating slot."
  (unless (eq this-command 'undo)
    (delete-region (overlay-start o)
                   (overlay-end o))))

;;; start-profiler-or-report

(autoload 'profiler-running-p "profiler")
(autoload 'profiler-report "profiler" nil t)
(autoload 'profiler-stop "profiler" nil t)

;;;###autoload
(defun start-profiler-or-report ()
  "Either start new profiling session or terminate an existing one with a report."
  (interactive)
  (if (profiler-running-p)
      (progn
        (profiler-report)
        (profiler-stop))
    (progn
      (profiler-start 'cpu+mem))))

;;; balance-other-windows

;;;###autoload
(defun balance-other-windows ()
  (interactive)
  (let ((win (selected-window)))
    (unwind-protect
        (progn
          (window-preserve-size win t t)
          (balance-windows))
      (progn
        (window-preserve-size win t nil)))))

;;;

;;;###autoload
(defun parse-regexp-groups (str)
  "Return list of indices of capturable regexp groups within STR regular expression. Order of
groups in the result is *not specified*."
  (cl-assert (stringp str))
  (let ((groups nil)
        (last-unnumbered-group 1)
        (idx 0)
        (end (length str)))
    (while (< idx end)
      (let ((idx1 (+ idx 1)))
        (pcase (aref str idx)
          ;; [...]
          ;; []...]
          (`?\[
           (if (< idx1 end)
               (let ((curr-idx (pcase (aref str idx1)
                                 (`?\]
                                  (+ idx1 1))
                                 (_
                                  idx1))))
                 (while (and (< curr-idx end)
                             (not (eq (aref str curr-idx) ?\])))
                   (setf curr-idx (+ curr-idx 1)))
                 (setf idx (+ curr-idx 1)))
             ;; Skip
             (setf idx idx1)))
          (`?\\
           (if (< idx1 end)
               (pcase (aref str idx1)
                 ;; \(
                 ;; \(?:
                 ;; \(?7:
                 (`?\(
                  (let ((idx2 (+ idx1 1)))
                    (if (< idx2 end)
                        (pcase (aref str idx2)
                          ;; \(?:
                          (`??
                           (let* ((start (+ idx2 1))
                                  (idx3 start)
                                  (done nil)
                                  (is-numbered? nil))
                             (while (and (< idx3 end)
                                         (not done))
                               (let ((c (aref str idx3)))
                                 (if (and (<= ?0 c)
                                          (<= c ?9))
                                     (setf is-numbered? t)
                                   (setf done t)))
                               (setf idx3 (+ idx3 1)))
                             (when is-numbered?
                               (push (string->number (substring-no-properties str start idx3)) groups))
                             (setf idx idx3)))
                          ;; \(
                          (_
                           (setf groups (cons last-unnumbered-group groups)
                                 last-unnumbered-group (+ last-unnumbered-group 1)
                                 idx idx2)))
                      ;; Skip
                      (setf idx idx2))))
                 ;; \sw
                 (`?s
                  ;; Skip next one too
                  (setf idx (+ idx1 2)))
                 ;; \_<
                 (`?_
                  ;; Skip next one too
                  (setf idx (+ idx1 2)))
                 (_
                  (setf idx (+ idx1 1))))
             ;; Skip
             (setf idx idx1)))
          (_
           (setf idx idx1)))))
    groups))

;;;###autoload
(defun narrow-to-region-indirect (&optional create-new-buf?)
  (interactive "P")
  (with-region-bounds start end
    (when (vim-visual-mode-p)
      (vim:visual-mode-exit:wrapper))
    (if create-new-buf?
        (progn
          (let* ((orig-buf (current-buffer))
                 (new-buf (make-indirect-buffer orig-buf
                                                (narrow-to-region-indirect--find-new-buf-name orig-buf)
                                                t)))
            (with-current-buffer new-buf

              (with-all-matching-overlays
                  ov
                  t
                (if (overlay-get ov 'is-fixed-after-clone?)
                    ;; So that subsequent indirect buffers don’t inherit
                    ;; fixidness state.
                    (overlay-put ov 'is-fixed-after-clone? nil)
                  ;; Remove overlays that were not explicitly migrated.
                  (delete-overlay ov)))

              (narrow-to-region start end))
            (switch-to-buffer new-buf)))

      (narrow-to-region start end))))

(defun narrow-to-region-indirect--find-new-buf-name (orig-buf)
  (generate-new-buffer-name (concat (buffer-name orig-buf)
                                    ":indirect")))

(provide 'common-heavy)

;; Local Variables:
;; End:

;; common-heavy.el ends here
