;; common-heavy.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 18 May 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'custom-predicates)
(require 'macro-util)

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
                          (concat (unless (platform-os-type? 'windows) "/")
                                  (join-lines path "/")))))
      (while (and (not found?)
                  (not (null? path)))
        (let ((subdir (funcall path-join (reverse path))))
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
                                (--any? (string-prefix? it p)
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
                                    (lambda (idx selection-type)
                                      (select-exit)
                                      (funcall
                                       (pcase selection-type
                                         (`same-window  #'find-file)
                                         (`other-window #'find-file-other-window))
                                       (nth idx files)))
                                    :item-show-function
                                    (lambda (x)
                                      (concat "file: " (file-name-nondirectory x) "\n"
                                              x "\n"))
                                    :preamble-function
                                    (lambda ()
                                      "Multiple files found\n\n")
                                    :separator-function
                                    (lambda () ""))))
        (error "No file found for \"%s\" regexp" filename-re)))))

;;;

(defun extract-unicode ()
  ;; note - every single bit of this function is made to let this function
  ;; work as fast as it can an as large buffers as possible
  ;; (on 2k lines performance is acceptable)
  (let ((chars (string->list (buffer-substring-no-properties (point-min)
                                                             (point-max)))))
    (sort (remove-duplicates (--filter (< 127 it) chars)) #'<)))

(defun input-unicode ()
  (interactive)
  (let* ((symbs (-map #'char->string (extract-unicode)))
         (symb (ido-completing-read "> " symbs)))
    (remove-text-properties 0 (length symb) '(font-lock-face nil) symb)
    (insert symb)))

;;;

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
    (dolist (p (--map (file-relative-name it new-config-dir)
                      (find-rec new-config-dir
                                :filep
                                (lambda (p)
                                  (let ((fname (file-name-nondirectory p)))
                                    (and (or (string-match-pure? "^.*\\.el$"
                                                                 fname)
                                             (string-match-pure? "^.*/?scripts/.*$"
                                                                 p)
                                             ;; yasnippet snippets
                                             (string-match-pure? "^.*/?snippets/.*$"
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
              (assert (file-exists? new))
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

;;;

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
                                  (remove-whitespace (subseq line 1)))))
           (old (join-lines (-map cleanup-diff-line
                                  (-filter (funcall make-filter ?-)
                                           lines))
                            "\n"))
           (new (join-lines (-map cleanup-diff-line
                                  (-filter (funcall make-filter ?+)
                                           lines))
                            "\n")))
      (string=? old new))))

(defun remove-duplicates-from-sorted-list-by (eq-pred xs)
  "Remove consecutive elements of xs for which eq-pred returns t."
  (when (not (null? xs))
    (let* ((prev (car xs))
           (ys (cdr xs))
           (result (list prev)))
      ;; prev is assumed to always be added to result
      (while (not (null? ys))
        (let ((y (car ys)))
          (unless (funcall eq-pred prev y)
            (setf prev y)
            (push prev result))
          (setf ys (cdr ys))))
      (nreverse result))))

;;;

(defparameter custom/exec-with-directory-runners
  (let ((tbl (make-hash-table :test #'equal))
        (make-starter
         (lambda (exec args)
           (assert (string? exec))
           (assert (list? args))
           (let ((cmdline (join-lines (cons exec args) " ")))
             (lambda (dir)
               (async-shell-command (concat cmdline " " (shell-quote-argument dir)))))))
        (standard-starter
         (lambda (exec)
           (lambda (dir)
             (async-shell-command (join-lines (list exec
                                                    (shell-quote-argument dir))
                                              " "))))))
    (puthash "thunar" (funcall make-starter "thunar" '()) tbl)
    (puthash "nautilus" (funcall make-starter "nautilus" '()) tbl)
    (puthash "exo-open"
             (funcall make-starter
                      "exo-open"
                      '("--launch"
                        "TerminalEmulator"
                        "--working-directory"))
             tbl)
    (puthash "konsole"
             (funcall make-starter
                      "konsole"
                      '("--workdir"))
             tbl)
    (puthash "xfce4-terminal"
             (funcall make-starter
                      "xfce4-terminal"
                      '("--default-working-directory"))
             tbl)
    (puthash "explorer"
             (funcall make-starter
                      "C:\\Windows\\explorer.exe"
                      '())
             tbl)

    tbl)
  "Definitions of various executables that can be started in particular folder.")

(defun custom/run-first-matching-exec (execs)
  (assert (-all? (lambda (exec)
                   (not (null? (gethash exec custom/exec-with-directory-runners))))
                 execs))
  (let ((dir (expand-file-name
              (aif buffer-file-name
                (file-name-directory it)
                default-directory))))
    (letrec ((iter
              (lambda (execs)
                (when (not (null? execs))
                  (if (executable-find (car execs))
                    (funcall (gethash (car execs)
                                      custom/exec-with-directory-runners)
                             dir)
                    (funcall iter (cdr execs)))))))
      (funcall iter execs))))

(defun start-file-manager ()
  "Start suitable file manager in folder associated with current buffer."
  (interactive)
  (save-window-excursion
    (custom/run-first-matching-exec
     (cond
       ((platform-os-type? 'windows)
        '("explorer"))
       ((platform-os-type? 'linux)
        '("thunar" "nautilus"))
       (t
        (error "unknown platform - no known file managers"))))))

(defun start-terminal-emulator ()
  "Start suitable terminal emulator in folder associated with current buffer."
  (interactive)
  (save-window-excursion
    (custom/run-first-matching-exec '("xfce4-terminal"
                                      "exo-open"
                                      "konsole"
                                      ;; "gnome-terminal"
                                      ))))

;;;

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
    (when (and (not (file-exists? buffer-file-name))
               (y-or-n? (format "Kill buffer %s?" (buffer-name buf))))
      (kill-buffer buf))))

(defun rm (filename)
  (interactive (list (expand-file-name
                      (read-file-name "Delete file: "
                                      nil
                                      buffer-file-name))))
  (let ((buf (find-buffer-visiting filename)))
    (delete-file filename)
    (when (and (buffer-live-p buf)
               (string=
                (with-current-buffer buf
                  (expand-file-name buffer-file-name))
                filename)
               (y-or-n? (format "Kill buffer %s?" (buffer-name buf))))
      (kill-buffer buf))))

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

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
      (switch-to-buffer buf)))

(defun fontify-conflict-markers (&optional mode)
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
           eol)
      0 'warning t))))

(defun resolve-obs-or-rel-filename (path dir)
  "Try to come up with filename that refers to existing file and contains
PATH and, maybe, DIR as it's parts."
  (if (or (file-exists-p path)
          (file-directory-p path))
    path
    (if (file-name-absolute-p path)
      (error "Non-existing absolute file name: %s, probably something went wrong" path)
      (let ((abs-path (concat (eproj-normalize-file-name dir) "/" path)))
        (if (or (file-exists-p abs-path)
                (file-directory-p abs-path))
          abs-path
          (error "File %s does not exist, try `eproj-update-buffer-project'"
                 abs-path))))))

(defun remove-duplicates-sorted (xs eq-func)
  "Remove duplicates from sorted list in linear time."
  (let ((ys xs))
    (while ys
      (if (and (cdr ys)
               (funcall eq-func (car ys) (cadr ys)))
        (setf (cdr ys) (cddr ys))
        (setf ys (cdr ys)))))
  xs)

(defun remove-duplicates-sorting (xs eq-func comparison)
  "Sort XS using COMPARISON function and remove duplicates from the result
using EQ-FUNC to determine equal elements."
  (assert (list? xs))
  (remove-duplicates-sorted (sort xs comparison) eq-func))

;;;

(provide 'common-heavy)

;; Local Variables:
;; End:

;; common-heavy.el ends here
