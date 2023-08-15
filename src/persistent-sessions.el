;; persistent-sessions.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 23 January 2013
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'subr-x)
  (require 'macro-util)
  (defvar eshell-history-ring)
  (defvar eshell-buffer-name)
  (defvar haskell-compilation-mode)
  (defvar rust-compilation-mode)
  (defvar dante-repl-mode))

(require 'frameset)

(require 'common)
(require 'dash)
(require 'haskell-constants)
(require 'persistent-sessions-error-reporting)
(require 'persistent-sessions-global-vars)
(require 'persistent-sessions-serializers)
(require 'pp)

;; nil - No 'version field in session data structure.
;;     - Encode strings and rings as-is via prin1.
;; 2   - Add 'version field to session data structure.
;;     - Encode strings and ring contents with explicit property list.
;; 3   - Support saving tabs from ‘tab-bar-mode’. Will output 'frames-per-tab instead of 'frames
;;       if 'tab-bar-mode was active.
(defconst +sessions-schema-version+ 3)

(defsubst make-session-entry (buf-name point variables mode other-data special-variables)
  "BUF-NAME         - buffer name
POINT             - position within buffer
VARIABLES         - values of local variables
MODE              - buffer's major mode
OTHER-DATA        - some data, depending on buffer type
SPECIAL-VARIABLES - local variables that may require special treatment when restoring"
  (list buf-name
        point
        variables
        mode
        other-data
        special-variables))

(defsubst session-entry/buffer-name (entry)
  "Extract buffer name from ENTRY. For buffers with  files this is absolute file
name, for temporary buffers - just the buffer name."
  (car-safe entry))

(defsubst session-entry/point (entry)
  (car-safe (cdr-safe entry)))

(defsubst session-entry/variables (entry)
  (car-safe (cdr-safe (cdr-safe entry))))

;; Major mode is remembered because there're cases when emacs infers wrong
;; mode for otherwise normal buffer. Also modes may be changed by hand, so
;; it is preserved for every buffer.
(defsubst session-entry/major-mode (entry)
  (car-safe (cdr-safe (cdr-safe (cdr-safe entry)))))

(defsubst session-entry/other-data (entry)
  (car-safe (cdr-safe (cdr-safe (cdr-safe (cdr-safe entry))))))

(defsubst session-entry/special-variables (entry)
  (car-safe (cdr-safe (cdr-safe (cdr-safe (cdr-safe (cdr-safe entry)))))))


(defconst +sessions-buffer-variables+
  (list
   ;; (list (lambda (buf)
   ;;         (with-current-buffer buf (memq major-mode +haskell-syntax-modes+)))
   ;;       )
   (list (lambda (buf)
           (memq (buffer-local-value 'major-mode buf) +rust-modes+))
         'rust-compile-command)
   (list (lambda (buf)
           (memq (buffer-local-value 'major-mode buf) +c-like-modes+))
         'c-indentation-style
         'c-basic-offset))
  "List of buffer-local variables to save in session file.
Format: list of (<predicate> <vars>) where predicate is a function of
single buffer argument that should return t if <vars> variables should
be saved or restored for current buffer. Obviously it shouldn't depend
on values of said variables.")

(defun sessions/get-buffer-variables (buffer)
  "Get buffer's local variables that should be saved."
  (with-current-buffer buffer
    (mapcan (lambda (entry)
              (let ((pred (car entry))
                    (vars (cdr entry)))
                (when (funcall pred buffer)
                  (sessions/store-buffer-local-variables buffer vars))))
            +sessions-buffer-variables+)))

(defun sessions/restore-buffer-variables (version buffer bindings)
  "Restore variables captured in BINDINGS for buffer BUFFER."
  (with-current-buffer buffer
    (dolist (entry +sessions-buffer-variables+)
      (let ((pred (car entry))
            (vars (cdr entry)))
        (when (and (funcall pred buffer)
                   bindings)
          (sessions/versioned/restore-buffer-local-variables
           version
           vars
           bindings))))))


(defvar *sessions-special-variables*
  (remove nil
          (list
           (when (eval-when-compile (fboundp #'structured-haskell-mode))
             (list 'structured-haskell-mode
                   (lambda (buffer)
                     (with-current-buffer buffer
                       (when (memq major-mode +haskell-syntax-modes+)
                         (and (boundp 'structured-haskell-mode)
                              structured-haskell-mode))))
                   (lambda (buffer value)
                     (with-current-buffer buffer
                       (when (memq major-mode +haskell-syntax-modes+)
                         (when (not (equal structured-haskell-mode value))
                           (structured-haskell-mode
                            (if value +1 -1)))))))))))

(defun sessions/get-special-buffer-variables (buffer)
  (-map (lambda (entry)
          (pcase entry
            (`(,var-name ,get-value ,_)
             (list var-name
                   (funcall get-value buffer)))
            (_
             (error "Invalid special variable entry: %s" entry))))
        *sessions-special-variables*))

(defun sessions/restore-special-buffer-variables (buffer values)
  (dolist (entry values)
    (pcase entry
      (`(,var-name ,value)
       (if-let (set-value (caddr (assq var-name *sessions-special-variables*)))
           (if (functionp set-value)
               (funcall set-value buffer value)
             (error "Error: found non-function set-value entry in *sessions-special-variables* for key %s"
                    set-value))
         (message "*sessions-special-variables*: warning: cannot find setter for special variable %s"
                  var-name)))
      (_
       (message "*sessions-special-variables*: warning: invalid special variables entry: %s"
                entry)))))


(defun sessions/get-global-variables ()
  "Get global variables that should be saved in form of sequence of (var . value)
entries."
  (let ((result nil))
    (maphash (lambda (var v)
               (when (and v
                          (boundp var))
                 (push (cons var
                             (sessions/store-value
                              (sessions/truncate-long-sequences
                               (symbol-value var))))
                       result)))
             *sessions-global-variables*)
    result))

(defun sessions/restore-global-variables (version bindings)
  "Restore global variables from BINDINGS."
  (dolist (bind bindings)
    (let ((var (car bind)))
      (when (gethash var *sessions-global-variables*)
        (set var
             (sessions/versioned/restore-value version (cdr bind)))))))

(defvar sessions/ignored-temporary-buffer-modes
  '(dired-mode
    magit-diff-mode
    magit-revision-mode
    magit-mode
    magit-popup-mode
    magit-stashes-mode
    magit-stash-mode
    magit-status-mode
    magit-refs-mode
    magit-merge-preview-mode
    magit-process-mode
    magit-rebase-mode
    magit-log-mode
    magit-log-select-mode
    magit-cherry-mode
    magit-reflog-mode)
  "Buffer with these modes should never be preserved across sessions.")

(defvar sessions/local-vars/compilation-mode
  '(compilation-error-regexp-alist
    compilation-directory
    compilation-arguments
    mode-line-process)
  "Local variables to store for `compilation-mode' buffers.")

(defvar sessions/special-modes
  (let ((mk-save-repl-buffer
         (lambda (store-extra)
           (cl-assert (functionp store-extra))
           (lambda (buf)
             (save-excursion
               (with-inhibited-read-only
                (goto-char (point-max))
                (forward-line -1)
                (append
                 (list (list 'contents (sessions/store-buffer-contents buf))
                       (list 'current-dir
                             (sessions/store-string
                              (expand-file-name default-directory))))
                 (funcall store-extra)))))))
        (save-compilation-buffer
         (lambda (buf)
           (with-current-buffer buf
             (save-excursion
               (with-inhibited-read-only
                (list (list 'contents (sessions/store-buffer-contents buf))
                      (list 'local-variables
                            (sessions/store-buffer-local-variables
                             buf
                             sessions/local-vars/compilation-mode))))))))
        (mk-restore-compilation-buffer
         (lambda (mode)
           (cl-assert (symbolp mode))
           (lambda (version buffer-name saved-data)
             (let ((buf (get-buffer-create buffer-name)))
               (with-current-buffer buf
                 (sessions/report-and-ignore-asserts
                     (format "while restoring contents of compilation buffer '%s'" buffer-name)
                   (aif (assq 'contents saved-data)
                       (sessions/versioned/restore-buffer-contents
                        version
                        buf
                        (cadr it)
                        (lambda () (insert "\n\n")))
                     (message "compilation-restore: no 'contents")))
                 (funcall mode)
                 (sessions/report-and-ignore-asserts
                     (format "while restoring local variables of compilation buffer '%s'" buffer-name)
                   (aif (assq 'local-variables saved-data)
                       (sessions/versioned/restore-buffer-local-variables
                        version
                        sessions/local-vars/compilation-mode
                        (cadr it))
                     (message "compilation-restore: no 'local-variables")))))))))
    `((eshell-mode
       (save ,(funcall mk-save-repl-buffer
                       (lambda ()
                         (list
                          (list 'eshell-history-ring
                                (sessions/store-ring eshell-history-ring))))))
       (restore ,(lambda (version buffer-name saved-data)
                   (message "Restoring eshell buffer %s" buffer-name)
                   (when-let (contents (assoc 'contents saved-data))
                     (save-excursion
                       (require 'eshell)
                       (let* ((eshell-buffer-name buffer-name)
                              ;; The buffer is created here.
                              (buf (eshell)))
                         (sessions/report-and-ignore-asserts
                             (format "while restoring contents of an eshell buffer '%s'" buffer-name)
                           (aif (assq 'contents saved-data)
                               (with-current-buffer buf
                                 (sessions/versioned/restore-buffer-contents
                                  version
                                  buf
                                  (cadr it)
                                  (lambda () (insert "\n\n"))))
                             (message "eshell-restore: no 'contents")))
                         (sessions/report-and-ignore-asserts
                             (format "while restoring current-directory of an eshell buffer '%s'" buffer-name)
                           (if-let (current-dir (cadr-safe (assq 'current-dir saved-data)))
                               (with-current-buffer buf
                                 (goto-char (point-max))
                                 (insert "cd \""
                                         (sessions/versioned/restore-string version current-dir)
                                         "\"")
                                 (eshell-send-input))
                             (message "eshell-restore: no 'current-dir")))
                         (sessions/report-and-ignore-asserts
                             (format "while restoring 'comint-input-ring of eshell buffer '%s'" buffer-name)
                           (aif (cadr-safe (assoc 'eshell-history-ring saved-data))
                               (with-current-buffer buf
                                 (setq-local eshell-history-ring (sessions/versioned/restore-ring version it)))
                             (message "eshell-restore: no 'eshell-input-ring")))))))))
      (shell-mode
       (save ,(funcall mk-save-repl-buffer
                       (lambda ()
                         (list
                          (list 'comint-input-ring
                                (sessions/store-ring comint-input-ring))))))
       (restore ,(lambda (version buffer-name saved-data)
                   (let ((buf (get-buffer-create buffer-name)))
                     (sessions/report-and-ignore-asserts
                         (format "while restoring contents of a shell buffer '%s'" buffer-name)
                       (aif (assq 'contents saved-data)
                           (with-current-buffer buf
                             (sessions/versioned/restore-buffer-contents
                              version
                              buf
                              (cadr it)
                              (lambda () (insert "\n\n"))))
                         (message "shell-restore: no 'contents")))
                     (shell buf)
                     (accept-process-output (get-buffer-process buf)
                                            5 ;; Time to wait in seconds.
                                            )
                     (sessions/report-and-ignore-asserts
                         (format "while restoring current-directory of shell buffer '%s'" buffer-name)
                       (if-let (current-dir (cadr-safe (assq 'current-dir saved-data)))
                           (with-current-buffer buf
                             (goto-char (point-max))
                             (insert "cd \""
                                     (sessions/versioned/restore-string version current-dir)
                                     "\"")
                             (comint-send-input))
                         (message "shell-restore: no 'current-dir")))
                     (sessions/report-and-ignore-asserts
                         (format "while restoring 'comint-input-ring of shell buffer '%s'" buffer-name)
                       (aif (cadr-safe (assq 'comint-input-ring saved-data))
                           (with-current-buffer buf
                             (setq-local comint-input-ring
                                         (sessions/versioned/restore-ring version it)))
                         (message "shell-restore: no 'comint-input-ring")))))))
      (dante-repl-mode
       (save ,(funcall mk-save-repl-buffer
                       (lambda ()
                         (list
                          (list 'comint-input-ring
                                (sessions/store-ring comint-input-ring))
                          (list 'dante-repl--last-command-line
                                (sessions/store-value dante-repl--last-command-line))))))
       (restore ,(lambda (version buffer-name saved-data)
                   (let ((buf (get-buffer-create buffer-name)))
                     (sessions/report-and-ignore-asserts
                         (format "while restoring contents of a dante-repl buffer '%s'" buffer-name)
                       (aif (assq 'contents saved-data)
                           (with-current-buffer buf
                             (sessions/versioned/restore-buffer-contents
                              version
                              buf
                              (cadr it)
                              (lambda () (insert "\n\n"))))
                         (message "dante-repl-restore: no 'contents")))

                     (let ((current-dir
                            (sessions/report-and-ignore-asserts
                                (format "while restoring current-directory of dante-repl buffer '%s'" buffer-name)
                              (if-let (current-dir (cadr-safe (assq 'current-dir saved-data)))
                                  (sessions/versioned/restore-string version current-dir)
                                (progn
                                  (message "dante-repl-restore: no 'current-dir")
                                  nil))))
                           (command-line
                            (aif (cadr-safe (assq 'dante-repl--last-command-line saved-data))
                                (with-current-buffer buf
                                  (setq-local dante-repl--last-command-line
                                              (sessions/versioned/restore-value version it))
                                  dante-repl--last-command-line)
                              (progn
                                (message "dante-repl-restore: no 'dante-repl--last-command-line")
                                nil))))

                       (dante-repl--start-in-buffer-with-command-line buf command-line nil current-dir)

                       (accept-process-output (get-buffer-process buf)
                                              5 ;; Time to wait in seconds.
                                              ))

                     (sessions/report-and-ignore-asserts
                         (format "while restoring 'comint-input-ring of dante- buffer '%s'" buffer-name)
                       (aif (cadr-safe (assq 'comint-input-ring saved-data))
                           (with-current-buffer buf
                             (setq-local comint-input-ring
                                         (sessions/versioned/restore-ring version it)))
                         (message "dante-repl-restore no 'comint-input-ring")))

                     (goto-char (point-max))))))
      (haskell-compilation-mode
       (save ,save-compilation-buffer)
       (restore ,(funcall mk-restore-compilation-buffer #'haskell-compilation-mode)))
      (rust-compilation-mode
       (save ,save-compilation-buffer)
       (restore ,(funcall mk-restore-compilation-buffer #'rust-compilation-mode))))))

(defun sessions/is-temporary-buffer? (buf)
  (with-current-buffer buf
    (and (null (buffer-file-name buf))
         (not (memq major-mode
                    sessions/ignored-temporary-buffer-modes))
         (not (assq major-mode
                    sessions/special-modes))
         (not (string-match-p "\\(?:^ \\)\\|\\(?:^\\*.*\\*$\\)"
                              (buffer-name buf))))))

(defun persistent-sessions--filter-display-param (current _filtered _parameters _saving)
  (not (eq (car current) 'display)))

(defun persistent-sessions--frameset-filter-alist ()
  (cons (cons 'display #'persistent-sessions--filter-display-param)
        frameset-filter-alist))

(defun sessions/save-buffers/make-session ()
  "Make session to save later"
  (let* ((max-lisp-eval-depth 1000)
         (print-circle t)
         (buffers (buffer-list))
         (buffer-data
          (-map (lambda (buf)
                  (with-current-buffer buf
                    (make-session-entry
                     (sessions/store-string (abbreviate-file-name buffer-file-name))
                     (point)
                     (sessions/get-buffer-variables buf)
                     major-mode
                     nil
                     (sessions/get-special-buffer-variables buf))))
                (--filter (not (null (buffer-file-name it)))
                          buffers)))
         (temporary-buffer-data
          (-map (lambda (buf)
                  (with-current-buffer buf
                    (let ((drop-properties?
                           (memq major-mode '(markdown-mode))))
                      (make-session-entry
                       (sessions/store-string (buffer-name buf) drop-properties?)
                       (point)
                       (sessions/get-buffer-variables buf)
                       major-mode
                       (sessions/store-string
                        (buffer-substring (point-min) (point-max))
                        drop-properties?)
                       (sessions/get-special-buffer-variables buf)))))
                (-filter #'sessions/is-temporary-buffer? buffers)))
         (special-buffer-data
          (remq nil
                (-map (lambda (buf)
                        (with-current-buffer buf
                          (when-let ((spec-entry (assq major-mode
                                                       sessions/special-modes))
                                     (save-func (cadr-safe (assq 'save spec-entry))))
                            (list major-mode
                                  (sessions/store-string
                                   (buffer-name buf))
                                  (funcall save-func buf)))))
                      buffers)))
         (frames-entry
          (list 'frameset (frameset-save (frame-list)
                                         :filters (persistent-sessions--frameset-filter-alist)))))

    (list (list 'schema-version +sessions-schema-version+)
          (list 'buffers buffer-data)
          (list 'temporary-buffers temporary-buffer-data)
          (list 'special-buffers special-buffer-data)
          (list 'global-variables (sessions/get-global-variables))
          frames-entry
          (list 'tab-bar-mode (if tab-bar-mode t nil)))))

;;;###autoload
(defun sessions/save-buffers (file)
  "Save all buffers that have physical file assigned into FILE."
  (interactive "FFile to save session to: ")
  (with-temp-buffer
    (insert ":;exec emacs --load \"$0\"\n\n")
    (insert ";:;exec gdb -ex \"run\" --args emacs --load \"$0\"\n\n")
    (insert (format ";; this session was created on %s\n"
                    (format-time-string "%A, %e %B %Y")))
    (print '(require 'persistent-sessions) (current-buffer))
    (let ((session (sessions/save-buffers/make-session)))
      (insert "(sessions/load-from-data\n")
      (insert "'(\n")
      (dolist (entry session)
        (cond
          ((memq (car-safe entry) '(buffers temporary-buffers special-buffers global-variables))
           (insert (format "(%s\n" (car entry)))
           (insert "(")
           (dolist (buf-entry (cadr entry))
             (insert (sessions/pp-to-string buf-entry nil))
             ;; (print buf-entry (current-buffer))
             )
           (insert "))\n"))
          (t
           (insert (sessions/pp-to-string entry nil))
           ;; (print entry (current-buffer))
           (insert "\n"))))
      (insert "))\n")
      (insert "\n\n;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; coding: utf-8
;; mode: emacs-lisp
;; End:")
      (write-region (point-min) (point-max) file)
      (make-file-executable file))))

;;;###autoload
(defun sessions/load-from-data (session-entries)
  "Load session from DATA."
  (let* ((version
          (cadr-safe
           (assq 'schema-version session-entries)))
         (setup-buffer
          (lambda (point mode vars special-vars)
            (sessions/report-and-ignore-asserts
                (format "while restoring major mode of buffer '%s'" (buffer-name))
              (sessions/assert-with-args (symbolp mode)
                                         "Invalid mode: %s"
                                         mode)
              (setf mode (or (and (fboundp mode)
                                  mode)
                             (default-value 'major-mode)))
              (unless (eq? major-mode mode)
                (sessions/call-symbol-function mode)))
            (sessions/report-and-ignore-asserts
                (format "while restoring point of buffer '%s'" (buffer-name))
              (sessions/assert-with-args (numberp point)
                                         "Invalid point: %s"
                                         point)
              (goto-char point))
            (sessions/report-and-ignore-asserts
                (format "while restoring buffer variables of buffer '%s'" (buffer-name))
              (sessions/restore-buffer-variables version (current-buffer) vars))
            (sessions/report-and-ignore-asserts
                (format "while restoring special buffer variables of buffer '%s'" (buffer-name))
              (sessions/restore-special-buffer-variables (current-buffer) special-vars)))))
    (sessions/report-and-ignore-asserts
        "while restoring extracting version"
      (sessions/assert-with-args
       (or (null version)
           (numberp version))
       "Invalid version: %s"
       version))
    (aif (assq 'buffers session-entries)
        (mapc (lambda (entry)
                (sessions/report-and-ignore-asserts
                    (format "while extracting buffer name from '%s'" entry)
                  (let ((buf-name
                         (sessions/versioned/restore-string
                          version
                          (session-entry/buffer-name entry))))
                    (sessions/assert-with-args (file-exists? buf-name)
                                               "File %s does not exist!"
                                               buf-name)
                    (with-current-buffer (find-file-noselect buf-name)
                      (funcall setup-buffer
                               (session-entry/point entry)
                               (session-entry/major-mode entry)
                               (session-entry/variables entry)
                               (session-entry/special-variables entry))))))
              (cadr it))
      (message "sessions/load-from-data: no 'buffers field"))
    (aif (assq 'temporary-buffers session-entries)
        (mapc (lambda (entry)
                (sessions/report-and-ignore-asserts
                    (format "while restoring temporary buffer from '%s'" entry)
                  (unless (memq (session-entry/major-mode entry) sessions/ignored-temporary-buffer-modes)
                    (let ((buf (get-buffer-create
                                (sessions/versioned/restore-string
                                 version
                                 (session-entry/buffer-name entry)))))
                      (with-current-buffer buf
                        (insert
                         (sessions/versioned/restore-string
                          version
                          (session-entry/other-data entry)))
                        (funcall setup-buffer
                                 (session-entry/point entry)
                                 (session-entry/major-mode entry)
                                 (session-entry/variables entry)
                                 (session-entry/special-variables entry)))))))
              (cadr it))
      (message "sessions/load-from-data: no 'temporary-buffers field"))
    (aif (assq 'special-buffers session-entries)
        (-map (lambda (saved-info)
                (let ((mmode (first saved-info))
                      (buf-name
                       (sessions/versioned/restore-string
                        version
                        (second saved-info)))
                      (special-data (third saved-info)))
                  (condition-case err
                      (when-let ((spec-entry (assq mmode sessions/special-modes))
                                 (restore-func (cadr-safe (assq 'restore spec-entry))))
                        (funcall restore-func version buf-name special-data))
                    (err
                     (message "sessions/load-from-data: failed to restore buffer %s: %s"
                              buf-name
                              (cdr err))))))
              (cadr it))
      (message "sessions/load-from-data: no 'special-buffers field"))

    (aif (assq 'tab-bar-mode session-entries)
        (when (cadr it)
          (add-hook 'window-setup-hook #'sessions--enable-tab-bar-mode))
      (message "sessions/load-from-data: no 'tab-bar-mode field"))

    (aif (assq 'frameset session-entries)
        (frameset-restore (cadr it)
                          :reuse-frames t
                          :cleanup-frames t
                          :filters (persistent-sessions--frameset-filter-alist))
      (message "sessios/load-from-data: no 'frameset field"))

    (aif (assq 'global-variables session-entries)
        (sessions/restore-global-variables version (cadr it))
      (message "sessions/load-from-data: no 'global-variables field"))))

(defun sessions--enable-tab-bar-mode ()
  (tab-bar-mode 1)
  (tab-bar--update-tab-bar-lines))

(defvar sessions/load-buffers-hook nil
  "Hook run after restoring session in `sessions/load-buffers'.")

;;;###autoload
(defun sessions/load-buffers (file)
  "Load session from FILE's contents."
  (interactive "fFile to load session from: ")
  (if (file-exists? file)
      (progn
        (sessions/load-from-data
         (with-temp-buffer
           (insert-file-contents-literally file)
           (read (buffer-substring-no-properties (point-min) (point-max)))))
        (run-hooks 'sessions/load-buffers-hook))
    (message "warning: file %s does not exist" file)))

;;;; Utils

(defun sessions/call-symbol-function (sym)
  "Call function bound to symbol SYM, autoloading it if necessary."
  (let ((func (symbol-function sym)))
    (when (autoloadp func)
      (autoload-do-load func))
    (funcall (symbol-function sym))))

(defun sessions/strip-text-properties (val)
  (cond
    ((stringp val)
     (substring-no-properties val))
    ((ring-p val)
     (let ((result (make-ring (ring-size val))))
       (dotimes (n (ring-length val))
         (ring-insert result
                      (sessions/strip-text-properties
                       (ring-ref val n))))
       result))
    (t
     val)))

(defun sessions/truncate-long-sequences (val)
  "Truncate overly long sequences."
  (let ((max-size 2000))
    (cond
      ((ring-p val)
       (if (< max-size (ring-size val))
           (foldr (lambda (r item)
                    (ring-insert r item)
                    r)
                  (make-ring max-size)
                  (seq-subseq (ring-elements val) 0 (min max-size (ring-length val))))
         val))
      ((and (or (listp val)
                (vectorp val))
            (< max-size (length val)))
       (seq-subseq val 0 (min max-size (length val))))
      ((hash-table-p val)
       (let ((result (copy-hash-table val)))
         (clrhash result)
         (maphash (lambda (k v)
                    (puthash k (sessions/truncate-long-sequences v) result))
                  val)
         result))
      (t
       val))))

(defun sessions/store-buffer-contents (buf)
  (with-current-buffer buf
    ;; collect properties as well
    (sessions/store-string
     (buffer-substring (point-min) (point-max))
     nil ;; Store some properties.
     ;; Ignored text properties that cause problems when reading because its
     ;; values are self-referencing circular structures.
     '(compilation-message))))

(defun sessions/versioned/restore-buffer-contents (version buf encoded-data &optional after-insert)
  (with-current-buffer buf
    (with-inhibited-read-only
     (with-inhibited-modification-hooks
      (with-inhibited-redisplay
        (goto-char (point-min))
        (insert (sessions/versioned/restore-string version encoded-data))
        (when after-insert
          (funcall after-insert)))))))

(defun sessions/store-buffer-local-variables (buffer vars)
  (with-current-buffer buffer
    (list 'buffer-local-vars
          (remq nil
                (-map (lambda (var)
                        (when (and (boundp var)
                                   (local-variable-p var))
                          (cons var
                                (sessions/store-value
                                 (symbol-value var)))))
                      vars)))))

(defun sessions/versioned/restore-buffer-local-variables (version vars encoded-bindings)
  (sessions/assert-with-args
   (and encoded-bindings
        (listp encoded-bindings)
        (eq (car encoded-bindings) 'buffer-local-vars))
   "Invalid buffer-local variables: %s"
   encoded-bindings)
  (dolist (bind (cadr encoded-bindings))
    (let ((var (car bind)))
      (when (memq var vars)
        (set var (sessions/versioned/restore-value version (cdr bind)))))))

(defun sessions/pp-to-string (object &optional indent)
  "Return a string containing the pretty-printed representation of OBJECT.
OBJECT can be any Lisp object.  Quoting characters are used as needed
to make output that `read' can handle, whenever this is possible."
  (with-temp-buffer
    (lisp-mode-variables nil)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (let ((print-escape-newlines pp-escape-newlines)
          (print-quoted t))
      (prin1 object (current-buffer)))
    (sessions/pp-buffer indent)
    (buffer-string)))

(defun sessions/pp-buffer (&optional indent)
  "Prettify the current buffer with printed representation of a Lisp object."
  (goto-char (point-min))
  (while (not (eobp))
    ;; (message "%06d" (- (point-max) (point)))
    (cond
      ((ignore-errors (down-list 1) t)
       (save-excursion
         (backward-char 1)
         (skip-chars-backward "'`#^")
         (when (and (not (bobp)) (memq (char-before) '(?\s ?\t ?\n)))
           (delete-region
            (point)
            (progn (skip-chars-backward " \t\n") (point)))
           (insert "\n"))))
      ((ignore-errors (up-list 1) t)
       (while (looking-at-p "\\s)")
         (forward-char 1))
       (delete-region
        (point)
        (progn (skip-chars-forward " \t\n") (point)))
       (insert ?\n))
      (t (goto-char (point-max)))))
  (goto-char (point-min))
  (when indent
    (indent-sexp)))

(provide 'persistent-sessions)

;; Local Variables:
;; End:

;; persistent-sessions.el ends here
