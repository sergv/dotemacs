;; persistent-sessions.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 23 January 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'fortunes)
(require 'revive-minimal)

(setf revive-plus:all-frames t
      revive:save-variables-mode-local-private
      '((c++-mode c-indentation-style c-basic-offset)))

;; nil - no 'version field in session data structure;
;;     - encode strings and rings as-is via prin1
;; 2   - add 'version field to session data structure;
;;     - encode strings and ring contents with explicit property list
(defconst +sessions-schema-version+ 2)

(defsubst make-session-entry (buf-name point variables major-mode other-data)
  (list buf-name
        point
        variables
        major-mode
        other-data))
(defsubst session-entry/buffer-name (entry)
  "Extract buffer name from ENTRY. For buffers with  files this is absolute file
name, for temporary buffers - just the buffer name."
  (first-safe entry))
(defsubst session-entry/point (entry)
  (first-safe (rest-safe entry)))
(defsubst session-entry/variables (entry)
  (first-safe (rest-safe (rest-safe entry))))
;; Major mode is remembered because there're cases when emacs infers wrong
;; mode for otherwise normal buffer. Also modes may be changed by hand, so
;; it is preserved for every buffer.
(defsubst session-entry/major-mode (entry)
  (first-safe (rest-safe (rest-safe (rest-safe entry)))))
(defsubst session-entry/other-data (entry)
  (first-safe (rest-safe (rest-safe (rest-safe (rest-safe entry))))))


(defun sessions/assert (condition msg)
  (unless condition
    (sessions/report-assert-failed msg)))

(defun sessions/assert-with-args (condition msg &rest args)
  (unless condition
    (let* ((truncate
            (lambda (arg)
              (let ((s (format "%S" arg)))
                (if (< (length s) 100)
                  s
                  (concat (substring s 0 (length s))
                          "...")))))
           (err-msg (apply #'format msg (-map truncate args))))
      (sessions/report-assert-failed err-msg))))

(put 'sessions/assert-failed 'error-conditions '(sessions/assert-failed error))

(defun sessions/report-assert-failed (msg)
  (signal 'sessions/assert-failed msg))

(defmacro sessions/report-and-ignore-asserts (&rest body)
  (declare (indent 0))
  `(condition-case err
       (progn ,@body)
     (sessions/assert-failed
      (message "Warning: assertion failed: %s" err))))


(defparameter *sessions-buffer-variables*
  (list
   (list (lambda (buf)
           (require 'haskell-misc)
           (with-current-buffer buf (memq major-mode +haskell-syntax-modes+)))
         'haskell-compile-command
         'haskell-compile-cabal-build-command
         'haskell-process-args-ghci))
  "List of buffer-local variables to save in session file.
Format: list of (<predicate> <vars>) where predicate is a function of
single buffer argument that should return t if <vars> variables should
be saved or restored for current buffer. Obviously it shouldn't depend
on values of said variables.")

(defun sessions/get-buffer-variables (buffer)
  "Get buffer's local variables that should be saved."
  (with-current-buffer buffer
    (-mapcat (lambda (entry)
               (let ((pred (car entry))
                     (vars (cdr entry)))
                 (when (funcall pred buffer)
                   (-map (lambda (var)
                           (when (and (boundp var)
                                      (local-variable? var))
                             (cons var (symbol-value var))))
                         vars))))
             *sessions-buffer-variables*)))

(defun sessions/restore-buffer-variables (buffer bindings)
  "Restore variables captured in BINDINGS for buffer BUFFER."
  (with-current-buffer buffer
    (dolist (entry *sessions-buffer-variables*)
      (let ((pred (car entry))
            (vars (cdr entry)))
        (when (funcall pred buffer)
          (dolist (bind bindings)
            (let ((var (car bind)))
              (when (memq var vars)
                (set var (cdr bind))))))))))

(defparameter *sessions-global-variables* '(log-edit-comment-ring
                                            vim:ex-history
                                            read-expression-history
                                            *search-minibuffer-history*)
  "List of global variables to save in session file.")

(defun sessions/strip-text-properties (val)
  (cond
    ((ring-p val)
     (let ((result (make-ring (ring-size val))))
       (dotimes (n (ring-length val))
         (ring-insert result
                      (sessions/strip-text-properties
                       (ring-ref val n))))
       result))
    ((stringp val)
     (substring-no-properties val))
    (t
     val)))

(defun sessions/truncate-long-sequences (val)
  "Truncate overly long sequences."
  (let ((max-size 1000))
    (cond
      ((ring? val)
       (if (< max-size (ring-size val))
         (foldr (lambda (r item)
                  (ring-insert r item)
                  r)
                (make-ring max-size)
                (subseq (ring-elements val) 0 (min max-size (ring-length val))))
         val))
      ((and (or (list? val)
                (vector? val))
            (< max-size (length val)))
       (subseq val 0 (min max-size (length val))))
      (t
       val))))

(defun sessions/get-global-variables ()
  "Get global variables that should be saved in form of sequence of (var . value)
entries."
  (remq nil
        (-map (lambda (var)
                (when (boundp var)
                  (cons var (sessions/strip-text-properties
                             (sessions/truncate-long-sequences
                              (symbol-value var))))))
              *sessions-global-variables*)))

(defun sessions/restore-global-variables (bindings)
  "Restore global variables from BINDINGS."
  (dolist (bind bindings)
    (set (car bind) (cdr bind))))

(defparameter sessions/ignored-temporary-buffer-modes
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

(defparameter sessions/special-modes
  `((shell-mode
     (save ,(lambda (buf)
              (save-excursion
                (with-inhibited-read-only
                 (goto-char (point-max))
                 (forward-line -1)
                 (list (list 'contents
                             ;; collect properties as well
                             (sessions/store-string
                              (buffer-substring (point-min) (point-max))))
                       (list 'current-dir
                             (sessions/store-string
                              (expand-file-name default-directory)))
                       (list 'comint-input-ring
                             (sessions/store-ring
                              comint-input-ring)))))))
     (restore ,(lambda (version buffer-name saved-data)
                 (let ((buf (get-buffer-create buffer-name)))
                   (sessions/report-and-ignore-asserts
                     (aif (assoc 'contents saved-data)
                       (with-current-buffer buf
                         (with-inhibited-read-only
                          (with-inhibited-modification-hooks
                           (with-inhibited-redisplay
                             (goto-char (point-min))
                             (insert (sessions/versioned/restore-string version (second it)))
                             (insert "\n\n")))))
                       (message "shell-restore: no 'contents")))
                   (shell buf)
                   (accept-process-output (get-buffer-process buf)
                                          5 ;; Time to wait in seconds.
                                          )
                   (sessions/report-and-ignore-asserts
                     (let ((current-dir
                            (cadr-safe
                             (assoc 'current-dir saved-data))))
                       (aif current-dir
                         (progn
                           (goto-char (point-max))
                           (insert "cd \""
                                   (sessions/versioned/restore-string version current-dir)
                                   "\"")
                           (comint-send-input))
                         (message "shell-restore: no 'current-dir"))))
                   (sessions/report-and-ignore-asserts
                     (aif (cadr-safe (assoc 'comint-input-ring saved-data))
                       (setf comint-input-ring
                             (sessions/versioned/restore-ring version it))
                       (message "shell-restore: no 'comint-input-ring")))))))))


(defun sessions/save-buffers/make-session ()
  "Make session to save later"
  (let* ((buffers (buffer-list))
         (buffer-data
          (-map (lambda (buf)
                  (with-current-buffer buf
                    (make-session-entry
                     (sessions/store-string (abbreviate-file-name buffer-file-name))
                     (point)
                     (sessions/get-buffer-variables buf)
                     major-mode
                     nil)))
                (--filter (not (null? (buffer-file-name it)))
                          buffers)))
         (temporary-buffer-data
          (-map (lambda (buf)
                  (with-current-buffer buf
                    (make-session-entry
                     (sessions/store-string (buffer-name buf))
                     (point)
                     (sessions/get-buffer-variables buf)
                     major-mode
                     (sessions/store-string
                      (buffer-substring (point-min) (point-max))))))
                (-filter #'sessions/is-temporary-buffer? buffers)))
         (special-buffer-data
          (remq nil
                (-map (lambda (buf)
                        (with-current-buffer buf
                          (when-let* (spec-entry
                                      (assq major-mode
                                            sessions/special-modes)
                                      save-func (cadr-safe (assq 'save spec-entry)))
                            (list major-mode
                                  (sessions/store-string
                                   (buffer-name buf))
                                  (funcall save-func buf)))))
                      buffers)))
         (frame-data
          (revive-plus:window-configuration-printable)))
    (list (list 'schema-version +sessions-schema-version+)
          (list 'buffers buffer-data)
          (list 'temporary-buffers temporary-buffer-data)
          (list 'special-buffers special-buffer-data)
          (list 'frames frame-data)
          (list 'global-variables (sessions/get-global-variables)))))

(defun sessions/save-buffers (file)
  "Save all buffers that have physical file assigned into FILE."
  (interactive "Ffile to save session in: ")
  (with-temp-buffer
    (insert ":;exec emacs --load \"$0\"\n\n")
    (insert ";:;exec gdb -ex \"run\" --args emacs --load \"$0\"\n\n")
    (insert (format ";; this session was created on %s\n;; Today's quotes are\n%s\n"
                    (format-time-string "%A, %e %B %Y")
                    (loop
                      for i from 0 to 5
                      concating
                      (concat
                       (join-lines
                        (-map (lambda (x) (concat ";; " x))
                              (split-into-lines (fortune/get-next-fortune))))
                       "\n;;\n"))))
    (print '(require 'persistent-sessions) (current-buffer))
    (let ((session (sessions/save-buffers/make-session)))
      (insert "(sessions/load-from-data\n")
      (insert "  '(\n")
      (dolist (x session)
        (print x (current-buffer))
        (insert "\n"))
      (insert "))\n")
      (insert "\n\n;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; coding: utf-8
;; mode: emacs-lisp
;; End:")
      (write-region (point-min) (point-max) file)
      (make-file-executable file))))

(defun sessions/is-temporary-buffer? (buf)
  (with-current-buffer buf
    (and (null? (buffer-file-name buf))
         (not (memq major-mode
                    sessions/ignored-temporary-buffer-modes))
         (not (assq major-mode
                    sessions/special-modes))
         (not (string-match-pure? "\\(?:^ \\)\\|\\(?:^\\*.*\\*$\\)"
                                  (buffer-name buf))))))

(defun sessions/call-symbol-function (sym)
  "Call function bound to symbol SYM, autoloading it if necessary."
  (let ((func (symbol-function sym)))
    (when (autoloadp func)
      (autoload-do-load func))
    (funcall (symbol-function sym))))

(defun sessions/load-from-data (session-entries)
  "Load session from DATA."
  (let ((setup-buffer
         (lambda (point mode vars)
           (sessions/report-and-ignore-asserts
             (sessions/assert-with-args (symbolp mode)
                                        "Invalid mode: %s"
                                        mode)
             (setf mode (or (and (fboundp mode)
                                 mode)
                            default-major-mode))
             (unless (eq? major-mode mode)
               (sessions/call-symbol-function mode)))
           (sessions/report-and-ignore-asserts
             (sessions/assert-with-args (numberp point)
                                        "Invalid point: %s"
                                        point)
             (goto-char point))
           (sessions/restore-buffer-variables (current-buffer) vars)))
        (version
         (cadr-safe
          (assq 'schema-version session-entries))))
    (sessions/report-and-ignore-asserts
      (sessions/assert-with-args
       (or (null version)
           (numberp version))
       "Invalid version: %s"
       version))
    (aif (assq 'buffers session-entries)
      (mapc (lambda (entry)
              (sessions/report-and-ignore-asserts
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
                             (session-entry/variables entry))))))
            (cadr it))
      (message "sessions/load-from-data: no 'buffers field"))
    (aif (assq 'temporary-buffers session-entries)
      (mapc (lambda (entry)
              (sessions/report-and-ignore-asserts
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
                               (session-entry/variables entry)))))))
            (cadr it))
      (message "sessions/load-from-data: no 'temporary-buffers field"))
    (aif (assq 'special-buffers session-entries)
      (-map (lambda (saved-info)
              (let ((mmode (first saved-info))
                    (buffer-name
                     (sessions/versioned/restore-string
                      version
                      (second saved-info)))
                    (special-data (third saved-info)))
                (when-let* (spec-entry   (assq mmode sessions/special-modes)
                            restore-func (cadr-safe (assq 'restore spec-entry)))
                  (funcall restore-func version buffer-name special-data))))
            (cadr it))
      (message "sessions/load-from-data: no 'special-buffers field"))
    (aif (assq 'frames session-entries)
      (revive-plus:restore-window-configuration (first (rest it)))
      (message "sessions/load-from-data: no 'frames field"))
    (aif (assq 'global-variables session-entries)
      (sessions/restore-global-variables (first (rest it)))
      (message "sessions/load-from-data: no 'global-variables field"))))

(defvar sessions/load-buffers-hook nil
  "Hook run after restoring session in `sessions/load-buffers'.")

(defun sessions/load-buffers (file)
  "Load session from FILE's contents."
  (interactive "ffile to load session from: ")
  (if (file-exists? file)
    (progn
      (sessions/load-from-data
       (with-temp-buffer
         (insert-file-contents-literally file)
         (read (buffer-substring-no-properties (point-min) (point-max)))
         ;; (read (current-buffer))
         ))
      (run-hooks 'sessions/load-buffers-hook))
    (message "warning: file %s does not exist" file)))



(defun sessions/versioned/restore-string (version encoded-data)
  (cond
    ((null version)
     (sessions/assert-with-args (stringp encoded-data)
                                "Trying legacy string restore from non-string: %s"
                                encoded-data)
     encoded-data)
    ((>= version 2)
     (sessions/restore-string encoded-data))
    (t
     (sessions/report-assert-failed
      (format "Invalid version: %s" version)))))

(defun sessions/versioned/restore-ring (version encoded-data)
  (cond
    ((null version)
     (sessions/assert-with-args (ring-p encoded-data)
                                "Trying legacy ring restore from non-ring: %s"
                                encoded-data)
     encoded-data)
    ((>= version 2)
     (sessions/restore-ring encoded-data))
    (t
     (sessions/report-assert-failed
      (format "Invalid version: %s" version)))))

(defun sessions/map-ring (f ring)
  (sessions/assert (ring-p ring) "Not a ring")
  (let ((result (make-ring (ring-size ring))))
    (dotimes (n (ring-length ring))
      (ring-insert result
                   (funcall f
                            (ring-ref ring n))))
    result))

(defun sessions/store-ring (ring &optional do-not-store-properties)
  (sessions/map-ring (lambda (x) (sessions/store-string x do-not-store-properties))
                     ring))

(defun sessions/restore-ring (encoded-data)
  (sessions/map-ring #'sessions/restore-string encoded-data))

(defun sessions/store-string (str &optional do-not-store-properties)
  (list
   'string
   (split-string (base64-encode-string (sessions/strip-text-properties str))
                 "\n"
                 t)
   (if do-not-store-properties
     nil
     (sessions/get-all-text-properties-in-string str))))

(defun sessions/restore-string (encoded-data)
  (sessions/assert (eq 'string (car encoded-data))
                   "Invalid tag of encoded string")
  (let ((base64-lines (cadr encoded-data))
        (props (caddr encoded-data)))
    (sessions/assert-with-args base64-lines
                               "Error while restoring string from encoded data: %s"
                               encoded-data)
    (let ((str
           (with-temp-buffer
             (dolist (line base64-lines)
               (insert line))
             (base64-decode-string
              (buffer-substring-no-properties (point-min) (point-max))))))
      (dolist (entry props)
        (sessions/report-and-ignore-asserts
          (sessions/assert-with-args (and (listp entry)
                                          (= 3 (length entry)))
                                     "Invalid property entry: %s"
                                     entry)
          (let ((prop-name (car entry))
                (prop-value (cadr entry))
                (prop-positions (caddr entry)))
            (sessions/assert-with-args (symbolp prop-name)
                                       "Invalid property name: %s"
                                       prop-name)
            (dolist (pos prop-positions)
              (sessions/assert-with-args (or (numberp pos)
                                             (consp pos))
                                         "Invalid property range: %s"
                                         pos)
              (let* ((singleton-range? (numberp pos))
                     (start (if singleton-range?
                              pos
                              (car pos)))
                     ;; Add one because `put-text-property' does not include its
                     ;; end argument into the property range
                     (end (+ 1
                             (if singleton-range?
                               pos
                               (cdr pos)))))
                (put-text-property start end prop-name prop-value str))))))
      str)))



(cl-defstruct (sessions/position-ranges
               (:conc-name sessions/position-ranges/))
  ranges ;; List of either <number> or cons pairs (<number> . <number>).
         ;; Cons pair stands for (<start> . <end>) range, single number -
         ;; for singleton range (<start> . <start>).
         ;;
         ;; Later positions appear at the beginning of the list.
  )

(defun sessions/position-ranges/add-position (pos position-ranges)
  (cl-assert (numberp pos))
  (let* ((ranges (sessions/position-ranges/ranges position-ranges))
         (new-ranges
          (if ranges
            (let ((range (car ranges)))
              (cond
                ((numberp range)
                 (cl-assert (< range pos)
                            nil
                            "Must add positions in increasing order only")
                 (if (= (+ range 1) pos)
                   (progn
                     (setf (car ranges) (cons range pos))
                     ranges)
                   (cons pos ranges)))
                ((consp range)
                 (let ((start (car range))
                       (end (cdr range)))
                   (cl-assert (< end pos)
                              nil
                              "Must add positions in increasing order only")
                   (if (= (+ end 1) pos)
                     (progn
                       (setf (cdr range) pos)
                       ranges)
                     (cons pos ranges))))
                (t
                 (error "Invalid range: %s" range))))
            (list pos))))
    (setf (sessions/position-ranges/ranges position-ranges)
          new-ranges)
    position-ranges))

(defun sessions/get-all-text-properties-in-string (str)
  (let ((properties
         (make-hash-table :test #'eq)))
    (dotimes (pos (length str))
      (let ((props (text-properties-at pos str)))
        (while props
          (let* ((key (car props))
                 (value (cadr props))
                 (value-table
                  (gethash key
                           properties
                           (make-hash-table :test #'equal))))
            (puthash
             value
             (sessions/position-ranges/add-position
              pos
              (gethash value
                       value-table
                       (make-sessions/position-ranges)))
             value-table)
            (puthash key
                     value-table
                     properties))
          (setf props (cddr props)))))
    (let ((results nil))
      (maphash (lambda (prop value-table)
                 (maphash (lambda (val ranges)
                            (push (list prop val (reverse (sessions/position-ranges/ranges ranges))) results))
                          value-table))
               properties)
      results)))

(provide 'persistent-sessions)

;; local Variables:
;; End:

;; persistent-sessions.el ends here
