;; configurable-compilation.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 27 December 2019
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'el-patch)
(require 'eproj)
(require 'persistent-sessions-global-vars)

(declare-function flycheck-rust-find-manifest "flycheck-rust")

;;;###autoload
(defun configurable-compilation--unimportant-text (x)
  "Denotes text that user should not focus attention on by default."
  (propertize x
              'face 'font-lock-comment-face
              'font-lock-face 'font-lock-comment-face))

(defvar-local configurable-compilation-command-presets nil
  "Alist of (<symbol> . <string>) pairs where symbol is the
user-visible name of the preset and string is the command to
execute.")

(defvar configurable-compilation-last-command (make-hash-table :test #'equal)
  "Mapping between (<proj-dir> . <major-mode>) and last chosen compilation command.")

(sessions-mark-global-var-for-save 'configurable-compilation-last-command)

(defvar-local configurable-compilation-mode nil
  "Symbol - mode to enable in the compilation buffer. This mode
should derive from ‘configurable-compilation-parent-mode’ and is
responsible for highlighting errors for compilers its using.")

(defvar configurable-compilation-history-var nil
  "Symbol - history variable to store history of chosen commands.")

(defvar-local compilation-command nil
  "Either literal string or symbol naming a variable that contains command to use.")

(defvar configurable-compilation--synonym-modes
  (let ((tbl (copy-hash-table eproj/synonym-modes-table)))
    (puthash 'haskell-cabal-mode 'haskell-mode tbl)
    tbl))

(defun configurable-compilation--resolve-synonym-modes (mode)
  "Resolve similar modes into the same mode so that cached compilation command will be the
same for a set of buffers rather than being different."
  (gethash mode configurable-compilation--synonym-modes mode))

(defun configurable-compilation--get-presets ()
  (aif (or (and (stringp compilation-command)
                compilation-command)
           (and (symbolp compilation-command)
                (boundp compilation-command)
                (eval compilation-command)))
      (if (stringp it)
          (cons (cons 'custom it)
                configurable-compilation-command-presets)
        (error "`compilation-command' evaluated to non-string"))
    configurable-compilation-command-presets))

(defmacro configurable-compilation--with-proj-dir-and-command-key (proj-dir-var command-key-var &rest body)
  (declare (indent 2))
  (cl-assert (symbolp proj-dir-var))
  (cl-assert (symbolp command-key-var))
  (let ((effective-major-mode-var '#:effective-major-mode))
    `(let* ((,proj-dir-var (configurable-compilation-proj-dir))
            (,effective-major-mode-var
             (configurable-compilation--resolve-synonym-modes major-mode))
            (,command-key-var (cons ,proj-dir-var ,effective-major-mode-var)))
       ,@body)))

(defun configurable-compilation-install-command-presets! (presets history-var compilation-mode)
  (cl-assert (--all? (and (consp it)
                          (symbolp (car it))
                          (functionp (cdr it)))
                     presets))
  (cl-assert (symbolp history-var))
  (cl-assert (symbolp compilation-mode))
  (setq-local configurable-compilation-command-presets presets)
  (configurable-compilation--with-proj-dir-and-command-key
      proj-dir
      command-key
    (unless (gethash command-key configurable-compilation-last-command)
      (puthash command-key
               (let ((presets (configurable-compilation--get-presets)))
                 (or (cdr-safe (assq 'build presets))
                     (cdar-safe presets)
                     (error "Failed to get default build preset from %s" configurable-compilation-command-presets)))
               configurable-compilation-last-command)))

  (setq-local configurable-compilation-history-var history-var
              configurable-compilation-mode compilation-mode))

(defvar-local configurable-compilation--cached-rust-project-root nil)

(cl-defstruct (cc-command
               (:constructor make--cc-command))
  (cmd nil :read-only t) ;; List of strings, non-empty.
  (env nil :read-only t) ;; List of strings or nil; gets added to ‘process-environment’.
  (dir nil :read-only t) ;; String, a value for ‘default-directory’. Never nil.
  (pretty-cmd nil :read-only t) ;; String with properties, what to show user when this command executes.
  )

(defun make-cc-command (cmd env dir pretty-cmd)
  (cl-assert (not (null cmd)))
  (cl-assert (listp cmd))
  (cl-assert (-all? #'stringp cmd))
  (cl-assert (or (null env) (and (listp env) (-all? #'stringp env))))
  (cl-assert (stringp dir))
  (cl-assert (file-directory-p dir))
  (cl-assert (stringp pretty-cmd))
  (make--cc-command :cmd cmd
                    :env env
                    :dir dir
                    :pretty-cmd pretty-cmd))

(defun configurable-compilation--format-timestamp (x)
  (format-time-string "%a %-d %b %Y %H:%M:%S" x))

(defun configurable-compilation--format-duration (start end)
  (format "%.2f seconds" (float-time (time-subtract end start))))

(defvar-local configurable-compilation--proj-root nil)

(defun configurable-compilation-proj-dir ()
  (or configurable-compilation--proj-root
      (setf configurable-compilation--proj-root
            (or
             (when-let ((epr (eproj-get-project-for-buf-lax (current-buffer))))
               (eproj-project/root epr))
             (when (derived-mode-p 'haskell-mode)
               (haskell-misc-get-project-root))
             (when (derived-mode-p 'rust-mode)
               (if configurable-compilation--cached-rust-project-root
                   configurable-compilation--cached-rust-project-root
                 (when-let ((buf (buffer-file-name))
                            (manifest (flycheck-rust-find-manifest buf)))
                   (setq-local configurable-compilation--cached-rust-project-root
                               (file-name-directory manifest)))))
             default-directory))))

(defun configurable-compilation-buffer-name (proj-dir)
  (concat "*compilation:" proj-dir "*"))

(defun configurable-compilation-start (&optional edit-command)
  "Run a compile command for the current Haskell buffer."
  (interactive "P")
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (configurable-compilation--with-proj-dir-and-command-key
      proj-dir
      command-key
    (let* ((raw-command
            (if edit-command
                (let* ((preset
                        (string->symbol
                         (completing-read "Build preset: "
                                          (configurable-compilation--get-presets)
                                          nil ;; predicate
                                          t   ;; require match
                                          nil ;; initial input
                                          configurable-compilation-history-var)))
                       (command
                        (cdr
                         (assq preset (configurable-compilation--get-presets)))))
                  ;; Remember command so it will be called again in the future.
                  (puthash command-key command configurable-compilation-last-command)
                  command)
              (gethash command-key configurable-compilation-last-command)))
           (cmd (cond
                  ;; ((stringp raw-command)
                  ;;  (if proj-dir
                  ;;      (format raw-command (expand-file-name proj-dir))
                  ;;    raw-command))
                  ((functionp raw-command)
                   (funcall raw-command proj-dir))
                  (t
                   (error "Raw command must a function of 1 argument: %s" raw-command))))
           (buf-name (configurable-compilation-buffer-name proj-dir)))

      (cl-assert (cc-command-p cmd))

      (compilation-start cmd
                         configurable-compilation-mode
                         (lambda (_) buf-name)))))

;;;###autoload
(el-patch-feature compile)

(defvar-local compilation-start-timestamp nil
  "Time when current compilation started")

(when-emacs-version (<= 28 it)
  (el-patch-defun compilation-start (command &optional mode name-function highlight-regexp)
    "Run compilation command COMMAND (low level interface).
If COMMAND starts with a cd command, that becomes the `default-directory'.
The rest of the arguments are optional; for them, nil means use the default.

MODE is the major mode to set in the compilation buffer.  Mode
may also be t meaning use `compilation-shell-minor-mode' under `comint-mode'.

If NAME-FUNCTION is non-nil, call it with one argument (the mode name)
to determine the buffer name.  Otherwise, the default is to
reuses the current buffer if it has the proper major mode,
else use or create a buffer with name based on the major mode.

If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight
the matching section of the visited source line; the default is to use the
global value of `compilation-highlight-regexp'.

Returns the compilation buffer created."
    (or mode (setq mode 'compilation-mode))
    (let* ((name-of-mode
            (if (eq mode t)
                "compilation"
              (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))))
           (thisdir default-directory)
           (thisenv compilation-environment)
           (buffer-path (and (local-variable-p 'exec-path) exec-path))
           (buffer-env (and (local-variable-p 'process-environment)
                            process-environment))
           outwin outbuf)
      (with-current-buffer
          (setq outbuf
                (get-buffer-create
                 (compilation-buffer-name name-of-mode mode name-function)))
        (let ((comp-proc (get-buffer-process (current-buffer))))
          (if comp-proc
              (if (or (not (eq (process-status comp-proc) 'run))
                      (eq (process-query-on-exit-flag comp-proc) nil)
                      (yes-or-no-p
                       (format "A %s process is running; kill it? "
                               name-of-mode)))
                  (condition-case ()
                      (progn
                        (interrupt-process comp-proc)
                        (sit-for (el-patch-swap 1 0.5))
                        (delete-process comp-proc))
                    (error nil))
                (error "Cannot have two processes in `%s' at once"
                       (buffer-name)))))
        ;; first transfer directory from where M-x compile was called
        (el-patch-wrap 2 0
          (when (stringp command)
            (setq default-directory thisdir)))
        ;; Make compilation buffer read-only.  The filter can still write it.
        ;; Clear out the compilation buffer.
        (let ((inhibit-read-only t)
              (default-directory thisdir))
          ;; Then evaluate a cd command if any, but don't perform it yet, else
          ;; start-command would do it again through the shell: (cd "..") AND
          ;; sh -c "cd ..; make"
          (el-patch-let ((orig (cd (cond
                                     ((not (string-match "\\`\\s *cd\\(?:\\s +\\(\\S +?\\|'[^']*'\\|\"\\(?:[^\"`$\\]\\|\\\\.\\)*\"\\)\\)?\\s *[;&\n]"
                                                         command))
                                      default-directory)
                                     ((not (match-end 1)) "~")
                                     ((eq (aref command (match-beginning 1)) ?\')
                                      (substring command (1+ (match-beginning 1))
                                                 (1- (match-end 1))))
                                     ((eq (aref command (match-beginning 1)) ?\")
                                      (replace-regexp-in-string
                                       "\\\\\\(.\\)" "\\1"
                                       (substring command (1+ (match-beginning 1))
                                                  (1- (match-end 1)))))
                                     ;; Try globbing as well (bug#15417).
                                     (t (let* ((substituted-dir
                                                (substitute-env-vars (match-string 1 command)))
                                               ;; FIXME: This also tries to expand `*' that were
                                               ;; introduced by the envvar expansion!
                                               (expanded-dir
                                                (file-expand-wildcards substituted-dir)))
                                          (if (= (length expanded-dir) 1)
                                              (car expanded-dir)
                                            substituted-dir)))))))
            (el-patch-swap
              orig
              (cond
                ((stringp command)
                 orig)
                ((cc-command-p command)
                 (cd (cc-command-dir command)))
                (t
                 (error "Unexpected compilation command: %s" command)))))
          (erase-buffer)
          ;; Select the desired mode.
          (if (not (eq mode t))
              (progn
                (buffer-disable-undo)
                (funcall mode))
            (setq buffer-read-only nil)
            (with-no-warnings (comint-mode))
            (compilation-shell-minor-mode))
          ;; Remember the original dir, so we can use it when we recompile.
          ;; default-directory' can't be used reliably for that because it may be
          ;; affected by the special handling of "cd ...;".
          ;; NB: must be done after (funcall mode) as that resets local variables
          (setq-local compilation-directory (el-patch-wrap 2 1
                                              (if (stringp command)
                                                  thisdir
                                                default-directory)))
          (setq-local compilation-environment
                      (el-patch-swap
                        thisenv
                        (append (cc-command-env command)
                                thisenv)))
          (if buffer-path
              (setq-local exec-path buffer-path)
            (kill-local-variable 'exec-path))
          (if buffer-env
              (setq-local process-environment buffer-env)
            (kill-local-variable 'process-environment))
          (if highlight-regexp
              (setq-local compilation-highlight-regexp highlight-regexp))
          (if (or compilation-auto-jump-to-first-error
                  (eq compilation-scroll-output 'first-error))
              (setq-local compilation-auto-jump-to-next t))
          ;; Output a mode setter, for saving and later reloading this buffer.
          (el-patch-add
            (setf compilation-start-timestamp (current-time)))
          (insert (el-patch-wrap 1 0
                    (configurable-compilation--unimportant-text
                     "-*- mode: "))
                  (el-patch-wrap 1 0
                    (configurable-compilation--unimportant-text
                     name-of-mode))
                  (el-patch-wrap 1 0
                    (configurable-compilation--unimportant-text
                     "; default-directory: "))
                  (el-patch-wrap 1 0
                    (configurable-compilation--unimportant-text
                     (prin1-to-string (abbreviate-file-name default-directory))))
                  (el-patch-wrap 1 0
                    (configurable-compilation--unimportant-text
                     " -*-\n"))
                  (el-patch-swap
                    (format "%s started at %s\n\n"
                            mode-name
                            (substring (current-time-string) 0 19))
                    (configurable-compilation--unimportant-text
                     (format "%s started at %s\n\n"
                             mode-name
                             (configurable-compilation--format-timestamp compilation-start-timestamp))))
                  (el-patch-swap
                    command
                    (cond
                      ((stringp command)
                       command)
                      ((cc-command-p command)
                       (cc-command-pretty-cmd command))))
                  "\n")
          ;; Mark the end of the header so that we don't interpret
          ;; anything in it as an error.
          (put-text-property (1- (point)) (point) 'compilation-header-end t)
          (setq thisdir default-directory))
        (set-buffer-modified-p nil))
      ;; Pop up the compilation buffer.
      ;; https://lists.gnu.org/r/emacs-devel/2007-11/msg01638.html
      (setq outwin (display-buffer outbuf '(nil (allow-no-window . t))))
      (with-current-buffer outbuf
        (let ((process-environment
               (append
                compilation-environment
                (and (derived-mode-p 'comint-mode)
                     (comint-term-environment))
                (list (format "INSIDE_EMACS=%s,compile" emacs-version))
                (copy-sequence process-environment))))
          (setq-local compilation-arguments
                      (list command mode name-function highlight-regexp))
          (setq-local revert-buffer-function 'compilation-revert-buffer)
          (and outwin
               ;; Forcing the window-start overrides the usual redisplay
               ;; feature of bringing point into view, so setting the
               ;; window-start to top of the buffer risks losing the
               ;; effect of moving point to EOB below, per
               ;; compilation-scroll-output, if the command is long
               ;; enough to push point outside of the window.  This
               ;; could happen, e.g., in `rgrep'.
               (not compilation-scroll-output)
               (set-window-start outwin (point-min)))

          ;; Position point as the user will see it.
          (let ((desired-visible-point
                 ;; Put it at the end if `compilation-scroll-output' is set.
                 (if compilation-scroll-output
                     (point-max)
                   ;; Normally put it at the top.
                   (point-min))))
            (goto-char desired-visible-point)
            (when (and outwin (not (eq outwin (selected-window))))
              (set-window-point outwin desired-visible-point)))

          ;; The setup function is called before compilation-set-window-height
          ;; so it can set the compilation-window-height buffer locally.
          (if compilation-process-setup-function
              (funcall compilation-process-setup-function))
          (and outwin (compilation-set-window-height outwin))
          ;; Start the compilation.
          (if (fboundp 'make-process)
              (let ((proc
                     (if (eq mode t)
                         ;; On remote hosts, the local `shell-file-name'
                         ;; might be useless.
                         (with-connection-local-variables
                          ;; comint uses `start-file-process'.
                          (get-buffer-process
                           (with-no-warnings
                             (comint-exec
                              outbuf
                              (downcase mode-name)
                              shell-file-name
                              nil
                              (el-patch-swap
                                `(,shell-command-switch ,command)
                                (list shell-command-switch
                                      (cond
                                        ((stringp command)
                                         command)
                                        ((cc-command-p command)
                                         (mapconcat #'identity (cc-command-cmd command) " ")))))))))
                       (el-patch-let ((orig
                                       (start-file-process-shell-command
                                        (downcase mode-name)
                                        outbuf
                                        command)))
                         (el-patch-swap
                           orig
                           (cond
                             ((stringp command)
                              orig)
                             ((cc-command-p command)
                              (cd (cc-command-dir command))
                              (let ((command-line (cc-command-cmd command)))
                                (apply #'start-file-process
                                       (concat "compilation for " (cc-command-dir command))
                                       (current-buffer)
                                       (car command-line)
                                       (cdr command-line))))))))))
                ;; Make the buffer's mode line show process state.
                (setq mode-line-process
                      '((:propertize ":%s" face compilation-mode-line-run)
                        compilation-mode-line-errors))

                ;; Set the process as killable without query by default.
                ;; This allows us to start a new compilation without
                ;; getting prompted.
                (when compilation-always-kill
                  (set-process-query-on-exit-flag proc nil))

                (set-process-sentinel proc #'compilation-sentinel)
                (unless (eq mode t)
                  ;; Keep the comint filter, since it's needed for proper
                  ;; handling of the prompts.
                  (set-process-filter proc #'compilation-filter))
                ;; Use (point-max) here so that output comes in
                ;; after the initial text,
                ;; regardless of where the user sees point.
                (set-marker (process-mark proc) (point-max) outbuf)
                (when compilation-disable-input
                  (condition-case nil
                      (process-send-eof proc)
                    ;; The process may have exited already.
                    (error nil)))
                (run-hook-with-args 'compilation-start-hook proc)
                (compilation--update-in-progress-mode-line)
                (push proc compilation-in-progress))
            ;; No asynchronous processes available.
            (message "Executing `%s'..." command)
            ;; Fake mode line display as if `start-process' were run.
            (setq mode-line-process
                  '((:propertize ":run" face compilation-mode-line-run)
                    compilation-mode-line-errors))
            (force-mode-line-update)
            (sit-for 0)                 ; Force redisplay
            (save-excursion
              ;; Insert the output at the end, after the initial text,
              ;; regardless of where the user sees point.
              (goto-char (point-max))
              (let* ((inhibit-read-only t) ; call-process needs to modify outbuf
                     (compilation-filter-start (point))
                     (status (call-process shell-file-name nil outbuf nil "-c"
                                           command)))
                (run-hooks 'compilation-filter-hook)
                (cond ((numberp status)
                       (compilation-handle-exit
                        'exit status
                        (if (zerop status)
                            "finished\n"
                          (format "exited abnormally with code %d\n" status))))
                      ((stringp status)
                       (compilation-handle-exit 'signal status
                                                (concat status "\n")))
                      (t
                       (compilation-handle-exit 'bizarre status status)))))
            (set-buffer-modified-p nil)
            (message "Executing `%s'...done" command)))
        ;; Now finally cd to where the shell started make/grep/...
        (el-patch-wrap 2 0
          (when (stringp command)
            (setq default-directory thisdir)))
        ;; The following form selected outwin ever since revision 1.183,
        ;; so possibly messing up point in some other window (bug#1073).
        ;; Moved into the scope of with-current-buffer, though still with
        ;; complete disregard for the case when compilation-scroll-output
        ;; equals 'first-error (martin 2008-10-04).
        (when compilation-scroll-output
          (goto-char (point-max))))

      ;; Make it so the next C-x ` will use this buffer.
      (setq next-error-last-buffer outbuf)))

  (el-patch-defun compilation-handle-exit (process-status exit-status msg)
    "Write MSG in the current buffer and hack its `mode-line-process'."
    (let ((inhibit-read-only t)
          (status (if compilation-exit-message-function
                      (funcall compilation-exit-message-function
                               process-status exit-status msg)
                    (cons msg exit-status)))
          (omax (point-max))
          (opoint (point))
          (cur-buffer (current-buffer))
          (el-patch-add
            (end-time nil)))
      ;; Record where we put the message, so we can ignore it later on.
      (goto-char omax)
      (el-patch-swap
        (insert ?\n mode-name " " (car status))
        (insert
         (configurable-compilation--unimportant-text
          (concat "\n" mode-name " " (car status)))))
      (if (and (numberp compilation-window-height)
               (zerop compilation-window-height))
          (message "%s" (cdr status)))
      (if (bolp)
          (forward-char -1))
      (el-patch-add
        (setf end-time (current-time)))
      (el-patch-swap
        (insert " at " (substring (current-time-string) 0 19))
        (insert (configurable-compilation--unimportant-text
                 " at ")
                (configurable-compilation--unimportant-text
                 (configurable-compilation--format-timestamp end-time))
                (configurable-compilation--unimportant-text
                 " and took ")
                (configurable-compilation--unimportant-text
                 (configurable-compilation--format-duration compilation-start-timestamp end-time))))
      (goto-char (point-max))
      ;; Prevent that message from being recognized as a compilation error.
      (add-text-properties omax (point)
                           (append '(compilation-handle-exit t) nil))
      (setq mode-line-process
            (list
             (let ((out-string (format ":%s [%s]" process-status (cdr status)))
                   (msg (format "%s %s" mode-name
                                (replace-regexp-in-string "\n?$" ""
                                                          (car status)))))
               (message "%s" msg)
               (propertize out-string
                           'help-echo msg
                           'face (if (> exit-status 0)
                                     'compilation-mode-line-fail
                                   'compilation-mode-line-exit)))
             compilation-mode-line-errors))
      ;; Force mode line redisplay soon.
      (force-mode-line-update)
      (if (and opoint (< opoint omax))
          (goto-char opoint))
      (run-hook-with-args 'compilation-finish-functions cur-buffer msg))))

(provide 'configurable-compilation)

;; Local Variables:
;; End:

;; configurable-compilation.el ends here
