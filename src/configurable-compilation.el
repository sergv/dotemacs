;; configurable-compilation.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 27 December 2019
;; Description:

(eval-when-compile
  (require 'macro-util))

(provide 'cmdline)
(require 'el-patch)
(require 'eproj)
(require 'persistent-sessions-global-vars)

(require 'configurable-compilation-baseline-emacs-fixes)

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
    (puthash 'haskell-cabal-config-mode 'haskell-mode tbl)
    (puthash 'haskell-cabal-project-mode 'haskell-mode tbl)
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

(defun make-optional-nix-cc-command (full-command-line env proj-dir)
  (let ((args (nix-maybe-call-via-flakes-exe-args
               (car full-command-line)
               (cdr full-command-line)
               proj-dir)))
    (make-cc-command (cmdline-to-executable-command args)
                     env
                     proj-dir
                     (cmdline-to-pretty-command args))))

(defun make-cc-command (full-command-line env dir pretty-cmd)
  (cl-assert (not (null full-command-line)))
  (cl-assert (listp full-command-line))
  (cl-assert (-all? #'stringp full-command-line))
  (cl-assert (or (null env) (and (listp env) (-all? #'stringp env))))
  (cl-assert (stringp dir))
  (cl-assert (file-directory-p dir))
  (cl-assert (stringp pretty-cmd))
  (make--cc-command :cmd full-command-line
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
            (abbreviate-file-name
             (or
              (when-let ((epr (eproj-get-project-for-buf-lax (current-buffer))))
                (eproj-project/root epr))
              (when (or (derived-mode-p 'haskell-mode)
                        (derived-mode-p 'haskell-ts-base-mode))
                (haskell-misc-get-project-root))
              (when (derived-mode-p 'rust-ts-mode)
                (if configurable-compilation--cached-rust-project-root
                    configurable-compilation--cached-rust-project-root
                  (when-let ((buf (buffer-file-name))
                             (manifest (flycheck-rust-find-manifest buf)))
                    (setq-local configurable-compilation--cached-rust-project-root
                                (file-name-directory manifest)))))
              default-directory)))))

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

(provide 'configurable-compilation)

;; Local Variables:
;; End:

;; configurable-compilation.el ends here
