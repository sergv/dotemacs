;; configurable-compilation.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 27 December 2019
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'eproj)
(require 'persistent-sessions-global-vars)

(declare-function flycheck-rust-find-manifest "flycheck-rust")

(defvar-local configurable-compilation-command-presets nil
  "Alist of (<symbol> . <string>) pairs where symbol is the
user-visible name of the preset and string is the command to
execute.")

(defvar configurable-compilation-last-command (make-hash-table :test #'eq)
  "Mapping between major modes and last chosen compilation command.")

(sessions-mark-global-var-for-save 'configurable-compilation-last-command)

(defvar-local configurable-compilation-mode nil
  "Symbol - mode to enable in the compilation buffer.")

(defvar-local configurable-compilation-make-buffer-name nil
  "Function of one argument (mode) that should return buffer name to use as string.")

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

(defun configurable-compilation-install-command-presets! (presets history-var compilation-mode make-buffer-name)
  (cl-assert (--all? (and (consp it)
                          (symbolp (car it))
                          (stringp (cdr it)))
                     presets))
  (cl-assert (symbolp history-var))
  (cl-assert (symbolp compilation-mode))
  (setq-local configurable-compilation-command-presets presets)
  (let ((effective-major-mode
         (configurable-compilation--resolve-synonym-modes major-mode)))
    (unless (gethash effective-major-mode configurable-compilation-last-command)
      (puthash effective-major-mode
               (let ((presets (configurable-compilation--get-presets)))
                 (or (cdr-safe (assq 'build presets))
                     (cdar-safe presets)
                     (error "Failed to get default build preset from %s" configurable-compilation-command-presets)))
               configurable-compilation-last-command)))
  (setq-local configurable-compilation-history-var history-var
              configurable-compilation-mode compilation-mode
              configurable-compilation-make-buffer-name make-buffer-name))

(defvar-local configurable-compilation--cached-rust-project-root nil)

(defun configurable-compilation-start (&optional edit-command)
  "Run a compile command for the current Haskell buffer."
  (interactive "P")
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let* ((proj-dir (or (when (derived-mode-p 'haskell-mode)
                         (haskell-misc-get-project-root))
                       (when (derived-mode-p 'rust-mode)
                         (if configurable-compilation--cached-rust-project-root
                             configurable-compilation--cached-rust-project-root
                           (when-let ((buf (buffer-file-name))
                                      (manifest (flycheck-rust-find-manifest buf)))
                             (setq-local configurable-compilation--cached-rust-project-root
                                         (file-name-directory manifest)))))
                       (when-let ((epr (eproj-get-project-for-buf-lax (current-buffer))))
                         (eproj-project/root epr))
                       default-directory))
         (effective-major-mode
          (configurable-compilation--resolve-synonym-modes major-mode))
         (raw-command
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
                (puthash effective-major-mode command configurable-compilation-last-command)
                command)
            (gethash effective-major-mode configurable-compilation-last-command)))
         (command (if proj-dir
                      (format raw-command (expand-file-name proj-dir))
                    raw-command)))

    (compilation-start command
                       configurable-compilation-mode
                       configurable-compilation-make-buffer-name)))

(provide 'configurable-compilation)

;; Local Variables:
;; End:

;; configurable-compilation.el ends here
