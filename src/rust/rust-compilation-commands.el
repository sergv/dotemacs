;; rust-compilation-commands.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 27 December 2019
;; Description:

(declare-function rust-get-compilation-buffer-name "rust-setup")

(eval-when-compile
  (require 'set-up-platform)
  (require 'macro-util)
  (defvar compilation-command))

(require 'persistent-sessions-global-vars)

(require 'configurable-compilation)
(require 'rust-mode)
(require 's)

(defun rust-compilation--make-cargo-build-command-presets (target-dir)
  (let ((cargo-command
         (lambda (env cmd &rest args)
           (when target-dir
             (setf args (cons "--target-dir" (cons target-dir args))))

           (setf args (cons rust-cargo-bin (cons cmd args)))

           (lambda (proj-dir)
             (make-cc-command args env proj-dir (s-join " " args))))))
    (mapcan (lambda (entry)
              (let ((target (car entry)))
                (if (listp target)
                    (--map (cons it (cdr entry)) target)
                  (list entry))))
            `((build . ,(funcall cargo-command nil "build" "--color=always"))
              (test .  ,(funcall cargo-command '("RUST_BACKTRACE=1") "test" "--color=always"))))))

(defvar rust-compilation-cargo-build-command-default-presets
  (rust-compilation--make-cargo-build-command-presets (fold-platform-os-type "/tmp/target" nil)))

(defvar rust-compile--build-presets-history nil)
(sessions-mark-global-var-for-save 'rust-compile--build-presets-history)

(defvar-local rust-compile-command nil
  "Variable to configure via file local variables to set custom compilation command for current file.")
(put 'rust-compile-command 'safe-local-variable #'stringp)

(defun rust-compilation-commands-install! (proj)
  ;; When ‘rust-compile-command’ is set via local variables we don’t see it here when we’re called
  ;; by ‘rust-setup’. So checking whether it’s non-nil has to be delayed until runtime.
  (setq-local compilation-command 'rust-compile-command)
  (let* ((tmp (eproj-query/fold-build-dir
               proj
               (lambda ()
                 (list rust-compilation-cargo-build-command-default-presets
                       nil
                       flycheck-cargo--default-check-args))
               (lambda (dir)
                 (list (rust-compilation--make-cargo-build-command-presets dir)
                       (if dir
                           (concat dir "/rls")
                         nil)
                       (flycheck-cargo--make-check-args dir)))))
         (presets (first tmp))
         (target-dir (second tmp))
         (check-args (third tmp)))

    (configurable-compilation-install-command-presets!
     presets
     'rust-compile--build-presets-history
     'rust-compilation-mode)
    (setq-local lsp-rust-target-dir target-dir
                flycheck-cargo-check-args check-args))

  (vim-local-emap "compile"  'vim:rust-compile)
  (vim-local-emap "c"        'vim:rust-compile)
  (vim-local-emap "ccompile" 'vim:rust-compile-choosing-command)
  (vim-local-emap "cc"       'vim:rust-compile-choosing-command)

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    (("C-m" "<f9>") vim:rust-compile:interactive)
    ("<return>"     newline-and-indent)))

(vim-defcmd vim:rust-compile (nonrepeatable)
  (configurable-compilation-start nil))
(vim-defcmd vim:rust-compile-choosing-command (nonrepeatable)
  (configurable-compilation-start t))

(provide 'rust-compilation-commands)

;; Local Variables:
;; End:

;; rust-compilation-commands.el ends here
