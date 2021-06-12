;; rust-compilation-commands.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 27 December 2019
;; Description:

(declare-function rust-get-compilation-buffer-name "rust-setup")
(defvar compilation-command nil)

(require 'configurable-compilation)
(require 'rust-mode)
(require 's)

(defvar rust-compilation-cargo-build-command-presets
  (let* ((tmp (fold-platform-os-type "/tmp/target" nil))
         (cargo-command
          (lambda (env cmd &rest args)
            (format "cd \"%%s\" &&%s %s %s%s %s"
                    (if env
                        (concat " " env)
                      "")
                    rust-cargo-bin
                    cmd
                    (if tmp
                        (concat " --target-dir=" tmp)
                      "")
                    (s-join " " args)))))
    (-mapcat (lambda (entry)                 ;
               (let ((target (car entry)))
                 (if (listp target)
                     (--map (cons it (cdr entry)) target)
                   (list entry))))
             `((build . ,(funcall cargo-command nil "build" "--color=always"))
               (test .  ,(funcall cargo-command "RUST_BACKTRACE=1" "test" "--color=always"))))))

(defvar rust-compile--build-presets-history nil)
(sessions-mark-global-var-for-save 'rust-compile--build-presets-history)

(defvar-local rust-compile-command nil
  "Variable to configure via file local variables to set custom compilation command for current file.")
(put 'rust-compile-command 'safe-local-variable #'stringp)

(defun rust-compilation-commands-install! ()
  ;; When ‘rust-compile-command’ is set via local variables we don’t see it here when we’re called
  ;; by ‘rust-setup’. So checking whether it’s non-nil has to be delayed until runtime.
  (setq-local compilation-command 'rust-compile-command)

  (configurable-compilation-install-command-presets!
   'rust-compilation-cargo-build-command-presets
   'rust-compile--build-presets-history
   'rust-compilation-mode
   #'rust-get-compilation-buffer-name)

  (vim:local-emap "compile"  'vim:rust-compile)
  (vim:local-emap "c"        'vim:rust-compile)
  (vim:local-emap "ccompile" 'vim:rust-compile-choosing-command)
  (vim:local-emap "cc"       'vim:rust-compile-choosing-command)

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    (("C-m" "<f9>") vim:rust-compile)))

(vim:defcmd vim:rust-compile (nonrepeatable)
  (configurable-compilation-start nil))
(vim:defcmd vim:rust-compile-choosing-command (nonrepeatable)
  (configurable-compilation-start t))

(provide 'rust-compilation-commands)

;; Local Variables:
;; End:

;; rust-compilation-commands.el ends here
