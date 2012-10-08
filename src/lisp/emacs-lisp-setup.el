;; emacs-lisp-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  8 December 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'general-lisp-setup)
(require 'slime-setup)
(require 'find-func)
(require 'emacs-lisp-abbrev+)
(require 'ansi-lisp-highlight)
(require 'elisp-slime-nav)

(define-common-lisp-style "emacs"
  "My custom indent style for emacs lisp."
  (:inherit "my-style")

  (:indentation
   (aif (as if))
   (condition-case (4 4 &body))
   (def-keys-for-map (&body))
   (with-temp-buffer (&body))
   (while (4 &body))
   (rxx (as let))
   (letrec (as let))
   (redefun (as defun))
   (edefun (as defun))
   (define-print-info-skeleton (4 &body))
   (define-lisp-print-info-skeleton (as define-print-info-skeleton))
   (define-repeated-function (4 &body))
   (define-switch-to-interpreter (4 (&whole 2 &rest 1) &rest 2))
   (check-for-stop (4 &rest 1))
   (skip-and-check (as check-for-stop))
   (moving-one-item-forward (4 4 &body))
   (forward-sexp-with-bounds (4 4 &body))
   (defstruct* (as defstruct))
   (sexpy-define-pattern-fontifier (4 &rest 1))
   (with-current-frame (as with-current-buffer))
   (with-disabled-undo (1))
   (with-preserved-buffer-modified-p (1))
   (with-inhibited-modification-hooks (1))
   (with-inhibited-readonly (1))
   (ert-deftest (as defun))
   (with-hidden-comments (1))
   (with-hidden-cloze-hints (1))
   (with-hidden-cloze-text (1))))





(defun expand-last-macro ()
  (interactive)
  ;; taken from pp.el
  (insert (pp-to-string (macroexpand (pp-last-sexp)))))

(defun expand-last-macro-all ()
  (interactive)
  ;; taken from pp.el
  (insert (pp-to-string (macroexpand-all (pp-last-sexp)))))


(defun emacs-lisp-setup ()
  (lisp-setup)

  (ansi-lisp-highlight-emacs-keywords)

  (setf autopair-extra-pairs
        '(:comment
          ((?` . ?'))
          :string
          ((?` . ?'))))
  (autopair-mode t)

  ;; (setf common-lisp-style "emacs")
  (common-lisp-set-style "emacs")

  (eldoc-mode 1)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("j"     eval-last-sexp)
    ("J"     eval-print-last-sexp-unlimited-length)
    (", m"   expand-last-macro)
    (", M"   expand-last-macro-all)

    ("M-:"   icicle-pp-eval-expression)
    ("M-."   elisp-slime-nav-find-elisp-thing-at-point)
    ("M-,"   pop-tag-mark)
    ("M-/"   lisp-complete-symbol)

    ("<tab>" indent-for-tab-command)
    ("<f9>"  elisp-compile-and-move))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("j"   eval-region))

  (def-keys-for-map vim:insert-mode-local-keymap
    ;; ("C-SPC" lisp-complete-symbol)
    ("<tab>" indent-for-tab-command)
    ("M-/"   lisp-complete-symbol))

  (def-keys-for-map read-expression-map
    ("<tab>" lisp-complete-symbol)
    ("M-/"   lisp-complete-symbol)
    ("C-w"   backward-delete-word)
    ("C-S-w" backward-delete-word*))

  ;; handled by edebug-mode-map
  ;; (def-keys-for-map emacs-lisp-mode-map
  ;;   ("<f1>"  edebug-step-mode))

  ;; should use global after-save-hook because of
  ;; backups
  (add-hook 'after-save-hook #'elisp-compile-and-move)
  (emacs-lisp-abbrev+-setup))

(eval-after-load "edebug"
                 '(progn
                   (def-keys-for-map edebug-mode-map
                     ("<f1>"  edebug-step-mode))))

(defun eval-print-last-sexp-unlimited-length ()
  (interactive)
  (let ((eval-expression-print-length nil)
        (print-length nil)
        (eval-expression-print-level nil))
    (eval-print-last-sexp)))

;;;;

(defconst elisp-do-not-compile-files
  '("profile-dotemacs.el"
    "recompile.el"
    "color-theme-darkspectrum.el"
    "color-theme-example.el"
    "color-theme-github.el"
    "color-theme-library.el"
    "color-theme-solarized.el")
  "List of file names that should not be compiled into bytecode.")

(defun elisp-compile-filep (filename)
  "Return T if file FILENAME should be bytecompiled."
  (let ((file (file-name-nondirectory filename)))
    (not (some #'(lambda (x)
                   (string= x file))
               elisp-do-not-compile-files))))


(defun elisp-move-filep (filename)
  "Return T if file FILENAME should be moved to `+bytecode-lib+'."
  ;; (let ((file (file-name-nondirectory filename)))
  ;;   (not (some #'(lambda (x)
  ;;                  (string= x file))
  ;;              *elisp-do-not-move-files*)))

  ;; do not move any files while I'm using /src directory
  ;; for loading
  nil)

(defun elisp-compile-and-move ()
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (when (and (eq major-mode 'emacs-lisp-mode)
             (not no-byte-compile))
    (save-excursion
     (let ((file (buffer-file-name))
           (window-config (current-window-configuration)))
       (cond
         ((not (elisp-compile-filep file))
          nil)
         ((not (elisp-move-filep file))
          (if (byte-compile-file file)
            (progn
              (kill-buffer "*Compile-Log*")
              ;; restore window config
              (set-window-configuration window-config))
            (message "Compilation errors, check out *Compile-log*")))

         ((string= "el"
                   (file-name-extension file))

          (let* ((compiled-file (byte-compile-dest-file file))
                 (dest-file (path-concat +bytecode-lib+
                                         (file-name-nondirectory compiled-file)))
                 (byte-compile-error-on-warn t)
                 (byte-compile-warning-types
                   (remove-if (lambda (x)
                                (memq x '(absolete
                                          cl-functions)))
                              byte-compile-warning-types)))
            (cond
              ((byte-compile-file file)
               (cond
                 ((file-exists-p +bytecode-lib+)
                  (when (file-exists-p dest-file)
                    (delete-file dest-file))
                  (message "Moving compiled file to %s" dest-file)
                  (rename-file compiled-file dest-file))
                 (t
                  ;; can't reach dest-file so remove
                  ;; currently produced file to reduce garbage
                  (delete-file compiled-file)))
               (kill-buffer "*Compile-Log*")
               (set-window-configuration window-config))
              (t
               (message "Compilation errors, check out *Compile-log*"))))))))))


(defun debugger-setup ()
  (def-keys-for-map debugger-mode-map
    +vi-essential-keys+
    +control-x-prefix+
    +vim-special-keys+
    +vim-word-motion-keys+
    ("<escape>" exit-recursive-edit)))

(add-hook 'debugger-mode-hook #'debugger-setup)

(provide 'emacs-lisp-setup)

;; Local Variables:
;; End:

;; emacs-lisp-setup.el ends here
