;; dotemacs.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(load-library "set-up-platform")
(load-library "set-up-environment-variables")
(load-library "set-up-paths")
(load-library "set-up-font")

(load-library "base-emacs-fixes")

;; ******************************************************************

(load-library "cycle-on-lines")

(load-library "common")
(load-library "persistent-store")
(persistent-store-init)

(load-library "backups")
(load-library "mode-line-setup")
(load-library "emacs-general-conf")

(load-library "local-autoloads")
(load-library "org-mode-autoload")
(load-library "persistent-sessions-autoloads")

(load-library "smartparens-setup")

;; ******************************************************************

(setq compilation-auto-jump-to-first-error nil
      whitespace-style '(face tabs)
      ;; whitespace-line-column 81
      ;; whitespace-style '(face lines-tail tabs)
      whitespace-global-modes nil)

(defconst +do-not-track-long-lines-modes+
  '(lisp-interaction-mode
    inferior-scheme-mode
    prolog-inferior-mode
    comint-mode
    inferior-octave-mode
    python-repl-mode
    inferior-haskell-mode

    makefile-automake-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    makefile-mode
    makefile-makepp-mode))

(defun vim:bind-local-keymaps ()
  (setf vim:normal-mode-local-keymap              (make-keymap)
        vim:visual-mode-local-keymap              (make-sparse-keymap)
        vim:insert-mode-local-keymap              (make-keymap)
        vim:operator-pending-mode-local-keymap    (make-sparse-keymap)
        vim:motion-mode-local-keymap              (make-sparse-keymap)
        vim:complex-command-override-local-keymap (make-sparse-keymap)))

(defvar disable-fci-mode? nil
  "Variable to control, whether to enable `fci-mode' in new buffers
or not.")

(defmacro with-disabled-fci (&rest body)
  (let ((old-val-var (gensym "Old-value")))
    `(let ((,old-val-var disable-fci-mode?))
       (unwind-protect
           (progn
             (setf disable-fci-mode? t)
             ,@body)
         (setf disable-fci-mode? ,old-val-var)))))

(defun* init-common (&key (use-yasnippet t)
                          (use-comment t)
                          (use-fci t)
                          (use-whitespace nil) ;; can be t, nil, 'tabs-only
                          (use-render-formula nil)
                          (use-hl-line t)
                          (sp-slurp-sexp-insert-space t))
  (hl-line-mode (if use-hl-line +1 -1))
  (when use-comment
    (comment-util-mode 1))

  ;; (set-buffer-file-coding-system 'utf-8-unix)

  (when use-yasnippet
    (yas-minor-mode-on))

  (when use-whitespace
    (when (and (not (eq? use-whitespace 'tabs-only))
               (memq major-mode
                     +do-not-track-long-lines-modes+))
      (error "Shouldn't have enabled whitespace-mode in %s" major-mode))
    (when (eq? use-whitespace 'tabs-only)
      (setq-local whitespace-style '(face tabs)))
    (whitespace-mode 1))

  (when use-render-formula
    (render-formula-mode 1))

  (vim:bind-local-keymaps)

  ;; I should figure what's going on here someday.
  (eval-after-load "smartparens"
    `(progn
       (when (not (eq ,sp-slurp-sexp-insert-space
                      sp-forward-slurp-sexp-insert-space))
         (setq-local sp-forward-slurp-sexp-insert-space ,sp-slurp-sexp-insert-space))))

  ;; bind in vim:normal-mode-local-keymap since
  ;; it will not be bound in vim:normal-mode-keymap because
  ;; everyone needs different versions, e.g. repls, shells
  (def-keys-for-map vim:normal-mode-local-keymap
    ("<return>"  sp-newline))
  (def-keys-for-map vim:insert-mode-local-keymap
    ("C-("       sp-backward-slurp-sexp)
    ("C-)"       sp-forward-slurp-sexp)
    ("M-("       sp-forward-barf-sexp)
    ("M-)"       sp-backward-barf-sexp)

    ("<return>"  sp-newline)

    ("M-?"       sp-convolute-sexp)
    ("C-<left>"  sp-backward-slurp-sexp)
    ("C-<right>" sp-forward-slurp-sexp)
    ("M-<left>"  sp-forward-barf-sexp)
    ("M-<right>" sp-backward-barf-sexp)
    ("M-<up>"    sp-splice-sexp-killing-backward)
    ("M-<down>"  sp-splice-sexp-killing-forward))

  (when (and use-fci
             (not disable-fci-mode?))
    (fci-mode (if (memq major-mode
                        +do-not-track-long-lines-modes+)
                -1
                +1))))

(defun* init-repl (&key (show-directory nil)
                        (bind-return t)
                        (create-keymaps nil)
                        (bind-vim:motion-current-line t))
  (use-repl-modeline :show-directory show-directory)
  (setq-local *vim:do-not-adjust-point* t)
  (setq-local vim:insert-mode-exit-move-point 'dont-move-at-line-end)
  (emacs-forget-buffer-process)

  (when create-keymaps
    (vim:bind-local-keymaps))
  (when bind-vim:motion-current-line
    (if (not (null? vim:operator-pending-mode-local-keymap))
      (def-keys-for-map vim:operator-pending-mode-local-keymap
        ("c" vim:motion-current-line))
      (message "init-repl warning: vim:operator-pending-mode-local-keymap is nil, \"c\" not bound in buffer %s"
               (current-buffer))))

  (cond ((keymapp bind-return)
         (def-keys-for-map bind-return
           ("<return>"   comint-send-input)
           ("C-<return>" sp-newline)))
        ((and (not (null? bind-return))
              (cons? bind-return)
              (keymapp (car bind-return)))
         (dolist (keymap bind-return)
           (def-keys-for-map keymap
             ("<return>"   comint-send-input)
             ("C-<return>" sp-newline))))
        ((not (null? bind-return))
         (def-keys-for-map (vim:normal-mode-local-keymap
                            vim:insert-mode-local-keymap)
           ("<return>"   comint-send-input)
           ("C-<return>" sp-newline)))))

(defun* bind-tab-keys (tab-binding
                       backtab-binding
                       &key (enable-yasnippet nil))
  (if enable-yasnippet
    (progn
      (setq-local yas-expand-fallback tab-binding)
      (def-keys-for-map (vim:normal-mode-local-keymap
                         vim:insert-mode-local-keymap)
        ("<tab>" yas-expand-or-fallback)))
    (dolist (kmap (list vim:normal-mode-local-keymap
                        vim:insert-mode-local-keymap))
      (define-key kmap (kbd "<tab>") tab-binding)))
  (dolist (kmap (list vim:normal-mode-local-keymap
                      vim:insert-mode-local-keymap))
    (dolist (binding (list (kbd "<backtab>")
                           (kbd "S-<tab>")
                           (kbd "S-<iso-lefttab>")))
      (define-key kmap binding backtab-binding))))

(load-library "fci-setup")

;; Autogenerated file with autoloads for everything

(load-library "c-like-setup")
(load-library "haskell-autoload")
(load-library "latex-autoloads")

(load-library "compilation-setup")
(load-library "completion-setup")
(load-library "hl-paren")
(load-library "grep-autoload")
(load-library "dired-autoload")
(load-library "yasnippet-autoload")
(load-library "git-autoload")
(load-library "ediff-autoload")
(load-library "undo-tree-setup")
(load-library "recentf-setup")
(load-library "misc-autoloads")
(load-library "eproj-setup")
(load-library "flycheck-setup")

;; load keys after everything to ensure that nothing will be rebond
;; after it finishes
(load-library "keys")
(load-library "vim-setup")

;; this is quick-and-dirty autoloading mechanism
(eval-after-load "ibuffer"
  '(progn
     (load-library "ibuffer-setup")))

(fortunes-init-scratch-buffer)

(load-library "solarized")
(solarized-dark)

(dolist (func '(downcase-region
                erase-buffer
                eval-expression
                narrow-to-defun
                narrow-to-page
                narrow-to-region
                set-goal-column
                upcase-region
                dired-find-alternate-file))
  (put func 'disabled nil))

(provide 'start)

(unless (featurep 'custom-variables-defined)
  (load-library ".emacs"))


(let ((user-info-file
       (find-if #'file-exists?
                (list (expand-file-name "~/user-info.el")
                      (concat +emacs-config-path+ "/src/user-info.el")))))
  (aif user-info-file
    (load-file it)
    (message "user-info.el not found")))

(let ((machine-specific-setup-file
       (find-if #'file-exists?
                (list (expand-file-name "~/machine-specific-setup.el")
                      (concat +emacs-config-path+ "/src/machine-specific-setup.el")))))
  (aif machine-specific-setup-file
    (load-file it)
    (message "machine-specific-setup.el not found")))

(when (platform-os-type? 'windows)
  (load-library "windows-setup"))

;; Local Variables:
;; End:

;; dotemacs.el ends here
