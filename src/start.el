;; dotemacs.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'set-up-platform)
(load-library "set-up-environment-variables")
(require 'set-up-paths)
(load-library "set-up-tmp-paths")
(unless noninteractive
  (require 'set-up-font))

(require 'base-emacs-fixes)

(load-library "foreign-setup")

(require 'cycle-on-lines)
(require 'common)
(load-library "persistent-store")
(persistent-store-init)

(load-library "backups")
(require 'mode-line-setup)
(require 'emacs-general-conf)

;; Autogenerated file with autoloads for everything
(require 'local-autoloads)
(require 'org-mode-autoload)

(require 'smartparens-setup)

(setq compilation-auto-jump-to-first-error nil
      whitespace-style '(face tabs)
      ;; whitespace-line-column 81
      ;; whitespace-style '(face lines-tail tabs)
      whitespace-global-modes nil)

(defconst +do-not-track-long-lines-modes+
  (alist->hash-table
   '((lisp-interaction-mode . t)
     (inferior-scheme-mode . t)
     (prolog-inferior-mode . t)
     (comint-mode . t)
     (inferior-octave-mode . t)
     (python-repl-mode . t)
     (dante-repl-mode . t)

     (makefile-automake-mode . t)
     (makefile-bsdmake-mode . t)
     (makefile-gmake-mode . t)
     (makefile-imake-mode . t)
     (makefile-mode . t)
     (makefile-makepp-mode . t)

     (magit-revision-mode . t)
     (magit-reflog-mode . t)
     (magit-refs-mode . t)
     (magit-status-mode . t))
   #'eq))

(defun vim:bind-local-keymaps ()
  (setf vim:normal-mode-local-keymap              (make-sparse-keymap)

        vim:visual-mode-local-keymap              (make-sparse-keymap)
        vim:insert-mode-local-keymap              (make-sparse-keymap)
        vim:operator-pending-mode-local-keymap    (make-sparse-keymap)
        vim:motion-mode-local-keymap              (make-sparse-keymap)
        vim:complex-command-override-local-keymap (make-sparse-keymap)))

(defun* init-common (&key (use-yasnippet t)
                          (use-comment t)
                          (use-fci t)
                          (use-whitespace nil) ;; can be t, nil, 'tabs-only
                          (use-render-formula nil)
                          (use-hl-line t)
                          (sp-slurp-sexp-insert-space t)
                          (enable-backup t)
                          (hl-parens-backend 'hl-paren) ;; can be 'hl-paren, 'smartparens
                          (typography t))
  (hl-line-mode (if use-hl-line +1 -1))
  (when use-comment
    (comment-util-mode 1))

  (unless enable-backup
    (backups-ignore-current-buffer!))

  ;; (set-buffer-file-coding-system 'utf-8-unix)

  (when use-yasnippet
    (yas-minor-mode-on))

  (when use-whitespace
    (when (and (not (eq? use-whitespace 'tabs-only))
               (gethash major-mode
                        +do-not-track-long-lines-modes+
                        nil))
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
    ("M-<up>"    sp-splice-sexp-killing-backward)
    ("M-<down>"  sp-splice-sexp-killing-forward))

  (when use-fci
    (setf display-fill-column-indicator-column 100)
    (display-fill-column-indicator-mode
     (if (gethash major-mode +do-not-track-long-lines-modes+ nil)
         -1
       +1)))
  (pcase hl-parens-backend
    (`hl-paren
     (setup-hl-paren))
    (`smartparens
     (show-smartparens-mode 1))
    (_
     (error "Invalid values for :hl-parens-backend argument: %s" hl-parens-backend)))

  (electric-quote-local-mode (if typography +1 -1)))

(defun* init-repl (&key (show-directory nil)
                        (bind-return t)
                        (create-keymaps nil)
                        (bind-vim:motion-current-line t))
  (use-repl-modeline :show-directory show-directory)
  (setq-local *vim:do-not-adjust-point* t
              vim:insert-mode-exit-move-point 'dont-move-at-line-end
              global-auto-revert-ignore-buffer t)
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
                       &key
                       (enable-yasnippet nil)
                       (yasnippet-fallback nil))
  (if enable-yasnippet
      (progn
        (setq-local yas-expand-fallback (or yasnippet-fallback tab-binding))
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

(require 'c-like-setup)
(require 'haskell-autoload)
(require 'latex-autoloads)
(require 'rust-autoloads)

(require 'compilation-setup)
(require 'completion-setup)
(require 'hl-paren)
(require 'grep-autoload)
(require 'yasnippet-autoload)
(require 'git-autoload)
(require 'ediff-autoload)
(require 'recentf-setup)
(require 'misc-autoloads)
(require 'flycheck-setup)
(require 'typography-setup)
(require 'eshell-autoload)

(require 'undo-tree-setup)

;; load keys after everything to ensure that nothing will be rebond
;; after it finishes
(require 'keys)
(require 'vim-setup)

;; this is quick-and-dirty autoloading mechanism
(eval-after-load "ibuffer"
  '(progn
     (require 'ibuffer-setup)))

(fortunes-init-scratch-buffer)

(require 'solarized)
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
       (-find #'file-exists?
              (list (expand-file-name "~/user-info.el")
                    (concat +emacs-config-path+ "/src/user-info.el")))))
  (aif user-info-file
      (load-file it)
    (message "user-info.el not found")))

(let ((machine-specific-setup-file
       (-find #'file-exists?
              (list (expand-file-name "~/machine-specific-setup.el")
                    (concat +emacs-config-path+ "/src/machine-specific-setup.el")))))
  (aif machine-specific-setup-file
      (load-file it)
    (message "machine-specific-setup.el not found")))

;; Local Variables:
;; End:

;; dotemacs.el ends here
