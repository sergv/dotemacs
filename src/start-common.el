;; start-common.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  3 March 2026
;; Description:

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
    dante-repl-mode

    makefile-automake-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    makefile-mode
    makefile-makepp-mode

    magit-revision-mode
    magit-reflog-mode
    magit-refs-mode
    magit-status-mode ))

(defun vim:bind-local-keymaps ()
  (setf vim-normal-mode-local-keymap              (make-sparse-keymap)

        vim-visual-mode-local-keymap              (make-sparse-keymap)
        vim-insert-mode-local-keymap              (make-sparse-keymap)
        vim-operator-pending-mode-local-keymap    (make-sparse-keymap)
        vim-motion-mode-local-keymap              (make-sparse-keymap)))

(cl-defun init-common (&key (use-yasnippet t)
                            (use-comment t)
                            (use-fci t)
                            (use-whitespace nil) ;; can be t, nil, 'tabs-only, 'tabs-and-trailing-only
                            (use-render-formula nil)
                            (use-hl-line t)
                            (enable-backup t)
                            (hl-parens-backend 'hl-paren) ;; can be 'hl-paren
                            (typography t)
                            (smerge t))
  "General set up for editing. Arguments meaning:
"
  (hl-line-mode (if use-hl-line +1 -1))

  (when smerge
    (smerge-mode +1))

  (when use-comment
    (comment-util-mode 1))

  (set-up-paredit)

  (unless enable-backup
    (backups-ignore-current-buffer!))

  ;; (set-buffer-file-coding-system 'utf-8-unix)

  (when use-yasnippet
    (yas-minor-mode-on))

  (when use-whitespace
    (pcase use-whitespace
      (`tabs-and-trailing-only
       (setq-local whitespace-style '(face tabs trailing)))
      (`tabs-only
       (setq-local whitespace-style '(face tabs)))
      (_
       (when (memq major-mode +do-not-track-long-lines-modes+)
         (error "Shouldn't have enabled whitespace-mode in %s" major-mode))))
    (whitespace-mode 1))

  (when use-render-formula
    (render-formula-mode 1))

  (vim:bind-local-keymaps)

  (when use-fci
    (setf display-fill-column-indicator-column 100)
    (display-fill-column-indicator-mode
     (if (memq major-mode +do-not-track-long-lines-modes+)
         -1
       +1)))
  (pcase hl-parens-backend
    (`hl-paren
     (setup-hl-paren))
    (_
     (error "Invalid values for :hl-parens-backend argument: %s" hl-parens-backend)))

  (electric-quote-local-mode (if typography +1 -1)))

(cl-defun init-repl (&key (show-directory nil)
                          (bind-return t)
                          (create-keymaps nil)
                          (bind-vim:motion-current-line t))
  (use-repl-modeline :show-directory show-directory)

  (setq-local vim-do-not-adjust-point t
              vim--insert-mode-exit-move-point 'dont-move-at-line-end
              global-auto-revert-ignore-buffer t)
  (emacs-forget-buffer-process)

  (setup-hl-paren)
  (set-up-paredit)

  (when create-keymaps
    (vim:bind-local-keymaps))

  (when bind-vim:motion-current-line
    (if vim-operator-pending-mode-local-keymap
        (def-keys-for-map vim-operator-pending-mode-local-keymap
          ("c" vim:motion-current-line:interactive))
      (message "init-repl warning: vim-operator-pending-mode-local-keymap is nil, \"c\" not bound in buffer %s"
               (current-buffer))))

  (let ((keymaps
         (cond ((keymapp bind-return)
                (list bind-return))
               ((and bind-return
                     (consp bind-return)
                     (keymapp (car bind-return)))
                bind-return)
               (bind-return
                (list vim-normal-mode-local-keymap
                      vim-insert-mode-local-keymap)))))
    (dolist (km keymaps)
      (def-keys-for-map km
        ("<return>"   comint-send-input)
        ("C-<return>" newline-and-indent)))))

(cl-defun bind-tab-keys (tab-binding
                         backtab-binding
                         &key
                         (enable-yasnippet nil)
                         (yasnippet-fallback nil))
  (let ((keymaps (list vim-normal-mode-local-keymap
                       vim-insert-mode-local-keymap)))
    (when tab-binding
      (if enable-yasnippet
          (progn
            (setq-local yas-expand-fallback (or yasnippet-fallback tab-binding))
            (dolist (kmap keymaps)
              (define-key kmap (kbd "<tab>") #'yas-expand-or-fallback)))
        (dolist (kmap keymaps)
          (define-key kmap (kbd "<tab>") tab-binding))))
    (when backtab-binding
      (dolist (kmap keymaps)
        (dolist (binding (list (kbd "<backtab>")
                               (kbd "S-<tab>")
                               (kbd "S-<iso-lefttab>")))
          (define-key kmap binding backtab-binding))))))

(provide 'start-common)

;; Local Variables:
;; End:

;; start-common.el ends here
