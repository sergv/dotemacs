;; haskell-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'common)
(require 'comment-util)
(require 'browse-kill-ring-setup)

(require 'align)
(require 'compilation-setup)
(require 'eproj)
(require 'company-eproj)
(require 'shell-setup)
(require 'company-mode-setup)

(require 'haskell-checkers)
(require 'shm)

(require 'haskell-abbrev+)
(require 'haskell-misc)
(require 'haskell-outline)
(require 'haskell-shm)

;; never cache module alist to a file
(setf inferior-haskell-module-alist-file nil)

(defadvice shm-mode-start (after
                           shm-enable-quarantine-display
                           activate
                           compile)
  (setq-local shm-display-quarantine t))

(defadvice shm-mode-stop (after
                          shm-disable-quarantine-display
                          activate
                          compile)
  (setq-local shm-display-quarantine nil))


;; ;; prevent paredit from reindenting Haskell lines
;; (dolist (func '(indent-region
;;                 indent-sexp
;;                 lisp-indent-line ;; ?
;;                 ))
;;   (eval
;;    `(defadvice ,func (around
;;                       ,(util/make-joined-name func "-block-haskell-indentation")
;;                       activate
;;                       compile)
;;       (unless (eq major-mode 'haskell-mode)
;;         ad-do-it))))

(vimmize-motion shm/goto-parent
                :name vim:shm/goto-parent
                :exclusive nil)

(vimmize-motion shm/goto-parent-end
                :name vim:shm/goto-parent-end
                :exclusive nil)

(vim:defcmd vim:ghc-core-create-core (nonrepeatable)
  (ghc-core-create-core))
(vim:defcmd vim:haskell-compile (nonrepeatable)
  (haskell-compile nil))
(vim:defcmd vim:haskell-compile-choosing-command (nonrepeatable)
  (haskell-compile t))
(vim:defcmd vim:haskell-lint (nonrepeatable)
  (haskell-lint))
(vim:defcmd vim:haskell-load-file-into-repl (nonrepeatable)
  (haskell-process-load-file))
(vim:defcmd vim:haskell-interactive-clear-buffer-above-prompt (nonrepeatable)
  (haskell-interactive-clear-buffer-above-prompt))

;; Ghc-mod stuff
;; (defvar-local vim:haskell-check-on-save nil
;;   "Whether to run `ghc-check' on saves.")
;; (vim:defcmd vim:haskell-ghc-init (nonrepeatable)
;;   (ghc-init)
;;   (setq-local vim:haskell-check-on-save t))
;; (vim:defcmd vim:haskell-ghc-check (nonrepeatable)
;;   (ghc-check-syntax)
;;   (setq-local vim:haskell-check-on-save t))
;; (vim:defcmd vim:haskell-ghc-reset (nonrepeatable)
;;   (ghc-reset)
;;   (setq-local vim:haskell-check-on-save nil))
;; (vim:defcmd vim:haskell-ghc-reload (nonrepeatable)
;;   (vim:haskell-ghc-reset)
;;   (ghc-reload))
;;
;; (defun haskell-ghc-mod-check-on-save ()
;;   (when (and vim:haskell-check-on-save
;;              (fboundp #'ghc-check-syntax))
;;     (ignore-errors
;;       (ghc-check-syntax))))

(vim:defcmd vim:haskell-flycheck-run (nonrepeatable)
  (flycheck-buffer))
(vim:defcmd vim:haskell-flycheck-compile (nonrepeatable)
  (call-interactively #'flycheck-compile))
(vim:defcmd vim:haskell-flycheck-configure (nonrepeatable)
  (flycheck-haskell-configure))
(vim:defcmd vim:haskell-flycheck-clear (nonrepeatable)
  (flycheck-clear
   t ;; interrupt running process
   ))
(vim:defcmd vim:haskell-flycheck-list-errors (nonrepeatable)
  (flycheck-list-errors))

(vim:defcmd vim:haskell-navigate-imports (nonrepeatable)
  (haskell-navigate-imports)
  (vim:save-position haskell-navigate-imports-start-point))

(defun haskell-update-eproj-tags-on-save ()
  (ignore-errors
    (eproj-update-buffer-tags)))

(defun haskell-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula nil
               :use-hl-line nil
               :use-whitespace 'tabs-only)
  (company-mode +1)
  (setq-local company-backends '(company-eproj))
  (fontify-conflict-markers)
  (flycheck-mode)
  (add-hook 'after-save-hook #'haskell-update-eproj-tags-on-save nil t)
  ;; (add-hook 'after-save-hook #'haskell-ghc-mod-check-on-save nil t)

  ;; ghci interaction uses comint - same as shell mode
  (turn-on-font-lock)

  ;; (haskell-doc-mode-setup)

  ;; fix vim treatment of words for Haskell
  ;; note: do not include underscore into vim:word as this would cause
  ;; inefficiencies while navigating haskell identifiers
  (setq-local vim:word "[:word:]'")
  (modify-syntax-entry ?\' "w")
  (modify-syntax-entry ?\@ "'")

  (setq-local eproj-symbnav/identifier-type 'haskell-symbol)
  (setq-local indent-region-function #'ignore)
  (setq-local yas-indent-line 'fixed)

  (structured-haskell-mode +1)
  (setq-local indent-line-function #'ignore)
  (setq-local abbrev+-fallback-function #'haskell-abbrev+-fallback-space)

  ;; (turn-on-haskell-simple-indent)

  (let ((offset
         (if-let (hask-offset
                  (ignore-errors
                    (multiple-value-bind (initial-root aux-info)
                        (eproj-get-initial-project-root-and-aux-info
                         (eproj--get-buffer-directory (current-buffer)))
                      (cadr-safe (assq 'haskell-offset aux-info)))))
           (progn
             (assert (integer? hask-offset)
                     nil
                     "haskell-offset in .eproj-info must be an integer, but got %s"
                     hask-offset)
             hask-offset)
           2)))
    (setq-local vim:shift-width       offset)
    (setq-local haskell-indent-offset offset)
    (setq-local haskell-indent-spaces offset)
    (setq-local shm-indent-spaces     offset))

  (setq-local compilation-read-command nil)
  (setq-local compilation-auto-jump-to-first-error nil)
  ;; don't skip any messages
  (setq-local compilation-skip-threshold 0)

  (bind-tab-keys #'haskell-shm-tab-or-indent-relative-forward
                 #'haskell-shm-backtab-or-indent-relative-backward
                 :enable-yasnippet t)

  (vim:local-emap "core"     'vim:ghc-core-create-core)
  (vim:local-emap "compile"  'vim:haskell-compile)
  (vim:local-emap "c"        'vim:haskell-compile)
  (vim:local-emap "ccompile" 'vim:haskell-compile-choosing-command)
  (vim:local-emap "cc"       'vim:haskell-compile-choosing-command)
  (vim:local-emap "hlint"    'vim:haskell-lint)
  (vim:local-emap "load"     'vim:haskell-load-file-into-repl)
  (vim:local-emap "lo"       'vim:haskell-load-file-into-repl)
  (vim:local-emap "l"        'vim:haskell-load-file-into-repl)
  ;; quick check commands
  (vim:local-emap "init"      'vim:haskell-flycheck-configure)
  (vim:local-emap "configure" 'vim:haskell-flycheck-configure)
  (vim:local-emap "conf"      'vim:haskell-flycheck-configure)
  (vim:local-emap "f"         'vim:haskell-flycheck-run)
  (vim:local-emap "ff"        'vim:haskell-flycheck-compile)
  (vim:local-emap "check"     'vim:haskell-flycheck-run)
  (vim:local-emap "ch"        'vim:haskell-flycheck-run)
  (vim:local-emap "clear"     'vim:haskell-flycheck-clear)
  (vim:local-emap "reset"     'vim:haskell-flycheck-clear)
  (vim:local-emap "errors"    'vim:haskell-flycheck-list-errors)
  (vim:local-emap "errs"      'vim:haskell-flycheck-list-errors)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("\\"      vim:haskell-flycheck-run)
    ("j"       vim:haskell-load-file-into-repl)
    ("g c c"   haskell-comment-node)
    ("+"       input-unicode)
    ("SPC SPC" show-ghc-mod-errors-or-switch-to-haskell)
    ("g w"     shm/goto-where)
    ("`"       ghc-display-errors)
    ("C-`"     haskell-lint)
    ("g i"     vim:haskell-navigate-imports)
    ("g I"     haskell-navigate-imports-return)
    ("g <tab>" haskell-reindent-at-point))

  (haskell-bind-shm-bindings :bind-colon t)
  (def-keys-for-map shm-map
    ("("            nil)
    ("["            nil)
    ("{"            nil)
    (")"            nil)
    ("]"            nil)
    ("}"            nil)
    ("C-w"          nil)
    ("M-w"          nil)
    ("C-y"          nil)
    ("M-y"          nil)
    ("M-a"          nil)
    ("M-k"          nil)
    ("C-k"          nil)
    ("<delete>"     nil)
    ("<deletechar>" nil)
    ("C-<return>"   shm/simple-indent-newline-same-col)
    ("TAB"          nil)
    ("<backtab>"    nil))

  (def-keys-for-map (vim:visual-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("(" sp--self-insert-command)
    ("[" sp--self-insert-command)
    ("{" sp--self-insert-command))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("C-w"             shm/backward-kill-word)
    ("C-u"             shm/insert-undefined)
    ("C-<up>"          shm/swing-up)
    ("C-<down>"        shm/swing-down)
    ("C-t"             haskell-compilation-prev-error-other-window)
    ("C-h"             haskell-compilation-next-error-other-window)
    ("C-SPC"           company-complete)

    ("S-<tab>"         nil)
    ("<S-iso-lefttab>" nil)
    ("<return>"        haskell-newline)
    ("<f6>"            haskell-process-load-file)
    ("<f9>"            haskell-compile)
    ("S-<f9>"          haskell-lint))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap)
    ("- ?" ghc-display-errors)
    ("- t" ghc-show-type)
    ("- i" ghc-show-info)
    ("- e" ghc-expand-th)
    ("- m" ghc-insert-module)
    ("- c" ghc-case-split)
    ("- r" ghc-refine)
    ("- a" ghc-auto)
    ("- s" ghc-insert-template-or-signature)

    ("*"   search-for-haskell-symbol-at-point-forward)
    ("C-*" search-for-haskell-symbol-at-point-forward-new-color)
    ("#"   search-for-haskell-symbol-at-point-backward)
    ("C-#" search-for-haskell-symbol-at-point-backward-new-color)
    ("'"   vim:shm/goto-parent)
    ("g t" haskell-node/move-to-topmost-start)
    ("g h" haskell-node/move-to-topmost-end))

  (haskell-define-align-bindings vim:visual-mode-local-keymap)

  (def-keys-for-map vim:operator-pending-mode-local-keymap
    ("in" vim:motion-inner-haskell-node)
    ("an" vim:motion-outer-haskell-node)
    ("n"  vim:motion-inner-haskell-node))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap
                     vim:motion-mode-local-keymap
                     vim:operator-pending-mode-local-keymap)
    ("m" vim:motion-jump-haskell-item)
    ("'" vim:shm/goto-parent)
    ("q" vim:shm/goto-parent-end))

  (haskell-setup-folding)
  (haskell-abbrev+-setup)
  (setup-eproj-symbnav)
  (setup-outline-headers :header-symbol "-"
                         :length-min 3))

;;; Set up inferior-haskell-mode

(defun inferior-haskell-mode-setup ()
  ;; undo-tree is useless for ghci interaction
  ;; well I'm not sure now, I hope it's useful since it proved itself useful
  ;; for other repls
  ;; (undo-tree-mode -1)
  (init-common :use-comment nil :use-yasnippet nil :use-fci nil)
  (init-repl :create-keymaps nil)

  (vim:local-emap "clear" 'vim:comint-clear-buffer-above-prompt)
  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC"  comint-clear-prompt)
    ("C-w"      backward-delete-word)
    ("C-S-w"    backward-delete-word*)

    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt)
    ("C-h"      comint-next-prompt)
    ("C-t"      comint-previous-prompt)
    ("C-<up>"   compilation-jump-to-prev-error)
    ("C-<down>" compilation-jump-to-next-error))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap
                     inferior-haskell-mode-map)
    ("C-SPC"    vim:comint-clear-buffer-above-prompt)
    ("M-p"      browse-comint-input-history)
    ("<return>" inf-haskell-send-input-or-jump-to-error))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap)
    ("- t" haskell-type)
    ("- i" haskell-info)
    ("- h" haskell-haddock-identifier)
    ("- m" haskell-haddock-module)
    ("- g" haskell-hoogle-at-point)
    ("- y" haskell-hayoo-at-point))

  (def-keys-for-map inferior-haskell-mode-map
    ("C-w"   backward-delete-word)
    ("C-S-w" backward-delete-word*)
    ("<tab>" nil)

    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)
    ("C-<up>"   comint-previous-prompt)
    ("C-<down>" comint-next-prompt)
    ("C-h"      comint-next-prompt)
    ("C-t"      comint-previous-prompt)
    ("S-<up>"   compilation-jump-to-prev-error)
    ("S-<down>" compilation-jump-to-next-error))

  (haskell-abbrev+-setup :repl t))

(defun haskell-interactive-mode-setup ()
  ;; undo-tree is useless for ghci interaction
  ;; well I'm not sure now, I hope it's useful since it proved itself useful
  ;; for other repls
  ;; (undo-tree-mode -1)
  (init-common :use-comment nil
               :use-yasnippet nil
               :use-whitespace nil
               :use-fci nil)
  (init-repl :create-keymaps t
             :bind-return nil
             :bind-vim:motion-current-line nil)
  (structured-haskell-mode -1)
  ;; very useful to automatically surround with spaces inserted operators
  (haskell-bind-shm-bindings :bind-colon nil)

  (vim:local-emap "clear" 'vim:haskell-interactive-clear-buffer-above-prompt)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC"  haskell-interactive-clear-prompt))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap
                     haskell-interactive-mode-map)
    ("C-SPC" vim:comint-clear-buffer-above-prompt)
    ("M-p"   browse-comint-input-history))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     haskell-interactive-mode-map)
    ("C-w"      backward-delete-word)
    ("C-S-w"    backward-delete-word*)

    ("<up>"     haskell-interactive-mode-history-previous)
    ("<down>"   haskell-interactive-mode-history-next)
    ("<tab>"    haskell-interactive-mode-tab)

    ("C-t"      haskell-interactive-jump-to-prev-prompt)
    ("C-h"      haskell-interactive-jump-to-next-prompt)
    ("S-<up>"   haskell-interactive-jump-to-prev-prompt)
    ("S-<down>" haskell-interactive-jump-to-next-prompt))

  (haskell-abbrev+-setup :repl t))

(defun haskell-compilation-setup ()
  (setq-local *compilation-jump-error-regexp*
              +haskell-compile-error-or-warning-regexp+)

  (vim:local-emap "c" 'vim:recompile)
  (def-keys-for-map haskell-compilation-mode-map
    ("<return>" compilation/goto-error)
    ("SPC"      compilation/goto-error-other-window)
    ("g g"      vim-mock:motion-go-to-first-non-blank-beg)
    ("G"        vim-mock:motion-go-to-first-non-blank-end)))

(defun ghc-check-mode-setup ()
  (init-common :use-comment nil :use-yasnippet nil :use-whitespace nil :use-fci nil)
  (def-keys-for-map vim:normal-mode-local-keymap
    ("<up>"     compilation-jump-to-prev-error)
    ("<down>"   compilation-jump-to-next-error)
    ("C-t"      compilation-jump-to-prev-error)
    ("C-h"      compilation-jump-to-next-error)
    ("<return>" compilation/goto-error)
    ("SPC"      compilation/goto-error-other-window)))

(defun haskell-cabal-setup ()
  (init-common :use-comment t :use-yasnippet t)
  (setq-local yas-indent-line 'fixed)
  (haskell-setup-folding)
  (fontify-merge-markers)
  (modify-syntax-entry ?- "_")
  (modify-syntax-entry ?. "_")
  (setq-local vim:shift-width 2)
  (setq-local standard-indent 2)
  (setq-local tab-always-indent t)
  (setq-local indent-line-function
              (lambda ()
                (indent-to standard-indent)))

  (vim:local-emap "compile"  'vim:haskell-compile)
  (vim:local-emap "c"        'vim:haskell-compile)
  (vim:local-emap "ccompile" 'vim:haskell-compile-choosing-command)
  (vim:local-emap "cc"       'vim:haskell-compile-choosing-command)

  (bind-tab-keys #'indent-relative-forward
                 #'indent-relative-backward
                 :enable-yasnippet t)
  (def-keys-for-map vim:normal-mode-local-keymap
    ("'" yafolding-go-parent-element))
  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("<f9>" haskell-compile)))

(defun haskell-lint-setup ()
  (setq-local *compilation-jump-error-regexp*
              haskell-lint-regex))

(defun ghc-core-setup ()
  (structured-haskell-mode -1)
  (hl-line-mode +1))

;; (defvar haskell-lint-error-regex
;;   "^\\(.*?\\) *:\\([0-9]+\\):\\([0-9]+\\): Error:")
;;
;; (defvar haskell-lint-warning-regex
;;   "^\\(.*?\\) *:\\([0-9]+\\):\\([0-9]+\\): Warning:")
;;
;; (defun hs-lint-setup ()
;;   (set (make-local-variable '*compilation-jump-error-regexp*)
;;        (concat "\\("
;;                hs-lint-error-regex
;;                "\\)\\|\\("
;;                hs-lint-warning-regex
;;                "\\)"))
;;   (setq-local compilation-error-regexp-alist
;;               (list
;;                (list hs-lint-error-regex
;;                      1 ;; file-group
;;                      2 ;; line-group
;;                      3 ;; column-group
;;                      2 ;; type - 2 - error
;;                      )
;;                (list hs-lint-warning-regex
;;                      1 ;; file-group
;;                      2 ;; line-group
;;                      3 ;; column-group
;;                      1 ;; type - 1 - warning
;;                      )))
;;   (def-keys-for-map hs-lint-mode-map
;;     ("<up>"   compilation-jump-to-prev-error)
;;     ("<down>" compilation-jump-to-next-error)
;;     ("t"      compilation-jump-to-prev-error)
;;     ("h"      compilation-jump-to-next-error)))

(defun ghc-prof-mode-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-fci nil
               :use-whitespace nil
               :use-render-formula nil
               :use-hl-line t)
  (kill-local-variable 'mode-name)
  (kill-local-variable 'mode-line-format)
  ;; Continue long lines past window's end, don't fol.
  (toggle-truncate-lines +1)
  (haskell-setup-folding))

(provide 'haskell-setup)

;; Local Variables:
;; End:

;; haskell-setup.el ends here
