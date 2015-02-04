;; haskell-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


(require 'custom)
(require 'common)
(require 'comment-util)
(require 'custom-predicates)
(require 'browse-kill-ring-setup)

(require 'align)
(require 'compilation-setup)
(require 'eproj)
(require 'shell-setup)

(require 'haskell-checkers)
(require 'shm)

(require 'haskell-abbrev+)
(require 'haskell-misc)
(require 'haskell-outline)
(require 'haskell-shm)

;; never cache module alist to a file
(setf inferior-haskell-module-alist-file nil)

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
(vim:defcmd vim:hs-lint (nonrepeatable)
  (hs-lint))
(vim:defcmd vim:inferior-haskell-load-file (nonrepeatable)
  (inferior-haskell-load-file))
(vim:defcmd vim:haskell-clear-buffer-and-load-file (nonrepeatable)
  (haskell-clear-buffer-and-load-file))


(defun haskell-setup ()
  (init-common :use-yasnippet t
               :use-nxhtml-menu nil
               :use-comment t
               :use-render-formula t)
  (add-hook 'after-save-hook
            (lambda ()
              (ignore-errors
                (eproj-update-buffer-tags)))
            nil
            t)

  ;; ghci interaction uses comint - same as shell mode
  (turn-on-font-lock)
  (font-lock-add-keywords nil
                          '(("`[^`]+`" . font-lock-function-name-face)))

  ;; fix vim treatment of words for Haskell

  ;; note: do not include underscore into vim:word as this would cause
  ;; inefficiencies while navigating haskell identifiers
  (setq-local vim:word "[:word:]'")
  (modify-syntax-entry ?_ "_")
  (modify-syntax-entry ?\' "w")
  (modify-syntax-entry ?\@ "'")

  ;; (setq-local vim:word "[:word:]_'")

  ;; (modify-syntax-entry ?\` "\"")

  (setq-local eproj-symbnav/identifier-type 'haskell-symbol)
  (setq-local indent-region-function #'ignore)
  (setq-local yas-indent-line 'fixed)

  (structured-haskell-mode +1)
  (setq-local indent-line-function #'ignore)
  (setq-local abbrev+-fallback-function #'haskell-abbrev+-fallback-space)

  ;; (turn-on-haskell-simple-indent)

  (turn-on-haskell-doc-mode)
  (setf haskell-doc-show-global-types t)

  ;; it's not always a good idea to wait
  ;; (setf inferior-haskell-wait-and-jump t)

  (setq-local yas-prompt-functions
              (list #'haskell-yas-completing-prompt))

  (let ((offset 2))
    (setq-local vim:shift-width       offset)
    (setq-local haskell-indent-offset offset)
    (setq-local haskell-indent-spaces offset)
    (setq-local shm-indent-spaces     offset))

  (setq-local compilation-read-command nil)
  ;; don't ask - just save
  (setq-local compilation-ask-about-save nil)
  (setq-local compilation-auto-jump-to-first-error nil)
  ;; don't skip any messages
  (setq-local compilation-skip-threshold 0)

  ;; (ghc-init)

  (vim:local-emap "core" 'vim:ghc-core-create-core)
  (vim:local-emap "compile" 'vim:haskell-compile)
  (vim:local-emap "c" 'vim:haskell-compile)
  (vim:local-emap "ccompile" 'vim:haskell-compile-choosing-command)
  (vim:local-emap "cc" 'vim:haskell-compile-choosing-command)
  (vim:local-emap "hlint" 'vim:hs-lint)
  (vim:local-emap "load" 'vim:inferior-haskell-load-file)
  (vim:local-emap "loadc" 'vim:haskell-clear-buffer-and-load-file)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("j"         inferior-haskell-send-decl)
    ("g c d"     comment-util-delete-commented-part)
    ("g c c"     haskell-comment-node)
    ("g c o r e" ghc-core-create-core)
    ("+"         input-unicode)
    ("SPC SPC"   switch-to-haskell)
    ("g w"       shm/goto-where)
    ("`"         haskell-compile)
    ("C-`"       hs-lint))

  (haskell-bind-shm-bindings)

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
    ("<tab>"           shm/tab)
    ("<backtab>"       shm/backtab)
    ("C-t"             haskell-compilation-prev-error-other-window)
    ("C-h"             haskell-compilation-next-error-other-window)

    ("S-<tab>"         nil)
    ("<S-iso-lefttab>" nil)
    ("<return>"        haskell-newline)
    ("<f6>"            inferior-haskell-load-file)
    ("<f9>"            haskell-compile)
    ("S-<f9>"          hs-lint)
    ("C-<f6>"          haskell-clear-buffer-and-load-file))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap)
    ;; (", t"     haskell-type)
    ;; (", i"     haskell-info)
    ;; (", h"     haskell-haddock-identifier)
    ;; (", m"     haskell-haddock-module)
    ;; (", g"     haskell-hoogle-at-point)
    (", y"     hayoo)

    ("*"       search-for-haskell-symbol-at-point-forward)
    ("C-*"     search-for-haskell-symbol-at-point-forward-new-color)
    ("#"       search-for-haskell-symbol-at-point-backward)
    ("C-#"     search-for-haskell-symbol-at-point-backward-new-color)
    ("'"       vim:shm/goto-parent)
    ;; ("'"       haskell-move-up)
    ("g n"     haskell-node/move-to-topmost-start)
    ("g t"     haskell-node/move-to-topmost-end))

  ;; (def-keys-for-map (vim:normal-mode-local-keymap
  ;;                    vim:insert-mode-local-keymap)
  ;;   ("<tab>"           haskell-simple-indent)
  ;;   ("S-<tab>"         haskell-simple-indent-backtab)
  ;;   ("<S-iso-lefttab>" haskell-simple-indent-backtab))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("g a"       nil)
    ("g a a"     haskell-align-generic)
    ("g a ="     haskell-align-on-equals)
    ("g a - >"   haskell-align-on-arrows)
    ("g a < -"   haskell-align-on-left-arrows)
    ("g a |"     haskell-align-on-guards)
    ("g a ,"     haskell-align-on-commas)
    ("g a - -"   haskell-align-on-comments)
    ("g a : :"   haskell-align-on-double-colons)
    ("g a # - }" haskell-align-on-pragma-close))

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
    ("C-<return>"   shm/simple-indent-newline-same-col))

  (haskell-setup-folding)
  (haskell-abbrev+-setup)
  (setup-eproj-symbnav)
  (setup-outline-headers :header-symbol "-"
                         :length-min 3))

;;; set up inferior-haskell-mode

(defun inferior-haskell-mode-setup ()
  ;; undo-tree is useless for ghci interaction
  ;; well I'm not sure now, I hope it's useful since it proved itself useful
  ;; for other repls
  ;; (undo-tree-mode -1)
  (init-common :use-comment nil :use-yasnippet nil)
  (init-repl :create-keymaps nil)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC"  comint-clear-prompt)
    ("C-SPC"    comint-clear-buffer-above-prompt)
    ("C-w"      backward-delete-word)
    ("C-S-w"    backward-delete-word*)

    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt)
    ("C-<up>"   compilation-jump-to-prev-error)
    ("C-<down>" compilation-jump-to-next-error))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap
                     inferior-haskell-mode-map)
    ("M-p"      browse-kill-ring)
    ("C-M-p"    browse-comint-input-history)
    ("<return>" inf-haskell-send-input-or-jump-to-error))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap)
    (", t"     haskell-type)
    (", i"     haskell-info)
    (", h"     haskell-haddock-identifier)
    (", m"     haskell-haddock-module)
    (", g"     haskell-hoogle-at-point)
    (", y"     haskell-hayoo-at-point))

  (def-keys-for-map inferior-haskell-mode-map
    ("C-w"      backward-delete-word)
    ("C-S-w"    backward-delete-word*)
    ("<tab>"    nil)
    ("C-SPC"    comint-clear-buffer-above-prompt)

    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)
    ("C-<up>"   comint-previous-prompt)
    ("C-<down>" comint-next-prompt)
    ("S-<up>"   compilation-jump-to-prev-error)
    ("S-<down>" compilation-jump-to-next-error))

  (haskell-abbrev+-setup :repl t))

(add-hook 'inferior-haskell-mode-hook #'inferior-haskell-mode-setup)

(defun haskell-interactive-mode-setup ()
  ;; undo-tree is useless for ghci interaction
  ;; well I'm not sure now, I hope it's useful since it proved itself useful
  ;; for other repls
  ;; (undo-tree-mode -1)
  (init-common :use-comment nil :use-yasnippet nil :use-whitespace nil)
  (init-repl :create-keymaps t :bind-return nil :bind-vim:motion-current-line nil)
  (haskell-bind-shm-bindings)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC"  haskell-interactive-clear-prompt))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap
                     haskell-interactive-mode-map)
    ("M-p"      browse-kill-ring)
    ("C-M-p"    browse-haskell-interactive-input-history)
    ;; ("<return>" inf-haskell-send-input-or-jump-to-error)
    )

  (def-keys-for-map (vim:normal-mode-local-keymap
                     haskell-interactive-mode-map)
    ("C-SPC"    haskell-interactive-clear-buffer-above-prompt)
    ("C-w"      backward-delete-word)
    ("C-S-w"    backward-delete-word*)

    ("<up>"     haskell-interactive-mode-history-previous)
    ("<down>"   haskell-interactive-mode-history-next)
    ("<tab>"    haskell-interactive-mode-tab)

    ("S-<up>"   haskell-interactive-jump-to-prev-prompt)
    ("S-<down>" haskell-interactive-jump-to-next-prompt))

  (haskell-abbrev+-setup :repl t))

(add-hook 'haskell-interactive-mode-hook #'haskell-interactive-mode-setup)

(defun haskell-compilation-setup ()
  (setq-local *compilation-jump-error-regexp*
              +haskell-compile-error-or-warning-regexp+)
  (def-keys-for-map haskell-compilation-mode-map
    ("`"        recompile)
    ("<return>" compilation/goto-error)
    ("SPC"      compilation/goto-error-other-window)
    ("g g"      vim-mock:motion-go-to-first-non-blank-beg)
    ("G"        vim-mock:motion-go-to-first-non-blank-end)))

(add-hook 'haskell-compilation-mode-hook #'haskell-compilation-setup)

(defun haskell-cabal-setup ()
  (init-common :use-comment t :use-yasnippet nil)
  (setq-local vim:shift-width 2)
  (setq-local standard-indent 2)
  (setq-local tab-always-indent t)
  (setq-local indent-line-function
              (lambda ()
                (indent-to standard-indent)))
  (def-keys-for-map '(vim:normal-mode-local-keymap
                      vim:insert-mode-local-keymap)
    ("<tab>"           indent-relative-forward)
    ("S-<tab>"         indent-relative-backward)
    ("S-<iso-lefttab>" indent-relative-backward)
    ("`"               haskell-compile)
    ("<f9>"            haskell-compile)))

(add-hook 'haskell-cabal-mode-hook #'haskell-cabal-setup)

(defun hs-lint-setup ()
  (setq-local *compilation-jump-error-regexp*
              hs-lint-regex)
  (let ((hs-lint-regex-orig
         "^\\(.*?\\) *:\\([0-9]+\\):\\([0-9]+\\): %s:.*[\n\C-m]Found:[\n\C-m]\\s +.*[\n\C-m]Why not:[\n\C-m]\\s +.*[\n\C-m]"))
    (setq-local compilation-error-regex-alist
                (list
                 (list (format hs-lint-regex-orig "Error")
                       1 2 3 2)
                 (list (format hs-lint-regex-orig "Warning")
                       1 2 3 1)))))

(add-hook 'hs-lint-setup-hook #'hs-lint-setup)

(defun ghc-core-setup ()
  (structured-haskell-mode -1))

(add-hook 'ghc-core-mode-hook #'ghc-core-setup)

(provide 'haskell-setup)

;; Local Variables:
;; End:

;; haskell-setup.el ends here
