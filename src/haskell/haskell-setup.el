;; haskell-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile (require 'subr-x))

(require 'align)
(require 'browse-kill-ring-setup)
(require 'comment-util)
(require 'common)
(require 'company-eproj)
(require 'company-mode-setup)
(require 'compilation-setup)
(require 'eproj)
(require 'flycheck-setup)
(require 'haskell-abbrev+)
(require 'haskell-misc)
(require 'haskell-outline)
(require 'haskell-shm)
(require 'intero)
(require 'shell-setup)
(require 'shm)
(require 'vim-intero-highlight-uses-mode)

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

(vimmize-motion haskell-backward-up-indentation-or-sexp
                :name vim:haskell-backward-up-indentation-or-sexp
                :exclusive t
                :do-not-adjust-point t)

(vimmize-motion haskell-up-sexp
                :name vim:haskell-up-sexp
                :exclusive t
                :do-not-adjust-point t)

(autoload 'flycheck--locate-dominating-file-matching "flycheck")

(vim:defcmd vim:ghc-core-create-core (nonrepeatable)
  (let* ((is-stack-project?
          (flycheck--locate-dominating-file-matching
           default-directory
           "stack.*\\.yaml\\'"))
         (ghc-core-program
          (if is-stack-project?
              "stack"
            ghc-core-program))
         (ghc-core-program-args
          (if is-stack-project?
              (cons "ghc"
                    (cons "--"
                          ghc-core-program-args))
            ghc-core-program-args)))
    (ghc-core-create-core current-prefix-arg)))

(vim:defcmd vim:haskell-compile (nonrepeatable)
  (haskell-compile nil))
(vim:defcmd vim:haskell-compile-choosing-command (nonrepeatable)
  (haskell-compile t))

(vim:defcmd vim:haskell-intero-load-file-into-repl (nonrepeatable)
  (intero-repl-load))
(vim:defcmd vim:haskell-intero-restart (nonrepeatable)
  (intero-restart))
(vim:defcmd vim:haskell-intero-restart-repl (nonrepeatable)
  (intero-restart-repl))

(vim:defcmd vim:haskell-load-file-into-repl (nonrepeatable)
  (haskell-process-load-file))

;; (vim:defcmd vim:haskell-set-target (nonrepeatable)
;;   (call-interactively #'haskell-session-change-target))
(vim:defcmd vim:haskell-interactive-clear-buffer-above-prompt (nonrepeatable)
  (haskell-interactive-clear-buffer-above-prompt))

(vim:defcmd vim:haskell-flycheck-configure (nonrepeatable)
  (flycheck-haskell-clear-config-cache)
  (setf flycheck-ghc-package-databases nil
        flycheck-haskell-ghc-executable nil
        flycheck-ghc-search-path nil
        flycheck-ghc-language-extensions nil
        flycheck-ghc-args nil
        flycheck-hlint-args nil)
  (flycheck-haskell-configure)
  (vim:flycheck-run))

(vim:defcmd vim:haskell-navigate-imports (nonrepeatable)
  (haskell-navigate-imports)
  (vim:save-position haskell-navigate-imports-start-point))

(defun haskell-update-eproj-tags-on-save ()
  (ignore-errors
    (eproj-update-buffer-tags)))

(defun haskell-go-to-symbol-home (&optional use-regexp?)
  "Try to get symbol location via intero (`intero-goto-definition'), if it's
enabled. Otherwise fall back to eproj tags."
  (interactive "P")
  (or (when (and intero-mode
                 (not use-regexp?))
        (intero-goto-definition))
      (eproj-symbnav/go-to-symbol-home use-regexp?)))

;;;###autoload
(defun haskell-setup ()
  (let ((intero-enabled?
         (if (and (not (derived-mode-p 'ghc-core-mode))
                  (if (executable-find "intero")
                      t
                    (message "[WARNING] Could not enable Intero because 'intero' executable was not found")))
             t
           nil))
        (flycheck-enabled? (not (derived-mode-p 'ghc-core-mode)))
        (liquid-haskell-enabled? nil))
    (init-common :use-yasnippet t
                 :use-comment t
                 :use-render-formula nil
                 :use-hl-line nil
                 :use-whitespace 'tabs-only)
    (fontify-conflict-markers!)
    (with-demoted-errors "haskell-watch failed: %s"
      (haskell-watch-register-current-buffer!))
    (add-hook 'after-save-hook #'haskell-update-eproj-tags-on-save nil t)

    ;; Read settings from '.eproj-info' file, if any.
    (let ((proj (with-demoted-errors "no eproj project found: %s"
                  (eproj-get-project-for-buf (current-buffer)))))

      (haskell-setup-indentation
       (eproj-query/haskell/indent-offset proj))

      (unless (derived-mode-p 'ghc-core-mode)
        (setf intero-enabled? (eproj-query/haskell/enable-intero? proj intero-enabled?))
        (if intero-enabled?
            (setf intero-enabled? (intero-mode-maybe))
          (when intero-mode
            (intero-mode -1)))

        (company-mode +1)
        (setq-local company-backends '(company-eproj))

        (setf flycheck-enabled?
              (eproj-query/general/enable-flycheck? proj flycheck-enabled?))
        (if flycheck-enabled?
            (let ((checker (if intero-enabled? 'intero 'haskell-stack-ghc)))
              (unless (flycheck-may-use-checker checker)
                (flycheck-verify-checker checker)
                (error "Unable to select checker '%s' for buffer '%s'"
                       checker (current-buffer)))
              (setq-local flycheck-checker checker)
              (flycheck-mode +1))
          (when flycheck-mode
            (flycheck-mode -1)))))

    ;; ghci interaction uses comint - same as shell mode
    (turn-on-font-lock)

    ;; fix vim treatment of words for Haskell
    ;; note: do not include underscore into vim:word as this would cause
    ;; inefficiencies while navigating haskell identifiers
    (setq-local vim:word "[:word:]'")
    ;; The underscore should remain part of word so we never search within
    ;; _c_style_identifiers.
    (modify-syntax-entry ?_ "_")
    (modify-syntax-entry ?\' "w")
    (modify-syntax-entry ?\@ "'")

    (setq-local eproj-symbnav/identifier-type 'haskell-symbol)
    (setq-local indent-region-function #'ignore)
    (setq-local yas-indent-line 'fixed)

    (structured-haskell-mode +1)
    (setq-local indent-line-function #'ignore)
    (setq-local abbrev+-fallback-function #'haskell-abbrev+-fallback-space)

    ;; (turn-on-haskell-simple-indent)

    (setq-local compilation-read-command nil)
    (setq-local compilation-auto-jump-to-first-error nil)
    ;; Don't skip any messages.
    (setq-local compilation-skip-threshold 0)

    (flycheck-install-ex-commands!
     :install-flycheck flycheck-enabled?
     :compile-func #'vim:haskell-compile
     :load-func
     (if intero-enabled?
         #'vim:haskell-intero-load-file-into-repl
       #'vim:haskell-load-file-into-repl))

    (when intero-enabled?
      (dolist (cmd '("re" "restart"))
        (vim:local-emap cmd #'vim:haskell-intero-restart)))

    (vim:local-emap "core" #'vim:ghc-core-create-core)
    (dolist (cmd '("cc" "ccompile"))
      (vim:local-emap cmd #'vim:haskell-compile-choosing-command))
    (when (or flycheck-enabled?
              intero-enabled?)
      (dolist (cmd '("conf" "configure"))
        (vim:local-emap cmd #'vim:haskell-flycheck-configure)))

    (def-keys-for-map vim:normal-mode-local-keymap
      ("\\"      vim:flycheck-run)
      ("j"       vim:haskell-load-file-into-repl)
      ("g c c"   haskell-comment-node)
      ("+"       input-unicode)
      ("SPC SPC" haskell-misc-switch-to-haskell)
      ("g w"     shm/goto-where)
      ("g i"     vim:haskell-navigate-imports)
      ("g I"     haskell-navigate-imports-return)
      ("g <tab>" haskell-reindent-at-point)

      ;; ("- t" ghc-show-type)
      ;; ("- i" ghc-show-info)
      ;; ("- e" ghc-expand-th)
      ;; ("- m" ghc-insert-module)
      ;; ("- c" ghc-case-split)
      ;; ("- r" ghc-refine)
      ;; ("- a" ghc-auto)
      ;; ("- s" ghc-insert-template-or-signature)

      ("- q"     shm/qualify-import)
      ("- e"     shm/export)

      ("- t"     intero-type-at)
      ("- u"     intero-uses-at)
      ("- i"     intero-info)
      ("- ."     intero-goto-definition)
      ("- a"     intero-apply-suggestions)
      ("- s"     intero-expand-splice-at-point))

    (def-keys-for-map vim:visual-mode-local-keymap
      ("`"       vim:wrap-backticks)
      ("g TAB"   haskell-format-pp-region-with-brittany)

      ("- e"     intero-repl-eval-region))

    (def-keys-for-map vim:insert-mode-local-keymap
      ;; Just let `smartparens-mode' take care of these.
      ;; ("\""  shm/double-quote)
      ;; ("("   shm/open-paren)
      ;; (")"   shm/close-paren)
      ;; ("["   shm/open-bracket)
      ;; ("]"   shm/close-bracket)
      ;; ("{"   shm/open-brace)
      ;; ("}"   shm/close-brace)
      ("`"   vim:wrap-backticks)
      (","   shm/comma))

    (install-haskell-smart-operators!
        vim:insert-mode-local-keymap
      :bind-colon t
      :bind-hyphen t
      :use-shm t)
    (def-keys-for-map shm-map
      ("("            nil)
      ("["            nil)
      ("{"            nil)
      (")"            nil)
      ("]"            nil)
      ("}"            nil)
      ("\""           nil)
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

    (def-keys-for-map (vim:normal-mode-local-keymap
                       vim:insert-mode-local-keymap)
      ("C-w"             shm/backward-kill-word)
      ("C-u"             shm/insert-undefined)
      ("C-<up>"          shm/swing-up)
      ("C-<down>"        shm/swing-down)
      ("C-t"             flycheck-enhancements-previous-error-with-wraparound)
      ("C-h"             flycheck-enhancements-next-error-with-wraparound)
      ("M-t"             haskell-compilation-prev-error-other-window)
      ("M-h"             haskell-compilation-next-error-other-window)
      ("C-SPC"           company-complete)

      ("C-l"             intero-repl-load)

      ("S-<tab>"         nil)
      ("<S-iso-lefttab>" nil)
      ("<return>"        haskell-newline)
      ("<f6>"            haskell-process-load-file)
      ("<f9>"            haskell-compile))

    (def-keys-for-map (vim:normal-mode-local-keymap
                       vim:visual-mode-local-keymap)
      ("*"   search-for-haskell-symbol-at-point-forward)
      ("C-*" search-for-haskell-symbol-at-point-forward-new-color)
      ("#"   search-for-haskell-symbol-at-point-backward)
      ("C-#" search-for-haskell-symbol-at-point-backward-new-color)
      ("'"   vim:haskell-backward-up-indentation-or-sexp)
      ("g t" haskell-node/move-to-topmost-start)
      ("g h" haskell-node/move-to-topmost-end))

    (haskell-define-align-bindings! vim:visual-mode-local-keymap)

    (def-keys-for-map vim:operator-pending-mode-local-keymap
      ("in" vim:motion-inner-haskell-node)
      ("an" vim:motion-outer-haskell-node)
      ("n"  vim:motion-inner-haskell-node))

    (def-keys-for-map (vim:normal-mode-local-keymap
                       vim:visual-mode-local-keymap
                       vim:motion-mode-local-keymap
                       vim:operator-pending-mode-local-keymap)
      ("m" vim:motion-jump-haskell-item)
      ("'" vim:haskell-backward-up-indentation-or-sexp)
      ("q" vim:haskell-up-sexp))

    (haskell-setup-folding)
    (setup-eproj-symbnav)
    ;; Override binding introduced by `setup-eproj-symbnav'.
    (def-keys-for-map vim:normal-mode-local-keymap
      ("C-." haskell-go-to-symbol-home))

    (setup-outline-headers :header-symbol "-"
                           :length-min 3)))

;;; Set up inferior-haskell-mode

;;;###autoload
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
    ("C-S-p"    browse-comint-input-history)
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

  (haskell-abbrev+-setup 2 :repl t))

;;;###autoload
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
  (setq-local indent-region-function #'ignore)
  ;; very useful to automatically surround with spaces inserted operators
  (install-haskell-smart-operators! vim:insert-mode-local-keymap
    :bind-colon nil
    :bind-hyphen nil
    :use-shm nil)

  (vim:local-emap "clear" 'vim:haskell-interactive-clear-buffer-above-prompt)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC"  haskell-interactive-clear-prompt))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("-"        haskell--ghci-shm/hyphen))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap
                     haskell-interactive-mode-map)
    ("C-SPC"    vim:comint-clear-buffer-above-prompt)
    ("C-S-p"    browse-haskell-interactive-input-history))

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

  (haskell-abbrev+-setup 2 :repl t))

;;;###autoload
(defun intero-repl-mode-setup ()
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
  (setq-local indent-region-function #'ignore)
  ;; very useful to automatically surround with spaces inserted operators
  (install-haskell-smart-operators! vim:insert-mode-local-keymap
    :bind-colon nil
    :bind-hyphen nil
    :use-shm nil)

  (vim:local-emap "clear" 'vim:haskell-interactive-clear-buffer-above-prompt)
  (dolist (cmd '("re" "restart"))
    (vim:local-emap cmd 'vim:haskell-intero-restart-repl))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC"  comint-clear-prompt)

    ("C-t"      comint-previous-prompt)
    ("C-h"      comint-next-prompt)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("-"        haskell--ghci-shm/hyphen)
    ("`"        vim:wrap-backticks))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("<tab>"    completion-at-point)
    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)

    ("C-("      vim:sp-backward-slurp-sexp)
    ("C-)"      vim:sp-forward-slurp-sexp)
    ("M-("      sp-absorb-sexp)
    ("M-)"      sp-emit-sexp)

    ("C-SPC"    vim:comint-clear-buffer-above-prompt)
    ("C-S-p"    browse-comint-input-history)

    (("C-l" "<f5>") intero-repl-reload))

  (haskell-abbrev+-setup 2 :repl t))

;;;###autoload
(defun haskell-compilation-setup ()
  (setq-local *compilation-jump-error-regexp*
              +haskell-compile-error-or-warning-navigation-regexp+)

  (vim:local-emap "c" 'vim:recompile)
  (def-keys-for-map haskell-compilation-mode-map
    +vim-interbuffer-navigation-keys+
    +vim-special-keys+
    ("<return>" compilation/goto-error)
    ("SPC"      compilation/goto-error-other-window)
    ("g g"      vim-mock:motion-go-to-first-non-blank-beg)
    ("G"        vim-mock:motion-go-to-first-non-blank-end)))

;;;###autoload
(defun haskell-cabal-setup ()
  (init-common :use-comment t :use-yasnippet t)
  (setq-local yas-indent-line 'fixed)
  (haskell-setup-folding)
  (fontify-merge-markers)
  (modify-syntax-entry ?. "_")
  (setup-indent-size 2)
  (setq-local indent-line-function
              (lambda ()
                (indent-to standard-indent)))

  (vim:local-emap "compile"  'vim:haskell-compile)
  (vim:local-emap "c"        'vim:haskell-compile)
  (vim:local-emap "ccompile" 'vim:haskell-compile-choosing-command)
  (vim:local-emap "cc"       'vim:haskell-compile-choosing-command)

  (bind-tab-keys #'tab-to-tab-stop
                 #'tab-to-tab-stop-backward
                 :enable-yasnippet t)
  (def-keys-for-map vim:normal-mode-local-keymap
    ("g <tab>" haskell-misc-cabal-align-and-sort-subsection)
    ("'"       yafolding-go-parent-element)
    ("SPC SPC" haskell-misc-switch-to-haskell))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("<f9>" haskell-compile)))

;;;###autoload
(defun ghc-core-setup ()
  (structured-haskell-mode -1)
  (hl-line-mode +1))

(provide 'haskell-setup)

;; Local Variables:
;; End:

;; haskell-setup.el ends here
