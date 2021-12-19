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
(require 'configurable-compilation)
(require 'dante)
(require 'dante-repl)
(require 'eproj)
(require 'flycheck-setup)
(require 'haskell-abbrev+)
(require 'haskell-compilation-commands)
(require 'haskell-misc)
(require 'haskell-outline)
(require 'hydra-setup)
(require 'lcr)
(require 'shell-setup)
(require 'smartparens-haskell)

(defvar ghc-core-program)
(defvar ghc-core-program-args)

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

(vim:defcmd vim:haskell-dante-load-file-into-repl (nonrepeatable)
  (dante-repl-load-file))

(vim:defcmd vim:haskell-dante-repl-restart (nonrepeatable)
  (dante-repl-restart))


(vim:defcmd vim:haskell-dante-restart (nonrepeatable)
  (unless dante-mode
    (error "dante is not enabled"))
  (flycheck-clear t)
  (dante-destroy)
  (lcr-cps-let ((_ (dante-session)))
    (flycheck-buffer)))

(vim:defcmd vim:haskell-dante-configure (nonrepeatable)
  (unless dante-mode
    (error "dante is not enabled"))
  (haskell-misc--configure-dante)
  (vim:haskell-dante-restart))

(vim:defcmd vim:dante-clear-buffer-above-prompt (nonrepeatable)
  (dante-repl-clear-buffer-above-prompt))

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
    (eproj-update-current-buffer-within-its-project!)))

(defun haskell-go-to-symbol-home-via-dante-or-eproj (&optional use-regexp?)
  (interactive "P")
  (if (or (not dante-mode) use-regexp?)
      (eproj-symbnav/go-to-symbol-home use-regexp?)
    (let* ((dante-ident-bounds (dante-thing-at-point))
           (dante-identifier
            (when dante-ident-bounds
              (buffer-substring-no-properties (car dante-ident-bounds)
                                              (cdr dante-ident-bounds)))))
      (lcr-cps-let ((_load_messages (dante-async-load-current-buffer nil nil))
                    (targets (dante-async-call
                              (concat ":loc-at " (dante--ghc-subexp dante-ident-bounds)))))
        (let ((ghci-tags (delq nil
                               (-map #'haskell-misc--ghc-src-span-to-eproj-tag
                                     (s-lines targets)))))
          (if ghci-tags
              (let* ((proj (eproj-get-project-for-buf (current-buffer)))
                     (effective-major-mode
                      (eproj/resolve-synonym-modes major-mode))
                     (lang (aif (gethash effective-major-mode eproj/languages-table)
                               it
                             (error "unsupported language %s" effective-major-mode)))
                     (tag->string (eproj-language/tag->string-func lang))
                     (tag->kind (eproj-language/show-tag-kind-procedure lang)))
                (eproj-symbnav/choose-location-to-jump-to
                 dante-identifier
                 tag->string
                 tag->kind
                 (eproj-symbnav-get-file-name)
                 proj
                 (eproj-symbnav-current-home-entry)
                 (--map (list dante-identifier it proj) ghci-tags)
                 t
                 "Choose symbol\n\n"))
            (eproj-symbnav/go-to-symbol-home use-regexp?)))))))

(defun haskell-misc--ghc-src-span-to-eproj-tag (string)
  "Extract a location from a ghc span STRING."
  ;; On external symbols, GHC may return a location such as integer-gmp-1.0.0.1:integer-gmp-1.0.0.1:GHC.Integer.Type
  (when (string-match "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))$" string)
    (let* ((file (match-string 1 string))
           (resolved-file
            (or (gethash file dante-original-buffer-map)
                (gethash (dante-local-name file) dante-original-buffer-map)
                file))
           (line (string-to-number (match-string 2 string)))
           (col (string-to-number (match-string 3 string))))
      (make-eproj-tag (expand-file-name resolved-file dante-project-root)
                      line
                      nil
                      (vector `(column . ,(1- col)))))))

(defhydra-ext hydra-haskell (:exit t :foreign-keys warn :hint nil)
  "
_a_ttrap          _j_: eval
_i_nfo
_t_ype
_q_ualify import"
  ("t" dante-type-at)
  ("i" dante-info)
  ("j" dante-eval-block)
  ("q" haskell-qualify-import)
  ("a" attrap-flycheck))

(defhydra-derive hydra-cabal-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_TAB_: align and sort subsection"
  ("<tab>" haskell-misc-cabal-align-and-sort-subsection))

(defhydra-derive hydra-haskell-vim-normal-j-ext hydra-vim-normal-j-ext (:exit t :foreign-keys nil :hint nil)
  ""
  ("cc" haskell-comment-node))

(defhydra-derive hydra-haskell-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_i_:     jump to imports  _t_: jump to topmost node start
_I_:     jump back        _h_: jump to topmont node end
_<tab>_: reindent"
  ("i"     vim:haskell-navigate-imports)
  ("I"     haskell-navigate-imports-return)
  ("<tab>" haskell-reindent-at-point)

  ("t"     haskell-move-to-topmost-start)
  ("h"     haskell-move-to-topmost-end))

(defhydra-derive hydra-haskell-vim-visual-g-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign  _t_: jump to topmost node start
       _h_: jump to topmont node end"
  ("a" hydra-haskell-align/body)
  ("t" haskell-move-to-topmost-start)
  ("h" haskell-move-to-topmost-end))

;;;###autoload
(defun haskell-setup ()
  (let ((non-vanilla-haskell-mode? (-any? #'derived-mode-p '(ghc-core-mode haskell-c2hs-mode haskell-hsc-mode))))
    (init-common :use-whitespace 'tabs-only)
    (fontify-conflict-markers!)
    (add-hook 'after-save-hook #'haskell-update-eproj-tags-on-save nil t)

    (pretty-ligatures-install!)
    (pretty-ligatures-install-special-haskell-ligatures!)

    (setq-local flycheck-enhancements--get-project-root-for-current-buffer
                #'haskell-misc-get-project-root)

    ;; Read settings from '.eproj-info' file, if any.
    (let (
          ;; NB may be nil.
          (proj (eproj-get-project-for-buf-lax (current-buffer))))

      (haskell-compilation-commands-install! proj)

      (haskell-setup-indentation
       :offset (eproj-query/haskell/indent-offset proj))

      (company-mode +1)
      (setq-local company-backends '(company-files
                                     (company-eproj company-dabbrev-code)
                                     company-dabbrev)
                  flycheck-highlighting-mode 'symbols)


      (eproj-setup-local-variables proj)

      (when (not non-vanilla-haskell-mode?)
        (flycheck-setup-from-eproj
         proj
         'haskell-dante ;; default checker
         (lambda (backend)
           (when (eq backend 'haskell-dante)
             (setq-local company-backends (cons 'dante-company company-backends))
             (dante-mode +1))
           (unless (flycheck-may-use-checker backend)
             (flycheck-verify-checker backend)
             (error "Unable to select checker '%s' for buffer '%s'"
                    backend (current-buffer)))
           (when (memq backend '(haskell-stack-ghc haskell-ghc))
             (error "Selected flycheck haskell checker will likely not work: %s"
                    backend))
           (when (memq backend '(haskell-dante))
             (add-hook 'flycheck-mode-hook #'haskell-misc--configure-dante nil t))))))

    (turn-on-font-lock)

    ;; The underscore should remain part of word so we never search within
    ;; _c_style_identifiers.
    (modify-syntax-entry ?_  "_")
    (modify-syntax-entry ?\' "w")
    (modify-syntax-entry ?\@ "'")

    (setq-local eproj-symbnav/identifier-type 'haskell-symbol
                indent-region-function #'ignore
                yas-indent-line 'fixed

                ;; Improve vim treatment of words for Haskell.
                ;; Note: underscore should not be included since it would prevent
                ;; navigating inside of some Haskell identifiers, e.g. foo_bar.
                vim:word "[:word:]'"

                indent-line-function #'ignore
                abbrev+-fallback-function #'haskell-abbrev+-fallback-space

                compilation-read-command nil
                compilation-auto-jump-to-first-error nil
                ;; Don't skip any messages.
                compilation-skip-threshold 0)

    ;; Dante doesn't play well with idle-change checks.
    (cond
      (dante-mode
       (setq-local flycheck-check-syntax-automatically '(save mode-enabled))

       (dolist (cmd '("re" "restart"))
         (vim:local-emap cmd #'vim:haskell-dante-restart))
       (dolist (cmd '("conf" "configure"))
         (vim:local-emap cmd #'vim:haskell-dante-configure)))
      ((and flycheck-mode
            (memq flycheck-checker '(haskell-stack-ghc haskell-ghc)))
       (dolist (cmd '("conf" "configure"))
         (vim:local-emap cmd #'vim:haskell-flycheck-configure))))

    (setq-local mode-line-format
                (apply #'default-mode-line-format
                       (append
                        (when dante-mode
                          (list
                           " "
                           '(:eval (dante-status))))
                        (when flycheck-mode
                          (list
                           " "
                           '(:eval (flycheck-pretty-mode-line)))))))

    (flycheck-install-ex-commands!
     :install-flycheck flycheck-mode
     :load-func
     (cond
       (dante-mode
        #'vim:haskell-dante-load-file-into-repl)))

    (vim:local-emap "core" #'vim:ghc-core-create-core)

    (cond
      (dante-mode
       (def-keys-for-map vim:normal-mode-local-keymap
         ("SPC SPC"      dante-repl-switch-to-repl-buffer)
         (("C-l" "<f6>") vim:haskell-dante-load-file-into-repl))))

    (def-keys-for-map vim:normal-mode-local-keymap
      ("\\"           vim:flycheck-run)
      ("g"            hydra-haskell-vim-normal-g-ext/body)
      ("j"            hydra-haskell-vim-normal-j-ext/body)
      ("+"            input-unicode)
      ("-"            hydra-haskell/body))

    (def-keys-for-map vim:visual-mode-local-keymap
      ("`"            vim:wrap-backticks)
      ("g"            hydra-haskell-vim-visual-g-ext/body))

    (def-keys-for-map (vim:normal-mode-local-keymap
                       vim:visual-mode-local-keymap)
      ("*"            search-for-haskell-symbol-at-point-forward)
      ("C-*"          search-for-haskell-symbol-at-point-forward-new-color)
      ("#"            search-for-haskell-symbol-at-point-backward)
      ("C-#"          search-for-haskell-symbol-at-point-backward-new-color)
      ("'"            vim:haskell-backward-up-indentation-or-sexp))

    (def-keys-for-map vim:insert-mode-local-keymap
      ("`"            vim:wrap-backticks)
      (","            haskell-smart-operators-comma))

    (install-haskell-smart-operators!
        vim:insert-mode-local-keymap
      :bind-colon t
      :bind-hyphen t)

    (def-keys-for-map (vim:normal-mode-local-keymap
                       vim:insert-mode-local-keymap)
      ("DEL"             haskell-backspace-with-block-dedent)
      ("<backspace>"     haskell-backspace-with-block-dedent)

      ("C-u"             haskell-insert-undefined)
      ("C-t"             flycheck-enhancements-previous-error-with-wraparound)
      ("C-h"             flycheck-enhancements-next-error-with-wraparound)
      ("M-t"             haskell-compilation-prev-error-other-window)
      ("M-h"             haskell-compilation-next-error-other-window)
      ("C-SPC"           company-complete)

      ("<return>"        haskell-newline-with-signature-expansion)
      ("C-<return>"      haskell--simple-indent-newline-indent)

      (("S-<tab>" "<S-iso-lefttab>" "<backtab>") nil))

    (def-keys-for-map (vim:normal-mode-local-keymap
                       vim:visual-mode-local-keymap
                       vim:motion-mode-local-keymap
                       vim:operator-pending-mode-local-keymap)
      ("'" vim:haskell-backward-up-indentation-or-sexp)
      ("q" vim:haskell-up-sexp))

    (haskell-setup-folding)
    (setup-eproj-symbnav :bind-keybindings nil)
    ;; Override binding introduced by `setup-eproj-symbnav'.
    (def-keys-for-map vim:normal-mode-local-keymap
      ("M-." haskell-go-to-symbol-home-via-dante-or-eproj)
      ("C-." eproj-symbnav/go-to-symbol-home)
      ("C-," eproj-symbnav/go-back)
      ("C-?" xref-find-references))))

;;;###autoload
(defun haskell-c2hs-setup ()
  (add-to-list 'flycheck-disabled-checkers 'haskell-hlint))

;;;###autoload
(defun haskell-hsc-setup ()
  (add-to-list 'flycheck-disabled-checkers 'haskell-hlint))

;;;###autoload
(defun dante-repl-mode-setup ()
  ;; undo-tree is useless for ghci interaction
  ;; well I'm not sure now, I hope it's useful since it proved itself useful
  ;; for other repls
  ;; (undo-tree-mode -1)
  (init-common :use-comment nil
               :use-yasnippet nil
               :use-whitespace nil
               :use-fci nil)

  ;; To make hideshow work
  (setq-local comment-start "--"
              comment-end ""
              comment-column 32
              comment-start-skip "--+ *"
              indent-region-function #'ignore)

  (init-repl :create-keymaps t
             :bind-return nil
             :bind-vim:motion-current-line nil)
  ;; very useful to automatically surround with spaces inserted operators
  (install-haskell-smart-operators! vim:insert-mode-local-keymap
    :bind-colon nil
    :bind-hyphen nil)

  (pretty-ligatures-install-safe!)
  (pretty-ligatures-install-special-haskell-ligatures!)

  (vim:local-emap "clear" 'vim:comint-clear-buffer-above-prompt)
  (dolist (cmd '("re" "restart"))
    (vim:local-emap cmd #'vim:haskell-dante-repl-restart))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC"  comint-clear-prompt))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("-"        haskell--ghci-hyphen)
    (":"        haskell--ghci-colon)
    ("`"        vim:wrap-backticks))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("<tab>"    completion-at-point)
    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)

    ("C-t"      comint-previous-prompt)
    ("C-h"      comint-next-prompt)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt)

    ("C-("      vim:sp-backward-slurp-sexp)
    ("C-)"      vim:sp-forward-slurp-sexp)
    ("M-("      sp-absorb-sexp)
    ("M-)"      sp-emit-sexp)

    ("C-SPC"    vim:comint-clear-buffer-above-prompt)
    ("C-S-p"    browse-comint-input-history))

  (haskell-setup-folding :enable-hs-minor-mode t)
  (haskell-abbrev+-setup t))

;;;###autoload
(defun haskell-compilation-setup ()
  (setq-local *compilation-jump-error-regexp*
              +haskell-compile-error-or-warning-navigation-regexp+)

  (pretty-ligatures-install-safe!)
  (pretty-ligatures-install-special-haskell-ligatures!)

  (vim:local-emap "c" 'vim:recompile)
  (def-keys-for-map haskell-compilation-mode-map
    +vim-special-keys+
    ("<return>" compilation/goto-error)
    ("SPC"      compilation/goto-error-other-window)))

;;;###autoload
(defun haskell-cabal-setup ()
  (init-common :use-comment t :use-yasnippet t)
  (haskell-setup-folding)
  (let ((proj (eproj-get-project-for-buf-lax (current-buffer))))
    (haskell-compilation-commands-install! proj))
  (fontify-merge-markers)
  (modify-syntax-entry ?. "_")
  (setup-indent-size 2)
  (setq-local yas-indent-line 'fixed
              indent-line-function
              (lambda ()
                (indent-to standard-indent)))

  (bind-tab-keys #'tab-to-tab-stop
                 #'tab-to-tab-stop-backward
                 :enable-yasnippet t)
  (def-keys-for-map vim:normal-mode-local-keymap
    ("g" hydra-cabal-vim-normal-g-ext/body)
    ("'" yafolding-go-parent-element))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("<return>"     haskell--simple-indent-newline-same-col)
    ("C-<return>"   haskell--simple-indent-newline-indent)))

;;;###autoload
(defun ghc-core-setup ()
  (pretty-ligatures-install!)
  (pretty-ligatures-install-special-haskell-ligatures!)
  (hl-line-mode +1))

(provide 'haskell-setup)

;; Local Variables:
;; End:

;; haskell-setup.el ends here
