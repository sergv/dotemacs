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
(require 'dante)
(require 'dante-repl)
(require 'eproj)
(require 'flycheck-setup)
(require 'haskell-abbrev+)
(require 'haskell-misc)
(require 'haskell-outline)
(require 'haskell-tags-server)
(require 'lcr)
(require 'shell-setup)
(require 'smartparens-haskell)

;; never cache module alist to a file
(setf inferior-haskell-module-alist-file nil)

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
  (haskell-start-compilation nil))
(vim:defcmd vim:haskell-compile-choosing-command (nonrepeatable)
  (haskell-start-compilation t))

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

(vim:defcmd vim:haskell-interactive-clear-buffer-above-prompt (nonrepeatable)
  (haskell-interactive-clear-buffer-above-prompt))

(vim:defcmd vim:dante-clear-buffer-above-prompt (nonrepeatable)
  dante-repl-clear-buffer-above-prompt)

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

(defun haskell-go-to-local-symbol-home (&optional use-regexp?)
  (interactive "P")
  (haskell-go-to-symbol-home t use-regexp?))

(defun haskell-go-to-global-symbol-home (&optional use-regexp?)
  (interactive "P")
  (haskell-go-to-symbol-home nil use-regexp?))

(defun haskell-go-to-symbol-home (is-local? use-regexp?)
  (if-let (proj (eproj-get-project-for-buf-lax (current-buffer)))
      (let ((namespace (let* ((all-projects (eproj-get-all-related-projects proj))
                              (shallow-dirs nil)
                              (recursive-dirs (-map #'eproj-project/root all-projects))
                              (ignored-globs (-mapcat #'eproj-project/ignored-files-globs all-projects)))
                         (list shallow-dirs
                               recursive-dirs
                               ignored-globs))))
        (haskell-tags-server-goto-definition is-local? use-regexp? namespace))
    (error "Current buffer has no project")))

(defun haskell-go-to-symbol-home-via-dante-or-eproj (&optional use-regexp?)
  (interactive "P")
  (if (or (not dante-mode) use-regexp?)
      (eproj-symbnav/go-to-symbol-home use-regexp?)
    (let* ((dante-ident-bounds (dante-thing-at-point))
           (dante-identifier
            (when dante-ident-bounds
              (buffer-substring-no-properties (car dante-ident-bounds)
                                              (cdr dante-ident-bounds)))))
      (lcr-cps-let ((_load_messages (dante-async-load-current-buffer nil))
                    (targets (dante-async-call
                              (concat ":loc-at " (dante--ghc-subexp dante-ident-bounds)))))
        (let ((ghci-tags (delq nil
                               (-map #'haskell-misc--ghc-src-span-to-eproj-tag
                                     (s-lines targets)))))
          (if ghci-tags
              (let ((proj (eproj-get-project-for-buf (current-buffer))))
                (eproj-symbnav/choose-symbol-home-to-jump-to
                 dante-identifier
                 (eproj/resolve-synonym-modes major-mode)
                 (expand-file-name buffer-file-name)
                 proj
                 (eproj-symbnav-current-home-entry)
                 (--map (list dante-identifier it proj) ghci-tags)))
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
                      `((column . ,(1- col)))))))

;;;###autoload
(defun haskell-setup ()
  (let ((non-vanilla-haskell-mode? (-any? #'derived-mode-p '(ghc-core-mode haskell-c2hs-mode haskell-hsc-mode))))
    (init-common :use-whitespace 'tabs-only)
    (fontify-conflict-markers!)
    (add-hook 'after-save-hook #'haskell-update-eproj-tags-on-save nil t)

    (pretty-ligatures-install!)
    (pretty-ligatures-install-special-haskell-ligatures!)

    (haskell-watch-register-current-buffer!)
    (setq-local flycheck-enhancements--get-project-root-for-current-buffer
                #'haskell-misc--get-potential-project-roots)

    ;; Read settings from '.eproj-info' file, if any.
    (let (
          ;; NB may be nil.
          (proj (eproj-get-project-for-buf-lax (current-buffer))))

      (haskell-setup-indentation
       :offset (eproj-query/haskell/indent-offset proj))

      (company-mode +1)
      (setq-local company-backends '(company-eproj))
      (setq-local flycheck-highlighting-mode 'symbols)

      (dolist (entry (eproj-query/local-variables proj major-mode nil))
        (set (make-local-variable (car entry)) (cadr entry)))

      (when (and (not non-vanilla-haskell-mode?)
                 (not noninteractive))
        (let* ((flycheck-backend
                (eproj-query/flycheck-checker
                 proj
                 major-mode
                 'haskell-dante)))
          (setq-local flycheck-disabled-checkers
                      (eproj-query/flycheck-disabled-checkers
                       proj
                       major-mode
                       (default-value 'flycheck-disabled-checkers)))
          (if flycheck-backend
              (progn
                (when (eq flycheck-backend 'haskell-dante)
                  (dante-mode +1))
                (unless (flycheck-may-use-checker flycheck-backend)
                  (flycheck-verify-checker flycheck-backend)
                  (error "Unable to select checker '%s' for buffer '%s'"
                         flycheck-backend (current-buffer)))
                (setq-local flycheck-checker flycheck-backend)
                (when (memq flycheck-checker '(haskell-stack-ghc haskell-ghc))
                  (error "Selected flycheck haskell checker will likely not work: %s"
                         flycheck-checker))
                (when (memq flycheck-checker '(haskell-dante))
                  (add-hook 'flycheck-mode-hook #'haskell-misc--configure-dante nil t))
                (flycheck-mode +1))
            ;; Disable flycheck if it was explicitly set to nil
            (progn
              (when flycheck-mode
                (flycheck-mode -1)))))))

    (when dante-mode
      (add-to-list 'company-backends 'dante-company))

    ;; ghci interaction uses comint - same as shell mode
    (turn-on-font-lock)

    ;; fix vim treatment of words for Haskell
    ;; note: do not include underscore into vim:word as this would cause
    ;; inefficiencies while navigating haskell identifiers
    (setq-local vim:word "[:word:]'")
    ;; The underscore should remain part of word so we never search within
    ;; _c_style_identifiers.
    (modify-syntax-entry ?_  "_")
    (modify-syntax-entry ?\' "w")
    (modify-syntax-entry ?\@ "'")

    (setq-local eproj-symbnav/identifier-type 'haskell-symbol)
    (setq-local indent-region-function #'ignore)
    (setq-local yas-indent-line 'fixed)

    (setq-local indent-line-function #'ignore)
    (setq-local abbrev+-fallback-function #'haskell-abbrev+-fallback-space)

    (setq-local compilation-read-command nil)
    (setq-local compilation-auto-jump-to-first-error nil)
    ;; Don't skip any messages.
    (setq-local compilation-skip-threshold 0)

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
     :compile-func #'vim:haskell-compile
     :load-func
     (cond
       (dante-mode
        #'vim:haskell-dante-load-file-into-repl)))

    (vim:local-emap "core" #'vim:ghc-core-create-core)
    (dolist (cmd '("cc" "ccompile"))
      (vim:local-emap cmd #'vim:haskell-compile-choosing-command))

    (cond
      (dante-mode
       (def-keys-for-map vim:normal-mode-local-keymap
         ("SPC SPC"      dante-repl-switch-to-repl-buffer)
         (("C-l" "<f6>") vim:haskell-dante-load-file-into-repl)
         (("- e" "j")    dante-eval-block))

       (def-keys-for-map (vim:normal-mode-local-keymap
                          vim:visual-mode-local-keymap)
         ("- t"          dante-type-at)
         ("- i"          dante-info))))

    (def-keys-for-map vim:normal-mode-local-keymap
      ("\\"           vim:flycheck-run)
      ("g c c"        haskell-comment-node)
      ("+"            input-unicode)
      ("g i"          vim:haskell-navigate-imports)
      ("g I"          haskell-navigate-imports-return)
      ("g <tab>"      haskell-reindent-at-point)

      ("- q"          haskell-qualify-import)
      ("- a"          attrap-flycheck))

    (def-keys-for-map vim:visual-mode-local-keymap
      ("`"            vim:wrap-backticks)
      ("g TAB"        haskell-format-pp-region-with-brittany))

    (def-keys-for-map (vim:normal-mode-local-keymap
                       vim:visual-mode-local-keymap)
      ("*"            search-for-haskell-symbol-at-point-forward)
      ("C-*"          search-for-haskell-symbol-at-point-forward-new-color)
      ("#"            search-for-haskell-symbol-at-point-backward)
      ("C-#"          search-for-haskell-symbol-at-point-backward-new-color)
      ("'"            vim:haskell-backward-up-indentation-or-sexp)
      ("g t"          haskell-move-to-topmost-start)
      ("g h"          haskell-move-to-topmost-end))

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

      ("S-<tab>"         nil)
      ("<S-iso-lefttab>" nil)
      ("<return>"        haskell-newline-with-signature-expansion)
      ("C-<return>"      haskell--simple-indent-newline-indent)
      (("C-m" "<f9>")    haskell-start-compilation))

    (haskell-define-align-bindings! vim:visual-mode-local-keymap)

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
      ("C-?" xref-find-references)

      ;; ("C-."   eproj-symbnav/go-to-symbol-home)
      ;; ("C-,"   eproj-symbnav/go-back)
      ;; ;; ("C-M-." eproj-symbnav/go-to-symbol-home)
      ;; ;; ("C-M-," eproj-symbnav/go-back)
      ;; ("C-M-." haskell-go-to-local-symbol-home)
      ;; ("C-M-," haskell-tags-server-go-back)
      ;; ("M-."   haskell-go-to-global-symbol-home)

      )

    (setup-outline-headers :header-symbol "-"
                           :length-min 3)))

;;;###autoload
(defun haskell-c2hs-setup ()
  (add-to-list 'flycheck-disabled-checkers 'haskell-hlint))

;;;###autoload
(defun haskell-hsc-setup ()
  (add-to-list 'flycheck-disabled-checkers 'haskell-hlint))

;;; Set up inferior-haskell-mode

;;;###autoload
(defun inferior-haskell-mode-setup ()
  ;; undo-tree is useless for ghci interaction
  ;; well I'm not sure now, I hope it's useful since it proved itself useful
  ;; for other repls
  ;; (undo-tree-mode -1)
  (init-common :use-comment nil :use-yasnippet nil :use-fci nil)
  (init-repl :create-keymaps nil)

  (pretty-ligatures-install-safe!)
  (pretty-ligatures-install-special-haskell-ligatures!)

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
  (setq-local indent-region-function #'ignore)
  ;; very useful to automatically surround with spaces inserted operators
  (install-haskell-smart-operators! vim:insert-mode-local-keymap
    :bind-colon nil
    :bind-hyphen nil)

  (pretty-ligatures-install-safe!)
  (pretty-ligatures-install-special-haskell-ligatures!)

  (vim:local-emap "clear" 'vim:haskell-interactive-clear-buffer-above-prompt)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC"  haskell-interactive-clear-prompt))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("-"        haskell--ghci-hyphen)
    (":"        haskell--ghci-colon))

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
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local comment-column 32)
  (setq-local comment-start-skip "--+ *")

  (init-repl :create-keymaps t
             :bind-return nil
             :bind-vim:motion-current-line nil)
  (setq-local indent-region-function #'ignore)
  ;; very useful to automatically surround with spaces inserted operators
  (install-haskell-smart-operators! vim:insert-mode-local-keymap
    :bind-colon nil
    :bind-hyphen nil)

  (pretty-ligatures-install-safe!)
  (pretty-ligatures-install-special-haskell-ligatures!)

  (vim:local-emap "clear" 'vim:haskell-interactive-clear-buffer-above-prompt)
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
  (haskell-abbrev+-setup 2 :repl t))

;;;###autoload
(defun haskell-compilation-setup ()
  (setq-local *compilation-jump-error-regexp*
              +haskell-compile-error-or-warning-navigation-regexp+)

  (pretty-ligatures-install-safe!)
  (pretty-ligatures-install-special-haskell-ligatures!)

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
    ("'"       yafolding-go-parent-element))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    (("C-m" "<f9>") haskell-start-compilation)
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
