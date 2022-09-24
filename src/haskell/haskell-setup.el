;; haskell-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'subr-x)
  (require 'macro-util)


  (defvar ghc-core-program)
  (defvar ghc-core-program-args))

(declare-function lsp-ui-sideline-mode "lsp-ui-sideline")

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
(require 'eproj-symbnav)
(require 'flycheck-setup)
(require 'haskell-abbrev+)
(require 'haskell-compilation-commands)
(require 'haskell-misc)
(require 'haskell-outline)
(require 'hydra-setup)
(require 'lcr)
(require 'lsp-haskell-setup)
(require 's-extras)
(require 'shell-setup)

(vimmize-motion haskell-backward-up-indentation-or-sexp
                :name vim:haskell-backward-up-indentation-or-sexp
                :exclusive t
                :unadjusted t
                :raw-result t)

(vimmize-motion haskell-up-sexp
                :name vim:haskell-up-sexp
                :exclusive t
                :unadjusted t
                :raw-result t)

(autoload 'flycheck--locate-dominating-file-matching "flycheck")

(vim-defcmd vim:ghc-core-create-core (nonrepeatable)
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

(vim-defcmd vim:dante-repl-switch-to-repl-buffer (nonrepeatable)
  (haskell-misc--configure-dante-if-needed!)
  (dante-repl-switch-to-repl-buffer))

(vim-defcmd vim:haskell-dante-load-file-into-repl (nonrepeatable)
  (haskell-misc--configure-dante-if-needed!)
  (dante-repl-load-file))

(vim-defcmd vim:haskell-dante-repl-restart (nonrepeatable)
  "Restart dante repl."
  (dante-repl-restart nil))

(vim-defcmd vim:haskell-lsp-flycheck-reset (nonrepeatable)
  "Restart lsp checker session."
  (vim:flycheck-clear)
  (when (and (boundp 'flycheck-checker)
             (eq flycheck-checker 'lsp))
    (lsp-workspace-restart (lsp--read-workspace))))

(vim-defcmd vim:haskell-dante-reset (nonrepeatable)
  "Destroy dante checker session and attempt to create a new one."
  ;; Don’t use ‘dante-restart’ because it won’t have any effect if there’s no
  ;; dante buffer.
  (unless dante-mode
    (error "dante is not enabled"))
  (vim:flycheck-clear)
  (dante-destroy)
  (lcr-cps-let ((_ (dante-session)))
    (flycheck-buffer)))

(vim-defcmd vim:haskell-dante-configure (nonrepeatable)
  (haskell-misc--configure-dante!))

(vim-defcmd vim:dante-clear-buffer-above-prompt (nonrepeatable)
  "Clear text above ghci prompt."
  (dante-repl-clear-buffer-above-prompt))

(vim-defcmd vim:haskell-navigate-imports (nonrepeatable)
  "Jump to the imports section."
  (vim-save-position)
  (haskell-navigate-imports))

(vim-defcmd vim:haskell-comment-line (count repeatable)
  (haskell-comment-line count))

(defun haskell-update-eproj-tags-on-save ()
  (ignore-errors
    (eproj-update-current-buffer-within-its-project!)))

(vim-defcmd vim:haskell-qualify-import (repeatable)
  (haskell-qualify-import))

;; Sample ‘packages’ content:
;; "active package flags:\n  -package-id base-4.15.1.0\n  -package-id aeson-2.0.3.0-e91573e5a9f0a74731f7cb1fe08486dfa1990213df0c4f864e51b791370cc73d"
(defun haskell-go-to-symbol-home--strip-ghci-packages-of-versions (packages)
  (let* ((lines (s-lines packages))
         (lines2 (if (string= (car lines) "active package flags:")
                     (cdr lines)
                   lines)))
    (--map (replace-regexp-in-string
            haskell-regexen/package-version
            ""
            (strip-string-prefix "  -package-id " it))
           lines2)))

(defun haskell-go-to-symbol-home--jump-to-filtered-tags (identifier mod-name pkgs-without-versions)
  (haskell-symbnav--jump-to-filtered-tags
   identifier
   (concat "/"
           "\\(:?" (regexp-opt pkgs-without-versions) "\\)"
           ".*"
           "/"
           (s-extras-replace-char! ?. ?/ mod-name)
           "."
           (eval-when-compile (regexp-opt +haskell-extensions+)))))

(defun haskell-dante-symbnav/go-to-symbol-home ()
  (let* ((dante-ident-bounds (dante-thing-at-point))
         (identifier
          (when dante-ident-bounds
            (buffer-substring-no-properties (car dante-ident-bounds)
                                            (cdr dante-ident-bounds)))))
    (lcr-cps-let ((_load_messages (dante-async-load-current-buffer nil nil))
                  (locations (dante-async-call
                              (concat ":loc-at " (dante--ghc-subexp dante-ident-bounds)))))
      (if-let (ghci-tags
               (save-match-data
                 (delq nil
                       (-map #'haskell-go-to-symbol-home--ghc-src-span-to-eproj-tag
                             (s-lines locations)))))
          (let* ((proj (eproj-get-project-for-buf (current-buffer)))
                 (effective-major-mode (eproj/resolve-synonym-modes major-mode))
                 (lang (aif (gethash effective-major-mode eproj/languages-table)
                           it
                         (error "unsupported language %s" effective-major-mode)))
                 (tag->string (eproj-language/tag->string-func lang))
                 (tag->kind (eproj-language/show-tag-kind-procedure lang)))
            (eproj-symbnav/choose-location-to-jump-to
             identifier
             tag->string
             tag->kind
             (eproj-symbnav-get-file-name)
             proj
             (eproj-symbnav-current-home-entry)
             (--map (list identifier it proj) ghci-tags)
             t
             "Choose symbol\n\n"))
        (lcr-cps-let ((_load-message (dante-async-load-current-buffer t nil))
                      (info (dante-async-call (concat ":i " identifier))))
          ;; Parse ghci responses, they may narrow down the result.
          (save-match-data
            (cond
              ;; Sometimes :loc-at couldn’t produce anything useful but :i pinpoints
              ;; the result perfectly.
              ((string-match haskell-regexen/ghci-info-definition-site-in-curr-project-for-old-ghci info)
               (let ((file (match-string 1 info))
                     (line (string->number (match-string 2 info)))
                     (column (string->number (match-string 3 info))))
                 (unless (file-name-absolute-p file)
                   (setq file (expand-file-name file (dante-project-root))))
                 (eproj-symbnav--jump-to-location file line column (eproj-symbnav-current-home-entry) identifier)))
              ;; Now try to check whether :loc-at produce module name we could use. The same module
              ;; name is available in the output of :i command but :loc-at also includes
              ;; specific package name we could use whereas :i doesn’t and we’ll have to
              ;; use set of currently loaded packages as an approximation.
              ((string-match haskell-regexen/ghci-loc-at-external-symbol locations)
               (let* ((mod-name (match-string-no-properties 3 locations))
                      (pkg1 (match-string-no-properties 1 locations))
                      (pkg2 (match-string-no-properties 2 locations))
                      (pkgs-without-versions
                       (if (equal pkg1 pkg2)
                           (list pkg1)
                         (list pkg1 pkg2))))
                 (haskell-go-to-symbol-home--jump-to-filtered-tags identifier
                                                                   mod-name
                                                                   pkgs-without-versions)))
              ;; Other times :i only provides us with a module name which is still
              ;; usefull to narrow down tag search.
              ((string-match haskell-regexen/ghci-info-definition-site info)
               (let ((mod-name (match-string-no-properties 1 info)))
                 (lcr-cps-let ((packages (dante-async-call ":show packages")))
                   (let* ((pkgs-without-versions (haskell-go-to-symbol-home--strip-ghci-packages-of-versions packages) ))
                     (haskell-go-to-symbol-home--jump-to-filtered-tags identifier
                                                                       mod-name
                                                                       pkgs-without-versions)))))
              ((string-match-p haskell-regexen/ghci-name-not-in-scope-error info)
               (error "Name not in scope, invoke eproj tags via M-."))
              (t
               (error "Failed to extract mod name from ghci result:\n%s" info)))))))))

(defun haskell-go-to-symbol-home-smart (&optional use-regexp?)
  (interactive "P")
  (cond
    ((dante-mode
      (if use-regexp?
          (eproj-symbnav/go-to-symbol-home use-regexp?)
        (haskell-dante-symbnav/go-to-symbol-home)))
     (lsp-mode
      (lsp-haskell-symbnav/go-to-symbol-home use-regexp?))
     (t
      (eproj-symbnav/go-to-symbol-home use-regexp?)))))

(defun haskell-find-references-smart ()
  (interactive)
  (if lsp-mode
      (lsp-symbnav/find-references)
    (xref-find-references)))

(defun haskell-go-to-symbol-home--ghc-src-span-to-eproj-tag (string)
  "Extract a location from a ghc span STRING."
  (when (string-match haskell-regexen/ghci-src-span string)
    (let* ((file (match-string-no-properties 1 string))
           (resolved-file
            (or (gethash file dante-original-buffer-map)
                (gethash (dante-local-name file) dante-original-buffer-map)
                file))
           (line (string-to-number (match-string-no-properties 2 string)))
           (col (string-to-number (match-string-no-properties 3 string))))
      (make-eproj-tag (expand-file-name resolved-file dante-project-root)
                      line
                      nil
                      (list `(column . ,(1- col)))))))

(defhydra hydra-haskell-lsp-toggle (:exit nil :foreign-keys nil :hint nil)
  "
Toggle:
_f_ormatting on typing             %`lsp-enable-on-type-formatting
_h_ighlight of symbol at point     %`lsp-enable-symbol-highlighting
_l_ens                             %`lsp-lens-mode
"
  ("f" lsp-toggle-on-type-formatting)
  ("h" lsp-toggle-symbol-highlight)
  ("l" lsp-lens-mode))

(defhydra-ext hydra-haskell-minus (:exit t :foreign-keys warn :hint nil)
  "
_q_ualify import  _-_: attrap
"
  ("q" vim:haskell-qualify-import:interactive)

  ("-" attrap-flycheck))

(defhydra-ext hydra-haskell-dante (:exit t :foreign-keys warn :hint nil)
  "
_i_nfo            _j_: eval
_t_ype            _-_: attrap
_q_ualify import"
  ("i" dante-info)
  ("t" dante-type-at)
  ("q" vim:haskell-qualify-import:interactive)

  ("j" dante-eval-block)
  ("-" attrap-flycheck))

(defhydra-ext hydra-haskell-lsp (:exit t :foreign-keys warn :hint nil)
  "
_-_: attrap       _d_ocumentation           toggle some _o_ptions
_a_ctions         _i_nfo
_q_ualify import  _t_ype
_r_ename          _u_ses of thing at point
"
  ("-" attrap-flycheck)
  ("a" lsp-execute-code-action)
  ("q" vim:haskell-qualify-import:interactive)
  ("r" lsp-rename)

  ("d" lsp-doc-other-window)
  ("i" lsp-doc-other-window)
  ("t" lsp-haskell-type-at-point)
  ("u" lsp-symbnav/find-references)

  ("o" hydra-haskell-lsp-toggle/body))

(defhydra-derive hydra-cabal-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_TAB_: align and sort subsection"
  ("<tab>" haskell-misc-cabal-align-and-sort-subsection))

(defhydra-derive hydra-haskell-vim-normal-j-ext hydra-vim-normal-j-ext (:exit t :foreign-keys nil :hint nil)
  ""
  ("cc" vim:haskell-comment-line:interactive))

(defhydra-derive hydra-haskell-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_i_:     jump to imports  _t_: jump to topmost node start
_<tab>_: reindent         _h_: jump to topmont node end"
  ("i"     vim:haskell-navigate-imports:interactive)
  ("<tab>" haskell-reindent-at-point)

  ("t"     haskell-move-to-topmost-start)
  ("h"     haskell-move-to-topmost-end))

(defhydra-derive hydra-haskell-vim-visual-g-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign            _t_: jump to topmost node start
_<tab>_: reindent  _h_: jump to topmont node end"
  ("a"     hydra-haskell-align/body)
  ("<tab>" haskell-reindent-region)

  ("t"     haskell-move-to-topmost-start)
  ("h"     haskell-move-to-topmost-end))

;;;###autoload
(defun haskell-setup-common-editing ()
  (def-keys-for-map vim-insert-mode-local-keymap
    ("'"  haskell-smart-operators-quote)
    ("`"  vim-wrap-backticks)

    ("\"" smart-operators-double-quote)
    ("\(" smart-operators-open-paren)
    ("\[" smart-operators-open-bracket))

  (def-keys-for-map vim-operator-pending-mode-local-keymap
    (("is" "s") vim:motion-inner-haskell-symbol:interactive)
    ("as"       vim:motion-outer-haskell-symbol:interactive)))

;;;###autoload
(defun haskell-setup ()
  (let ((non-vanilla-haskell-mode? (-any? #'derived-mode-p '(ghc-core-mode haskell-c2hs-mode haskell-hsc-mode))))
    (init-common :use-whitespace 'tabs-only)
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
      (setq-local company-backends (if proj
                                       '(company-files
                                         (company-eproj company-dabbrev-code)
                                         company-dabbrev)
                                     '(company-files
                                       company-dabbrev-code
                                       company-dabbrev))
                  flycheck-highlighting-mode 'symbols)

      (eproj-setup-local-variables proj)

      (when (not non-vanilla-haskell-mode?)
        (flycheck-setup-from-eproj
         proj
         'haskell-dante ;; default checker
         (lambda (backend)
           (pcase backend
             (`haskell-dante
              (setq-local company-backends (cons 'dante-company company-backends))
              (dante-mode +1))
             (`lsp
              ;; todo: check that lsp executable is around
              ;; (lsp)
              (lsp-deferred)
              (when lsp-mode
                (lsp-diagnostics-mode))))
           ;; (unless (flycheck-may-use-checker backend)
           ;;   (flycheck-verify-checker backend)
           ;;   (error "Unable to select checker '%s' for buffer '%s'"
           ;;          backend (current-buffer)))
           (when (memq backend '(haskell-dante))
             (add-hook 'flycheck-mode-hook #'haskell-misc--configure-dante! nil t))))))

    (turn-on-font-lock)

    ;; The underscore should remain part of word so we never search within
    ;; _c_style_identifiers.
    (modify-syntax-entry ?_  "_")
    (modify-syntax-entry ?\' "w")
    (modify-syntax-entry ?\@ "'")

    (setq-local eproj-symbnav/identifier-type 'haskell-symbol

                yas-indent-line 'fixed

                beginning-of-defun-function #'haskell-move-to-topmost-start-impl

                ;; Improve vim treatment of words for Haskell.
                ;; Note: underscore should not be included since it would prevent
                ;; navigating inside of some Haskell identifiers, e.g. foo_bar.
                vim-word "[:word:]'"

                compilation-read-command nil
                compilation-auto-jump-to-first-error nil
                ;; Don't skip any messages.
                compilation-skip-threshold 0)

    (def-keys-for-map vim-normal-mode-local-keymap
      ("SPC SPC"      vim:dante-repl-switch-to-repl-buffer:interactive)
      (("C-l" "<f6>") vim:haskell-dante-load-file-into-repl:interactive))

    ;; Dante doesn't play well with idle-change checks.
    (cond
      (dante-mode
       (setq-local flycheck-check-syntax-automatically
                   (if buffer-file-name
                       '(save mode-enabled)
                     ;; There may be no save step for a temporary buffer.
                     '(save mode-enabled new-line)))

       (dolist (cmd '("conf" "configure"))
         (vim-local-emap cmd #'vim:haskell-dante-configure))

       (def-keys-for-map vim-normal-mode-local-keymap
         ("-" hydra-haskell-dante/body))

       (flycheck-install-ex-commands!
        :install-flycheck flycheck-mode
        :load-func #'vim:haskell-dante-load-file-into-repl
        :reset-func #'vim:haskell-dante-reset))
      (lsp-mode
       (dolist (cmd '("conf-repl" "configure-repl"))
         (vim-local-emap cmd #'vim:haskell-dante-configure))

       (setq-local lsp-ui-sideline-show-code-actions t
                   lsp-ui-sideline-enable t
                   lsp-ui-sideline-ignore-duplicate t
                   lsp-ui-sideline-show-hover nil
                   ;; Maybe be a good idea to try enabling this one
                   lsp-ui-sideline-show-diagnostics nil
                   lsp-ui-sideline-delay 0.05)
       (lsp-ui-sideline-mode +1)
       (def-keys-for-map vim-normal-mode-local-keymap
         ("-"   hydra-haskell-lsp/body)
         ("C-r" lsp-rename))

       (flycheck-install-ex-commands!
        :install-flycheck flycheck-mode
        :load-func #'vim:haskell-dante-load-file-into-repl
        :reset-func #'vim:haskell-lsp-flycheck-reset))
      (flycheck-mode
       ;; Fallback, should rarely be reached since dante should handle most of the cases.
       (def-keys-for-map vim-normal-mode-local-keymap
         ("-" hydra-haskell-minus/body))

       (flycheck-install-ex-commands!
        :install-flycheck flycheck-mode
        :load-func #'vim:haskell-dante-load-file-into-repl:interactive)))

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

    (vim-local-emap "core" #'vim:ghc-core-create-core)

    (def-keys-for-map vim-normal-mode-local-keymap
      ("\\"           vim:flycheck-run:interactive)
      ("g"            hydra-haskell-vim-normal-g-ext/body)
      ("j"            hydra-haskell-vim-normal-j-ext/body)
      ("C-="          input-unicode))

    (def-keys-for-map vim-visual-mode-local-keymap
      ("`"            vim-wrap-backticks)
      ("g"            hydra-haskell-vim-visual-g-ext/body))

    (def-keys-for-map (vim-normal-mode-local-keymap
                       vim-visual-mode-local-keymap)
      ("*"            search-for-haskell-symbol-at-point-forward)
      ("C-*"          search-for-haskell-symbol-at-point-forward-new-color)
      ("#"            search-for-haskell-symbol-at-point-backward)
      ("C-#"          search-for-haskell-symbol-at-point-backward-new-color)
      ("'"            vim:haskell-backward-up-indentation-or-sexp:interactive))

    (haskell-setup-common-editing)

    (install-haskell-smart-operators!
        vim-insert-mode-local-keymap
      :bind-colon t
      :bind-hyphen t
      :track-extensions? t)

    (def-keys-for-map (vim-normal-mode-local-keymap
                       vim-insert-mode-local-keymap)
      ("DEL"         haskell-backspace-with-block-dedent)
      ("<backspace>" haskell-backspace-with-block-dedent)

      ("C-u"         haskell-insert-undefined)
      ("C-h"         flycheck-enhancements-next-error-with-wraparound)
      ("C-t"         flycheck-enhancements-previous-error-with-wraparound)
      ("M-h"         compilation-navigation-next-error-other-window)
      ("M-t"         compilation-navigation-prev-error-other-window)
      ("C-SPC"       company-complete)

      ;; Consider using haskell-indentation-newline-and-indent.
      ("<return>"    haskell-newline-with-signature-expansion)
      ("C-<return>"  haskell--simple-indent-newline-indent)

      (("S-<tab>" "<S-iso-lefttab>" "<backtab>") nil))

    (def-keys-for-map (vim-normal-mode-local-keymap
                       vim-visual-mode-local-keymap
                       vim-motion-mode-local-keymap
                       vim-operator-pending-mode-local-keymap)
      ("'" vim:haskell-backward-up-indentation-or-sexp:interactive)
      ("q" vim:haskell-up-sexp:interactive))

    (haskell-setup-folding)
    (setup-eproj-symbnav :bind-keybindings t)
    (setq-local xref-show-definitions-function #'eproj-xref-symbnav-show-xrefs
                xref-show-xrefs-function #'eproj-xref-symbnav-show-xrefs)
    (def-keys-for-map vim-normal-mode-local-keymap
      ("M-." haskell-go-to-symbol-home-smart)
      ("M-," eproj-symbnav/go-back)
      ("M-?" haskell-find-references-smart))))

;;;###autoload
(defun haskell-c2hs-setup ()
  (add-to-list 'flycheck-disabled-checkers 'haskell-hlint))

;;;###autoload
(defun haskell-hsc-setup ()
  (add-to-list 'flycheck-disabled-checkers 'haskell-hlint))

;;;###autoload
(defun dante-repl-mode-setup ()
  (init-common :use-comment nil
               :use-yasnippet nil
               :use-whitespace nil
               :use-fci nil
               :smerge nil)

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
  (install-haskell-smart-operators! vim-insert-mode-local-keymap
    :bind-colon nil
    :bind-hyphen nil
    :track-extensions? nil)

  (pretty-ligatures-install-safe!)
  (pretty-ligatures-install-special-haskell-ligatures!)

  (vim-local-emap "clear" #'vim:comint-clear-buffer-above-prompt)
  (dolist (cmd '("re" "restart"))
    (vim-local-emap cmd #'vim:haskell-dante-repl-restart))

  (def-keys-for-map vim-normal-mode-local-keymap
    ("H"        dante-repl-restart)
    ("SPC SPC"  comint-clear-prompt))

  (haskell-setup-common-editing)

  (def-keys-for-map vim-insert-mode-local-keymap
    ("-"        haskell--ghci-hyphen)
    (":"        haskell--ghci-colon))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("<tab>"    completion-at-point)
    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)

    ("C-t"      comint-previous-prompt)
    ("C-h"      comint-next-prompt)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt)

    ("C-SPC"    vim:comint-clear-buffer-above-prompt:interactive)
    ("M-p"      browse-comint-input-history))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-visual-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("C-<return>" dante-repl-newline))

  (haskell-setup-folding)
  (haskell-abbrev+-setup t))

;;;###autoload
(defun haskell-compilation-setup ()
  (pretty-ligatures-install-safe!)
  (pretty-ligatures-install-special-haskell-ligatures!)

  (vim-local-emap "c" #'vim:recompile))

;;;###autoload
(defun haskell-cabal-setup ()
  (init-common :use-comment t
               :use-yasnippet t)
  (haskell-setup-folding)
  (let ((proj (eproj-get-project-for-buf-lax (current-buffer))))
    (haskell-compilation-commands-install! proj))
  (modify-syntax-entry ?. "_")
  (setup-indent-size 2)
  (setq-local yas-indent-line 'fixed
              indent-line-function
              (lambda ()
                (indent-to standard-indent)))

  (bind-tab-keys #'tab-to-tab-stop
                 #'tab-to-tab-stop-backward
                 :enable-yasnippet t)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("SPC SPC" haskell-cabal-find-related-file)
    ("g"       hydra-cabal-vim-normal-g-ext/body)
    ("'"       yafolding-go-parent-element))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("<return>"     haskell--simple-indent-newline-same-col)
    ("C-<return>"   haskell--simple-indent-newline-indent))

  (def-keys-for-map vim-insert-mode-local-keymap
    ("," haskell-cabal-smart-operators-comma)))

;;;###autoload
(defun ghc-core-setup ()
  (pretty-ligatures-install!)
  (pretty-ligatures-install-special-haskell-ligatures!)
  (hl-line-mode +1)

  (setq-local beginning-of-defun-function #'haskell-move-to-topmost-start-impl))

(provide 'haskell-setup)

;; Local Variables:
;; End:

;; haskell-setup.el ends here
