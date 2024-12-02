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
(require 'haskell-flycheck-cabal-build)
(require 'haskell-misc)
(require 'haskell-outline)
(require 'haskell-syntax-table)
(require 'hydra-setup)
(require 'lcr)
(require 'lsp-setup)
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

;;;###autoload (autoload 'vim:dante-repl-switch-to-repl-buffer "haskell-setup" nil t)
;;;###autoload (autoload 'vim:dante-repl-switch-to-repl-buffer:interactive "haskell-setup" nil t)
(vim-defcmd vim:dante-repl-switch-to-repl-buffer (nonrepeatable)
  (haskell-misc--configure-dante-if-needed!)
  (dante-repl-switch-to-repl-buffer))

;;;###autoload (autoload 'vim:haskell-dante-load-file-into-repl "haskell-setup" nil t)
;;;###autoload (autoload 'vim:haskell-dante-load-file-into-repl:interactive "haskell-setup" nil t)
(vim-defcmd vim:haskell-dante-load-file-into-repl (nonrepeatable)
  (haskell-misc--configure-dante-if-needed!)
  (dante-repl-load-file))

(vim-defcmd vim:haskell-dante-repl-restart (nonrepeatable)
  "Restart dante repl."
  (dante-repl-restart nil))

(vim-defcmd vim:haskell-lsp-flycheck-reset (nonrepeatable)
  "Restart lsp checker session."
  (vim:flycheck-clear:wrapper)
  (when (and (boundp 'flycheck-checker)
             (eq flycheck-checker 'lsp))
    (lsp-workspace-restart (lsp--read-workspace))))

(vim-defcmd vim:haskell-dante-reset (nonrepeatable)
  "Destroy dante checker session and attempt to create a new one."
  ;; Don’t use ‘dante-restart’ because it won’t have any effect if there’s no
  ;; dante buffer.
  (unless dante-mode
    (error "dante is not enabled"))
  (vim:flycheck-clear:wrapper)
  (dante-destroy)
  (lcr-spawn (lcr-call dante-start))
  (flycheck-buffer))

(vim-defcmd vim:haskell-dante-configure (nonrepeatable)
  (haskell-misc--configure-dante!))

(vim-defcmd vim:dante-clear-buffer-above-prompt (nonrepeatable)
  "Clear text above ghci prompt."
  (dante-repl-clear-buffer-above-prompt))

;;;###autoload (autoload 'vim:haskell-navigate-imports "haskell-setup" nil t)
;;;###autoload (autoload 'vim:haskell-navigate-imports:interactive "haskell-setup" nil t)
(vim-defcmd vim:haskell-navigate-imports (nonrepeatable)
  "Jump to the imports section."
  (vim-save-position)
  (haskell-navigate-imports)
  (setq-local vim-save-position-omit-next t))

(vim-defcmd vim:haskell-comment-line (count repeatable)
  (haskell-comment-line count))

(defun haskell-update-eproj-tags-on-save ()
  (ignore-errors
    (eproj-update-current-buffer-within-its-project!)))

(vim-defcmd vim:haskell-qualify-import (repeatable)
  (haskell-qualify-import))

(vim-defcmd vim:replace-haskell-symbol-at-point (nonrepeatable)
  "Partially construct vim ex-replace command from symbol at point.
With prefix argument puts symbol at point also in substitute part"
  (vim:replace-symbol-at-point--impl 'haskell-symbol))

(vim-defcmd vim:replace-qualified-haskell-symbol-at-point (nonrepeatable)
  "Partially construct vim ex-replace command from symbol at point.
With prefix argument puts symbol at point also in substitute part"
  (vim:replace-symbol-at-point--impl 'qualified-haskell-symbol))

(vim-defcmd vim:attrap-flycheck (repeatable)
  (attrap-flycheck (point)))

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

(defun haskell-dante--strip-instances-from-ghci-info (str)
  "The info may look like, for a type ‘Foo’ that comes from
third-party package but has instances defined in current project:

```
Foo in `Foo.hs'

type Foo :: * -> *
data Foo a = Foo | Bar a a
  	-- Defined in ‘Data.Foo’
instance Pretty a => Pretty (Foo a)
  -- Defined at packages/pretty-instances/src/Prettyprinter/Instances.hs
```

This function will strip everything after the first ‘instance’ in order for location
regexps to not be confused by the instance location."
  (if-let (end (string-search "\ninstance" str))
      (substring str nil end)
    str))

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
                      (raw-info (dante-async-call (concat ":i " identifier))))
          (let ((info (haskell-dante--strip-instances-from-ghci-info raw-info)))
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
                 (error "Failed to extract mod name from ghci result:\n%s" info))))))))))

(defun haskell-go-to-symbol-home-smart (&optional use-regexp?)
  (interactive "P")
  (cond
    (dante-mode
     (if use-regexp?
         (eproj-symbnav/go-to-symbol-home use-regexp?)
       (haskell-dante-symbnav/go-to-symbol-home)))
    (lsp-mode
     (lsp-haskell-symbnav/go-to-symbol-home use-regexp?))
    (t
     (eproj-symbnav/go-to-symbol-home use-regexp?))))

(defun haskell-find-references-smart ()
  (interactive)
  (if lsp-mode
      (lsp-symbnav/find-references)
    (let ((identifier (eproj-symbnav/identifier-at-point nil)))
      (xref-find-references identifier))))

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
      (make-eproj-tag (expand-file-name resolved-file (dante-project-root))
                      line
                      nil
                      (list `(column . ,(1- col)))))))

(defhydra-derive hydra-haskell-lsp-toggle hydra-lsp-toggle (:exit t :foreign-keys nil :hint nil)
  "")

(defhydra-ext hydra-haskell-minus (:exit t :foreign-keys warn :hint nil)
  "
_q_ualify import  _-_: attrap
"
  ("q" vim:haskell-qualify-import:interactive)

  ("-" vim:attrap-flycheck:interactive))

(defhydra-ext hydra-haskell-dante (:exit t :foreign-keys warn :hint nil)
  "
_i_nfo            _j_: eval
_t_ype            _-_: attrap
_q_ualify import
_e_xport"
  ("i" dante-info)
  ("t" dante-type-at)
  ("q" vim:haskell-qualify-import:interactive)
  ("e" haskell-export-ident-at-point)

  ("j" dante-eval-block)
  ("-" vim:attrap-flycheck:interactive))

(defhydra-ext hydra-haskell-lsp (:exit t :foreign-keys warn :hint nil)
  "
_-_: attrap       _d_ocumentation           toggle some _o_ptions
_a_ctions         _i_nfo
_q_ualify import  _t_ype
_r_ename          _u_ses of thing at point
_e_xport
"
  ("-" vim:attrap-flycheck:interactive)
  ("a" lsp-execute-code-action)
  ("q" vim:haskell-qualify-import:interactive)
  ("r" lsp-rename)
  ("e" haskell-export-ident-at-point)

  ("d" lsp-doc-other-window)
  ("i" lsp-doc-other-window)
  ("t" lsp-haskell-type-at-point)
  ("u" lsp-symbnav/find-references)

  ("o" hydra-haskell-lsp-toggle/body))

(defhydra-derive hydra-cabal-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_TAB_: align and sort subsection"
  ("<tab>" haskell-misc-cabal-align-and-sort-subsection))

(defhydra-derive-special-docstring hydra-haskell-vim-normal-j-ext hydra-vim-normal-j-ext (:exit t :foreign-keys nil :hint nil)
  (lambda (str)
    (save-match-data
      (let* ((substr  "ta_b_s      _cd_: delete comment  _ss_: replace symbol\nu_n_narrow\n\n")
             (new-row "u_n_narrow                        _sS_: replace qualified symbol"))
        (if (string-match (regexp-quote substr) str)
            (replace-match (concat (seq-take-while (lambda (x) (not (eq x ?\n))) substr) "\n"
                                   new-row "\n\n")
                           nil
                           t
                           str)
          (error "Docstring of ‘hydra-haskell-vim-normal-j-ext’s parent has unexpected shape, please fix definition of ‘hydra-haskell-vim-normal-j-ext’.")))))
  ("ss" vim:replace-haskell-symbol-at-point:interactive)
  ("sS" vim:replace-qualified-haskell-symbol-at-point:interactive)
  ("cc" vim:haskell-comment-line:interactive))

(defhydra-derive hydra-haskell-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_i_:     jump to imports  _t_: jump to topmost node start
_<tab>_: reindent         _h_: jump to topmont node end"
  ("i"     vim:haskell-navigate-imports:interactive)
  ("<tab>" haskell-reindent-at-point)

  ("t"     haskell-move-to-topmost-start)
  ("h"     haskell-move-to-topmost-end))

(defhydra-derive hydra-haskell-ts-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_i_:     jump to imports  _t_: jump to topmost function/entity start
_<tab>_: reindent         _h_: jump to topmont function/entity end"
  ("i"     vim:haskell-navigate-imports:interactive)
  ("<tab>" haskell-ts-reindent-at-point)

  ("t"     haskell-ts-beginning-of-defun)
  ("h"     haskell-ts-end-of-defun))

(defhydra-derive hydra-haskell-vim-visual-g-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign            _t_: jump to topmost node start
_<tab>_: reindent  _h_: jump to topmont node end"
  ("a"     hydra-haskell-align/body)
  ("<tab>" haskell-reindent-region)

  ("t"     haskell-move-to-topmost-start)
  ("h"     haskell-move-to-topmost-end))

(defhydra-derive hydra-haskell-ts-vim-visual-g-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign            _t_: jump to topmost function/entity start
_<tab>_: reindent  _h_: jump to topmont function/entity end"
  ("a"     hydra-haskell-align/body)
  ("<tab>" haskell-ts-reindent-region)

  ("t"     haskell-ts-beginning-of-defun)
  ("h"     haskell-ts-end-of-defun))

(defhydra-derive hydra-haskell-vim-visual-j-ext hydra-vim-visual-j-ext (:exit t :foreign-keys nil :hint nil)
  ""
  ("cu" haskell-uncomment-region-simple))

;;;###autoload
(defun haskell-setup-common-editing ()

  (setq-local paredit-forward-sexp-function #'haskell-forward-sexp-no-pairing

              yafolding-empty-line-function #'haskell-on-blank-line?

              eproj-symbnav/identifier-type 'haskell-symbol

              ;; Improve vim treatment of words for Haskell.
              ;; Note: underscore should not be included since it would prevent
              ;; navigating inside of some Haskell identifiers, e.g. foo_bar.
              vim-word "[:word:]'"

              search-syntax-table haskell-search-fixed-syntax-table
              search-ignore-syntax-text-properties t)

  (def-keys-for-map vim-insert-mode-local-keymap
    ("'"  haskell-smart-operators-quote)

    ("\"" smart-operators-double-quote)
    ("\(" haskell-smart-operators-open-paren)
    ("\[" haskell-smart-operators-open-bracket)
    ("\{" haskell-smart-operators-open-brace))

  (def-keys-for-map (vim-insert-mode-local-keymap
                     vim-visual-mode-local-keymap)
    ("`"  vim-wrap-backticks))

  (def-keys-for-map vim-visual-mode-local-keymap
    ("j"  hydra-haskell-vim-visual-j-ext/body))

  (def-keys-for-map vim-operator-pending-mode-local-keymap
    (("is" "s") vim:motion-inner-haskell-symbol:interactive)
    ("as"       vim:motion-outer-haskell-symbol:interactive)
    (("iS" "S") vim:motion-inner-qualified-haskell-symbol:interactive)
    ("aS"       vim:motion-outer-qualified-haskell-symbol:interactive)))

(defun haskell-setup-common-prelude ()
  (init-common :use-whitespace 'tabs-only)
  (add-hook 'after-save-hook #'haskell-update-eproj-tags-on-save nil t)

  ;; Order is important, otherwise special forall fontification doesn’t work
  (pretty-ligatures-install-special-haskell-ligatures!)
  (pretty-ligatures-install!)

  (turn-on-font-lock)

  (setq-local flycheck-enhancements--get-project-root-for-current-buffer
              #'haskell-misc-get-project-root

              yas-indent-line 'fixed

              beginning-of-defun-function #'haskell-move-to-topmost-start-impl

              compilation-read-command nil
              compilation-auto-jump-to-first-error nil
              ;; Don't skip any messages.
              compilation-skip-threshold 0

              find-tag-default-function #'haskell-misc-find-tag-default

              pseudovim-motion-jump-item--matching-comment-start-str "{-"
              pseudovim-motion-jump-item--matching-comment-end-str "-}"))

(defun haskell-setup-common-project (set-up-indentation?)
  ;; Read settings from '.eproj-info' file, if any.
  (let (
        ;; NB may be nil.
        (proj (eproj-get-project-for-buf-lax (current-buffer))))

    (haskell-compilation-commands-install! proj)

    (when set-up-indentation?
      (haskell-setup-indentation (eproj-query/haskell/indent-offset proj) nil))

    (haskell-abbrev+-setup nil)

    (company-mode +1)
    (setq-local company-backends (if proj
                                     '(company-files
                                       (company-eproj company-dabbrev-code)
                                       company-dabbrev)
                                   '(company-files
                                     company-dabbrev-code
                                     company-dabbrev))
                company-transformers
                '(delete-duplicate-candidates-from-company-dabbrev-code)
                flycheck-highlighting-mode 'symbols)

    (eproj-setup-local-variables proj)

    proj))

(defun haskell-setup-common-keybindings (should-enable-flycheck?)
  (def-keys-for-map vim-normal-mode-local-keymap
    ("SPC SPC"      vim:dante-repl-switch-to-repl-buffer:interactive)
    (("C-l" "<f6>") vim:haskell-dante-load-file-into-repl:interactive))

  (def-keys-for-map vim-normal-mode-local-keymap
    ("\\"           vim:flycheck-run:interactive)
    ("g"            hydra-haskell-vim-normal-g-ext/body)
    ("j"            hydra-haskell-vim-normal-j-ext/body)
    ("C-="          input-unicode))

  (def-keys-for-map vim-visual-mode-local-keymap
    ("g"            hydra-haskell-vim-visual-g-ext/body))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-visual-mode-local-keymap)
    ("*"            search-for-haskell-symbol-at-point-forward)
    ("C-*"          search-for-haskell-symbol-at-point-forward-new-color)
    ("#"            search-for-haskell-symbol-at-point-backward)
    ("C-#"          search-for-haskell-symbol-at-point-backward-new-color))

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
    ("M-?" haskell-find-references-smart))

  (awhen should-enable-flycheck?
    (flycheck-mode it))

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
                         '(:eval (flycheck-pretty-mode-line))))))))

;;;###autoload
(defun haskell-ts-setup ()
  (def-keys-for-map vim-normal-mode-local-keymap
    ("g"   hydra-haskell-ts-vim-normal-g-ext/body))

  (def-keys-for-map vim-visual-mode-local-keymap
    ("g"   hydra-haskell-ts-vim-visual-g-ext/body))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("C-r" haskell-ts-rename-at-point)))

;;;###autoload
(defun haskell-setup ()
  (let ((non-vanilla-haskell-mode? (-any? #'derived-mode-p '(ghc-core-mode haskell-c2hs-mode haskell-hsc-mode)))
        (using-lsp? nil)
        (should-enable-flycheck? nil))

    (haskell-setup-common-prelude)

    (add-hook 'after-save-hook #'make-script-file-exec nil t)

    (let ((proj (haskell-setup-common-project t)))
      (when (not non-vanilla-haskell-mode?)
        (flycheck-setup-from-eproj-deferred
         proj
         +haskell-default-checker+
         (lambda (backend)
           (pcase backend
             (`haskell-dante
              (setq-local company-backends
                          '(company-files
                            (dante-company company-eproj company-dabbrev-code)
                            company-dabbrev))
              (dante-mode +1))
             (`lsp
              ;; todo: check that lsp executable is around
              (lsp-deferred)
              (setf using-lsp? t)
              (lsp-diagnostics-mode)
              ))
           ;; (unless (flycheck-may-use-checker backend)
           ;;   (flycheck-verify-checker backend)
           ;;   (error "Unable to select checker '%s' for buffer '%s'"
           ;;          backend (current-buffer)))
           )
         (lambda (should-enable?)
           (setf should-enable-flycheck? (if should-enable? +1 -1))))))

    (haskell-setup-common-keybindings should-enable-flycheck?)

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
      (using-lsp?
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
        :load-func #'vim:haskell-dante-load-file-into-repl:interactive))
      (t
       ;; Fallback for a fallback, in case some errors occured during setup.
       (def-keys-for-map vim-normal-mode-local-keymap
         ("-" hydra-haskell-minus/body))))))

;;;###autoload
(defun haskell-c2hs-setup ()
  (add-to-list 'flycheck-disabled-checkers 'haskell-hlint))

;;;###autoload
(defun haskell-hsc-setup ()
  (let ((should-enable-flycheck? nil))
    (haskell-setup-common-prelude)

    (add-to-list 'flycheck-disabled-checkers 'haskell-hlint)

    (let ((proj (haskell-setup-common-project t)))
      (flycheck-setup-from-eproj-deferred proj
                                          'haskell-cabal-build
                                          #'ignore
                                          (lambda (should-enable?)
                                            (setf should-enable-flycheck? (if should-enable? +1 -1)))))

    (haskell-setup-common-keybindings should-enable-flycheck?)

    (def-keys-for-map vim-normal-mode-local-keymap
      ("-" hydra-haskell-minus/body))

    (flycheck-install-ex-commands!
     :install-flycheck flycheck-mode
     :load-func #'vim:haskell-dante-load-file-into-repl:interactive)))

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

  (pretty-ligatures-install-special-haskell-ligatures!)
  (pretty-ligatures-install-safe!)

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

    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt)

    ("C-SPC"    vim:comint-clear-buffer-above-prompt:interactive)
    ("M-p"      browse-comint-input-history))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-visual-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("C-t"        comint-previous-prompt)
    ("C-h"        comint-next-prompt)
    ("C-<return>" dante-repl-newline))

  (haskell-setup-folding)
  (haskell-abbrev+-setup t))

;;;###autoload
(defun haskell-compilation-setup ()
  (pretty-ligatures-install-special-haskell-ligatures!)
  (pretty-ligatures-install-safe!)

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
(defun ghc-core-mode-setup ()
  (init-common :use-comment t
               :use-yasnippet t)
  (haskell-setup-folding :enable-cpp nil)
  (pretty-ligatures-install-special-haskell-ligatures!)
  (pretty-ligatures-install!)
  (hl-line-mode +1)
  (setq-local search-syntax-table ghc-core-symbol--identifier-syntax-table
              search-ignore-syntax-text-properties t)

  (setq-local beginning-of-defun-function #'haskell-move-to-topmost-start-impl)

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-visual-mode-local-keymap)
    ("*"          search-for-ghc-core-symbol-at-point-forward)
    ("C-*"        search-for-ghc-core-symbol-at-point-forward-new-color)
    ("#"          search-for-ghc-core-symbol-at-point-backward)
    ("C-#"        search-for-ghc-core-symbol-at-point-backward-new-color))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("C-<return>" haskell--simple-indent-newline-indent)))

;;;###autoload
(defun cmm-setup ()
  (init-common :use-render-formula nil
               :use-yasnippet nil
               :use-whitespace 'tabs-only)
  (hl-line-mode +1)
  (setq-local beginning-of-defun-function #'haskell-move-to-topmost-start-impl))

(provide 'haskell-setup)

;; Local Variables:
;; End:

;; haskell-setup.el ends here
