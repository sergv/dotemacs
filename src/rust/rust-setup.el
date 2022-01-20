;; rust-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 22 March 2018
;; Description:

(eval-when-compile
  (require 'set-up-platform)
  (require 'macro-util)

  (defvar whitespace-line-column)
  (defvar sp-report-unmatched-expressions))

(declare-function server-edit "server")

(require 'common)
(require 'flycheck)
(require 'haskell-compile)
(require 'indentation)
(require 'lsp-rust)
(require 'lsp-setup)
(require 'lsp-rust-setup)
(require 'pretty-ligatures)
(require 'rust-compilation-commands)
(require 'smartparens-rust)
(require 'smartparens-setup)

(require 'rust-autoloads)

(defun flycheck-cargo--make-check-args (dir)
  (append '("--offline")
          (when dir
            (list (format "--target-dir=%s"
                          dir)))))

(defvar flycheck-cargo--default-check-args
  (flycheck-cargo--make-check-args (fold-platform-os-type "/tmp/target" "target")))

(setf rust-indent-method-chain t
      rust-indent-where-clause t

      rust-playpen-url-format nil
      rust-shortener-url-format nil

      flycheck-cargo-check-args flycheck-cargo--default-check-args)

(let ((cargo-home (getenv "CARGO_HOME"))
      (rustup-home (getenv "RUSTUP_HOME")))
  (when (or cargo-home
            rustup-home)
    (setf lsp-rust-library-directories nil)
    (awhen cargo-home
      (push (concat it "/registry/src") lsp-rust-library-directories))
    (awhen rustup-home
      (push (concat it "/toolchains") lsp-rust-library-directories))))

(puthash 'rust-mode
         #'rust-format-buffer
         *mode-indent-functions-table*)

(setf sp-sexp-prefix
      (cons '(rust-mode regexp "[&]")
            (--remove (eq (car it) 'rust-mode)
                      sp-sexp-prefix)))

;;;; Utilities

(defconst rust-compilation-buffer-name "*rust-compilation*")

(defun rust-get-compilation-buffer-name (&rest _args)
  rust-compilation-buffer-name)

(defun rust-compilation-next-error ()
  "Select next error in `rust-compilation-buffer-name' buffer and jump to
it's position in current window."
  (interactive)
  (text-property-jump-forward 'compilation-message nil t nil))

(defun rust-compilation-prev-error ()
  "Select previous error in `rust-compilation-buffer-name' buffer and jump to
it's position in current window."
  (interactive)
  (text-property-jump-backward 'compilation-message nil t nil))

(defun rust-compilation-next-error-other-window ()
  "Select next error in `rust-compilation-buffer-name' buffer and jump to
it's position in current window."
  (interactive)
  (aif (get-buffer rust-compilation-buffer-name)
      (let ((err (with-selected-window (get-buffer-window it t)
                   (with-current-buffer it
                     (rust-compilation-next-error)
                     (when hl-line-mode
                       (hl-line-highlight))
                     (rust-compilation--error-at-point)))))
        (compilation/jump-to-error err nil))
    (error "No Rust compilation buffer")))

(defun rust-compilation-prev-error-other-window ()
  "Select previous error in `rust-compilation-buffer-name' buffer and jump to
it's position in current window."
  (interactive)
  (aif (get-buffer rust-compilation-buffer-name)
      (let ((err (with-selected-window (get-buffer-window it t)
                   (with-current-buffer it
                     (rust-compilation-prev-error)
                     (when hl-line-mode
                       (hl-line-highlight)))
                   (rust-compilation--error-at-point))))
        (compilation/jump-to-error err nil))
    (error "No Rust compilation buffer")))

(defvar rust-compilation-extra-error-modes '(gnu)
  "Extra modes from `compilation-error-regexp-alist-alist' whose
warnings will be colorized in `rust-compilation-mode'.")

(defun rust-compilation-filter-hook ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(defun rust--prettify-symbols-compose-p (start end match)
  "Return true iff the symbol MATCH should be composed.
The symbol starts at position START and ends at position END.
This is the default for `prettify-symbols-compose-predicate'
which is suitable for most programming languages such as C or Lisp."
  (and (pretty-ligatures--compose-p start end match)
       (if (string-match-p (rx bos (or "&&" "<<" ">>" "||") eos)
                           match)
           (let ((start-char (or (char-before start) ?\s))
                 (end-char (or (char-after end) ?\s))
                 (is-closure?
                  (save-excursion
                    (goto-char start)
                    (skip-syntax-backward " >")
                    (awhen (char-before)
                      (or (memq it '(?\( ?, ?\{ ?=))
                          (text-before-matches? "move")
                          (text-before-matches? "return"))))))
             (and (not is-closure?)
                  (or (char= start-char ?\n)
                      (char= (char-syntax start-char) ?\ ))
                  (or (memq end-char '(?\r ?\n))
                      (char= (char-syntax end-char) ?\ ))))
         t)))

(defalign rust-align-on-equals
  (rx (? (or (any ?+ ?* ?| ?& ?/ ?! ?% ?- ?^)
             "<<"
             ">>"))
      "="
      (not ?=))
  :require-one-or-more-spaces t)

(vim-defcmd vim:rust-flycheck-configure (nonrepeatable)
  (unless rust-flycheck-configure
    (error "Don’t know how to configure %s checker"
           flycheck-cherker)))

(defun rust-flycheck-configure ()
  (when (and (boundp 'flycheck-checker)
             (memq flycheck-checker '(rust rust-clippy)))
    (flycheck-rust-setup)
    t))

(vim-defcmd vim:rust-flycheck-reset (nonrepeatable)
  (rust-flycheck-reset))

(defun rust-flycheck-reset ()
  (interactive)
  (vim:flycheck-clear)
  (when (and (boundp 'flycheck-checker)
             (eq flycheck-checker 'lsp))
    (lsp-restart-workspace)))

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'rust-flycheck-configure))

;;;;; rust-compilation-mode

(add-to-list 'compilation-transform-file-match-alist
             (list "/rustc/" nil))

(defconst cargo-test-regexps
  (list
   (list (rx (or (seq bol
                      (* " ")
                      "at")
                 ",")
             " "
             (group-n 1 (+? not-newline))
             ":"
             (group-n 2 (+ (any (?0 . ?9))))
             (? ":"
                (group-n 3 (+ (any (?0 . ?9)))))
             eol)
         1   ;; file group
         2   ;; line group
         3   ;; cloumn group
         0   ;; error type, 0 = info
         nil ;; hyperlink
         )
   ;; Output of dbg! macro
   (list (rx bol
             "["
             (group-n 1 (+ not-newline))
             ":"
             (group-n 2 (+ (any (?0 . ?9))))
             "]")
         1   ;; file group
         2   ;; line group
         nil ;; cloumn group
         0   ;; error type, 0 = info
         nil ;; hyperlink
         ))
  "Regexp to highlight backtrace positions when tests fail.")

(define-compilation-mode rust-compilation-mode "Rust Compilation"
  "Rust-specific `compilation-mode' derivative."
  (setq-local compilation-error-regexp-alist
              (append (list rustc-compilation-regexps
                            rustc-colon-compilation-regexps
                            cargo-compilation-regexps)
                      cargo-test-regexps
                      rust-compilation-extra-error-modes)
              *compilation-jump-error-regexp*
              (mapconcat (lambda (x) (concat "\\(?:" (car x) "\\)"))
                         (-filter #'listp compilation-error-regexp-alist)
                         "\\|")

              compilation-environment '("TERM=xterm-256color"))

  (vim-local-emap "c" 'vim:recompile)

  (add-hook 'compilation-filter-hook #'rust-compilation-filter-hook nil t))

(defun rust-compilation--error-at-point ()
  (aif (plist-get (text-properties-at (point)) 'compilation-message)
      (let* ((loc (compilation--message->loc it))
             (file (caar (compilation--loc->file-struct loc)))
             (line (compilation--loc->line loc))
             (col (awhen (compilation--loc->col loc) (1- it))))
        (make-compilation-error :compilation-root-directory default-directory
                                :filename file
                                :line-number line
                                :column-number col))
    (error "No compilation error at point")))

(defun rust-compilation-jump-to-error (other-window)
  (compilation/jump-to-error (rust-compilation--error-at-point) other-window))

(defun rust-compilation-goto-error ()
  "Jump to location of error or warning (file, line and column) in current window."
  (interactive)
  (rust-compilation-jump-to-error nil))

(defun rust-compilation-goto-error-other-window ()
  "Jump to location of error or warning (file, line and column) in other window."
  (interactive)
  (rust-compilation-jump-to-error t))


(def-keys-for-map rust-compilation-mode-map
  ("<up>"     rust-compilation-prev-error)
  ("<down>"   rust-compilation-next-error)
  ("t"        rust-compilation-prev-error)
  ("h"        rust-compilation-next-error)
  ("<return>" rust-compilation-goto-error)
  ("SPC"      rust-compilation-goto-error-other-window))

;;;;; rust-syntax-mode

(define-derived-mode rust-syntax-mode special-mode "Rust syntax"
  "Major mode to highlight Rust programs."
  :group 'rust-mode
  :syntax-table rust-mode-syntax-table
  (rust-mode--set-up-local-vars))

;;;;

(defhydra hydra-rust-toggle (:exit nil :foreign-keys nil :hint nil)
  "
Toggle:
_f_ormatting on typing             %`lsp-enable-on-type-formatting
_h_ighlight of symbol at point     %`lsp-enable-symbol-highlighting
_i_nlay hints                      %`lsp-rust-analyzer-inlay-hints-mode
_l_ens                             %`lsp-lens-mode
"
  ("f" lsp-toggle-on-type-formatting)
  ("h" lsp-toggle-symbol-highlight)
  ("i" lsp-rust-analyzer-inlay-hints-mode)
  ("l" lsp-lens-mode))

(defhydra-ext hydra-rust-dash (:exit t :foreign-keys nil :hint nil)
  "
_a_ctions                 jump to _c_argo toml     toggle some _o_ptions
_d_ocumentation           jump to _p_arent module
_e_xplain error at point
_i_mplementations
_m_acro expand
_r_ename
_t_ype
_u_sages"
  ("a" lsp-execute-code-action)
  ("d" lsp-doc-other-window)
  ("e" flycheck-explain-error-at-point)
  ("i" lsp-symbnav/find-implementations)
  ("m" lsp-rust-analyzer-expand-macro)
  ("r" lsp-rename)
  ("t" lsp-rust-type-of-thing-at-point)
  ("u" lsp-symbnav/find-references)

  ("c" lsp-rust-open-cargo-toml)
  ("p" lsp-rust-find-parent-module)

  ("o" hydra-rust-toggle/body))

(defhydra-ext hydra-rust-visual-dash (:exit t :foreign-keys nil :hint nil)
  "
_a_ctions
_t_ype of region"
  ("a" lsp-execute-code-action)
  ("t" lsp-rust-type-of-thing-at-point))

(defun lsp-rust-open-cargo-toml ()
  "Open Cargo.toml for current project."
  (interactive)
  (aif (lsp-request "experimental/openCargoToml" (lsp--text-document-position-params))
      (find-file (lsp--uri-to-path (lsp:location-uri it)))
    (error "Failed to locate Cargo.toml")))

(defhydra-ext hydra-rust-align (:exit t :foreign-keys nil :hint nil)
  "
_=_: on equals"
  ("=" rust-align-on-equals))

(defhydra-derive hydra-rust-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_t_: beginning of defun
_h_: end of defun"
  ("t" rust-beginning-of-defun)
  ("h" rust-end-of-defun))

(defun vim-rust-beginning-of-defun (&optional arg)
  "Vim wrapper around `rust-beginning-of-defun'."
  (interactive "p")
  (vim-save-position)
  (rust-beginning-of-defun arg))

(defun vim-rust-end-of-defun ()
  "Vim wrapper around `rust-end-of-defun'."
  (interactive)
  (vim-save-position)
  (rust-end-of-defun))

(defhydra-derive hydra-rust-vim-visual-g-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign                _t_: beginning of defun
_<tab>_: format region _h_: end of defun"
  ("a"     hydra-rust-align/body)
  ("<tab>" rust-format-region)

  ("t"     vim-rust-beginning-of-defun)
  ("h"     vim-rust-end-of-defun:interactive))

(defun rust-insert-unimplemented ()
  "Insert unimplemented!()."
  (interactive "*")
  (let ((start (point)))
    (when (and (looking-back "[^\[\(\{;, ]" (line-beginning-position))
               (not (bolp)))
      (insert " ")
      (setq start (1+ start)))
    (when (and (looking-at-p "[^\]\)\},; ]+_*")
               (not (eolp)))
      (insert " ")
      (forward-char -1))
    (insert "unimplemented!()")
    (evaporate-region start (point))
    (goto-char start)))

(defconst rust-regexen/empty-line "^\\(?:[ \t]*$\\)")

(defun rust-back-up-indent-level ()
  "Move up to lesser indentation level, skipping empty lines.

Returns t if indentation occured."
  (let ((start-indent (indentation-size))
        (col (current-column)))
    (cond
      ((> col start-indent)
       (back-to-indentation)
       t)
      ;; Do not move past 0th column in order to not skip to the
      ;; beginning of file.
      ((/= 0 start-indent)
       ;;(= col start-indent)
       (while (and (not (bobp))
                   (let ((curr-indent (indentation-size)))
                     (or (>= curr-indent start-indent)
                         (> curr-indent col))))
         (forward-line -1)
         (while (looking-at-p rust-regexen/empty-line)
           (forward-line -1)))
       (back-to-indentation)
       t)
      (t
       nil))))

;;;###autoload
(defun rust-backward-up-indentation-or-sexp ()
  "Rust brother of `sp-backward-up-sexp' that considers both
sexps and indentation levels."
  (interactive)
  (let* ((start (point))
         (sp-report-unmatched-expressions nil)
         (with-indentation
          (with-demoted-errors
              (save-excursion
                (rust-back-up-indent-level)
                (let ((p (point)))
                  (when (/= p start)
                    p)))))
         (with-sp
          (when (/= 0 (syntax-ppss-depth (syntax-ppss start)))
            (with-demoted-errors
                (save-excursion
                  (sp-backward-up-sexp)
                  (let ((p (point)))
                    (when (/= p start)
                      p)))))))
    (if (and with-indentation
             with-sp)
        (goto-char (max with-indentation with-sp))
      (goto-char (or with-indentation
                     with-sp
                     (error "Both indentation-based and sexp-based navigations failed"))))))

(vimmize-motion rust-backward-up-indentation-or-sexp
                :name vim:rust-backward-up-indentation-or-sexp
                :exclusive t
                :do-not-adjust-point t)

;;;; Setup

;;;###autoload
(defun rust-setup ()
  (init-common :use-render-formula nil
               :sp-slurp-sexp-insert-space nil
               :use-yasnippet t
               :use-whitespace t
               :hl-parens-backend 'smartparens)
  (fontify-conflict-markers!)
  (setup-folding t '(:header-symbol "/" :length-min 3))
  (company-mode +1)
  (setq-local ;; Don't skip any messages.
   compilation-skip-threshold 0
   compilation-buffer-name-function #'rust-get-compilation-buffer-name)

  (pretty-ligatures--install (append pretty-ligatures-c-like-symbols
                                     pretty-ligatures-python-like-words))
  (setq-local prettify-symbols-compose-predicate #'rust--prettify-symbols-compose-p)

  (bind-tab-keys #'indent-for-tab-command
                 #'tab-to-tab-stop-backward
                 :enable-yasnippet t)

  (setf vim-shift-width rust-indent-offset
        tab-width rust-indent-offset)

  (let (;; NB may be nil.
        (proj (eproj-get-project-for-buf-lax (current-buffer))))

    (setq-local company-backends
                `(company-files
                  ,(if proj
                       '(company-eproj company-dabbrev-code company-keywords)
                     '(company-dabbrev-code company-keywords))
                  company-dabbrev))

    (rust-compilation-commands-install! proj)
    (eproj-setup-local-variables proj)
    (flycheck-setup-from-eproj proj 'lsp)
    ;; (flycheck-setup-from-eproj proj 'rust-clippy)
    )

  (setq-local whitespace-line-column 100
              whitespace-style '(face tabs lines-tail)

              mode-line-format
              (apply #'default-mode-line-format
                     (when flycheck-mode
                       (list
                        " "
                        '(:eval (flycheck-pretty-mode-line)))))

              lsp-ui-sideline-show-code-actions t
              lsp-ui-sideline-enable t
              lsp-ui-sideline-ignore-duplicate t
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-show-diagnostics nil
              lsp-ui-sideline-delay 0.05)

  (dolist (cmd '("conf" "configure"))
    (vim-local-emap cmd #'vim:rust-flycheck-configure))

  (flycheck-install-ex-commands!
   :install-flycheck flycheck-mode
   :reset-func #'vim:rust-flycheck-reset:interactive)

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-visual-mode-local-keymap
                     vim-operator-pending-mode-keymap
                     vim-motion-mode-keymap)
    ("'" vim:rust-backward-up-indentation-or-sexp:interactive))

  (def-keys-for-map vim-normal-mode-local-keymap
    ("-"   hydra-rust-dash/body)
    ("g"   hydra-rust-vim-normal-g-ext/body))

  (def-keys-for-map vim-visual-mode-local-keymap
    ("-"   hydra-rust-visual-dash/body)
    ("g"   hydra-rust-vim-visual-g-ext/body))

  (def-keys-for-map vim-insert-mode-local-keymap
    ("," smart-operators-comma)
    ("+" rust-smart-operators-self-insert)
    ("-" rust-smart-operators-self-insert)
    ("*" rust-smart-operators-self-insert)
    ("/" rust-smart-operators-self-insert)
    ("%" rust-smart-operators-self-insert)
    ("^" rust-smart-operators-self-insert)
    ;; ! is too special
    ("!" rust-smart-operators-self-insert)
    ("&" rust-smart-operators-self-insert)
    ("|" rust-smart-operators-self-insert)
    ("<" rust-smart-operators-self-insert)
    (">" rust-smart-operators-self-insert)
    ("=" rust-smart-operators-self-insert))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("C-u"   rust-insert-unimplemented)
    ("C-t"   flycheck-enhancements-previous-error-with-wraparound)
    ("C-h"   flycheck-enhancements-next-error-with-wraparound)
    ("M-t"   rust-compilation-prev-error-other-window)
    ("M-h"   rust-compilation-next-error-other-window)
    ("C-SPC" company-complete))

  ;; (setup-eproj-symbnav)
  (setup-lsp-symbnav)

  (lsp)

  (when lsp-mode
    (def-keys-for-map vim-normal-mode-local-keymap
      ("C-r" lsp-rename))))


(provide 'rust-setup)

;; Local Variables:
;; End:

;; rust-setup.el ends here
