;; rust-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 22 March 2018
;; Description:

(require 'common)
(require 'flycheck)
(require 'haskell-compile)
(require 'indentation)
(require 'pretty-ligatures)
(require 'rust-compilation-commands)
(require 'smartparens-rust)
(require 'smartparens-setup)

(setf rust-indent-method-chain t
      rust-indent-where-clause t

      rust-playpen-url-format nil
      rust-shortener-url-format nil

      flycheck-cargo-check-args
      (list (format "--target-dir=%s"
                    (fold-platform-os-type "/tmp/target" "target"))))

(puthash 'rust-mode
         #'rust-format-buffer
         *mode-indent-functions-table*)

;;;; Utilities

(defconst rust-compilation-buffer-name "*rust-compilation*")

(defun rust-get-compilation-buffer-name (&rest args)
  rust-compilation-buffer-name)

(defun rust-compilation-next-error-other-window ()
  "Select next error in `rust-compilation-buffer-name' buffer and jump to
it's position in current window."
  (interactive)
  (aif (get-buffer rust-compilation-buffer-name)
      (compilation-navigation-next-error-in-buffer-other-window it)
    (error "No Rust compilation started")))

(defun rust-compilation-prev-error-other-window ()
  "Select previous error in `rust-compilation-buffer-name' buffer and jump to
it's position in current window."
  (interactive)
  (aif (get-buffer rust-compilation-buffer-name)
      (compilation-navigation-prev-error-in-buffer-other-window it)
    (error "No Rust compilation started")))

(defvar rust-compilation-extra-error-modes haskell-compilation-extra-error-modes
  "Extra modes from `compilation-error-regexp-alist-alist' whose
warnings will be colorized in `rust-compilation-mode'.")

(defun rust-compilation-filter-hook ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(make-align-function rust-align-on-equals
                     "\\([+*|&/!%]\\|-\\|\\^\\)?=[^=]"
                     :require-one-or-more-spaces t)

(vim:defcmd vim:rust-flycheck-configure (nonrepeatable)
  (flycheck-rust-setup))

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;;;; rust-compilation-mode

(define-compilation-mode rust-compilation-mode "Rust Compilation"
  "Rust-specific `compilation-mode' derivative."
  (setq-local compilation-error-regexp-alist
              (append (list rustc-compilation-regexps
                            rustc-colon-compilation-regexps
                            cargo-compilation-regexps)
                      rust-compilation-extra-error-modes))
  (setq-local *compilation-jump-error-regexp*
              (mapconcat (lambda (x) (concat "\\(?:" (car x) "\\)"))
                         (-filter #'listp compilation-error-regexp-alist)
                         "\\|"))

  (setq-local compilation-environment '("TERM=xterm-256color"))

  (vim:local-emap "c" 'vim:recompile)

  (add-hook 'compilation-filter-hook #'rust-compilation-filter-hook nil t))

(def-keys-for-map rust-compilation-mode-map
  +vim-special-keys+
  ("<return>" compilation/goto-error)
  ("SPC"      compilation/goto-error-other-window))

(defhydra-ext hydra-rust-dash (:exit t :foreign-keys nil :hint nil)
  "
_e_xplain error at pointn
"
  ("e" flycheck-explain-error-at-point))

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

(defun vim:rust-beginning-of-defun (&optional arg)
  (interactive "p")
  (vim:save-position)
  (rust-beginning-of-defun))

(defun vim:rust-end-of-defun ()
  (interactive)
  (vim:save-position)
  (rust-end-of-defun))

(defhydra-derive hydra-rust-vim-visual-g-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign  _t_: beginning of defun
         _h_: end of defun"
  ("a" hydra-rust-align/body)

  ("t" vim:rust-beginning-of-defun)
  ("h" vim:rust-end-of-defun))

;;;; Setup

;;;###autoload
(defun rust-setup ()
  (init-common :use-render-formula nil
               :sp-slurp-sexp-insert-space nil
               :use-yasnippet t
               :use-whitespace 'tabs-only
               :hl-parens-backend 'smartparens)
  (fontify-conflict-markers!)
  (setup-folding t '(:header-symbol "/" :length-min 3))
  (company-mode +1)
  (setq-local company-backends
              '(company-files
                (company-eproj company-dabbrev-code company-keywords)
                company-dabbrev))
  ;; Don't skip any messages.
  (setq-local compilation-skip-threshold 0)
  (setq-local compilation-buffer-name-function #'rust-get-compilation-buffer-name)

  (pretty-ligatures--install
   (append pretty-ligatures-c-like-symbols
           pretty-ligatures-python-like-words))
  (rust-compilation-commands-install!)

  (setf vim:shift-width rust-indent-offset
        tab-width rust-indent-offset)

  (let (
        ;; NB may be nil.
        (proj (eproj-get-project-for-buf-lax (current-buffer))))

    (dolist (entry (eproj-query/local-variables proj major-mode nil))
      (set (make-local-variable (car entry)) (cadr entry)))

    (when (not noninteractive)
      (let* ((flycheck-backend
              (eproj-query/flycheck-checker
               proj
               major-mode
               'rust-clippy)))
        (setq-local flycheck-disabled-checkers
                    (eproj-query/flycheck-disabled-checkers
                     proj
                     major-mode
                     flycheck-disabled-checkers))
        (if flycheck-backend
            (progn

              (unless (flycheck-may-use-checker flycheck-backend)
                (flycheck-verify-checker flycheck-backend)
                (error "Unable to select checker '%s' for buffer '%s'"
                       flycheck-backend (current-buffer)))
              (setq-local flycheck-checker flycheck-backend)
              (flycheck-mode +1))
          ;; Disable flycheck if it was explicitly set to nil
          (progn
            (when flycheck-mode
              (flycheck-mode -1)))))))

  (setq-local whitespace-line-column 80)
  (setq-local whitespace-style '(face tabs lines-tail))

  (setq-local mode-line-format
              (apply #'default-mode-line-format
                     (when flycheck-mode
                       (list
                        " "
                        '(:eval (flycheck-pretty-mode-line))))))

  (dolist (cmd '("conf" "configure"))
    (vim:local-emap cmd #'vim:rust-flycheck-configure))

  (flycheck-install-ex-commands!
   :install-flycheck flycheck-mode)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("-" hydra-rust-dash/body)
    ("g" hydra-rust-vim-normal-g-ext/body))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("g" hydra-rust-vim-visual-g-ext/body))

  (def-keys-for-map vim:insert-mode-local-keymap
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

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("C-t"   flycheck-enhancements-previous-error-with-wraparound)
    ("C-h"   flycheck-enhancements-next-error-with-wraparound)
    ("M-t"   rust-compilation-prev-error-other-window)
    ("M-h"   rust-compilation-next-error-other-window)
    ("C-SPC" company-complete))

  (setup-eproj-symbnav))


(provide 'rust-setup)

;; Local Variables:
;; End:

;; rust-setup.el ends here
