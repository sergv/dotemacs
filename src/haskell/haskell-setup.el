;; haskell-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


(require 'custom)
(require 'common)
(require 'custom-predicates)
(require 'browse-kill-ring-setup)

;; (require 'paredit)
(require 'align)
(require 'shell-setup)

(require 'haskell-checkers)

(require 'haskell-abbrev+)
(require 'haskell-misc)
(require 'haskell-outline)

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

(defun haskell-setup ()
  ;; ghci interaction uses comint - same as shell mode
  (turn-on-font-lock)
  (font-lock-add-keywords nil
                          '(("`[^`]+`" . font-lock-function-name-face)))
  ;;
  ;; fix vim treatment of words for Haskell

  ;; note: do not include underscore into vim:word as this would cause
  ;; inefficiencies while navigating haskell identifiers
  (setq-local vim:word "[:word:]'")
  (modify-syntax-entry ?_ "_")

  ;; (setq-local vim:word "[:word:]_'")

  (modify-syntax-entry ?\` "\"")

  (init-common :use-yasnippet t
               :use-nxhtml-menu nil
               :use-comment t
               :use-render-formula t)
  (setq-local yas-indent-line 'fixed)
  (autopair-mode t)
  ;; (turn-on-haskell-indentation)
  ;; (setf haskell-indentation-cycle-warn nil)
  (turn-on-haskell-simple-indent)

  (turn-on-haskell-doc-mode)
  (setf haskell-doc-show-global-types t)

  (setf inferior-haskell-wait-and-jump t
        inferior-haskell-module-alist-file (path-concat
                                            +prog-data-path+
                                            "inf-haskell-module-alist")

        haskell-indentation-left-offset       4
        haskell-indentation-layout-offset     4
        haskell-indentation-starter-offset    4
        haskell-indentation-ifte-offset       4
        haskell-indentation-where-pre-offset  2
        haskell-indentation-where-post-offset 2)

  (if-buffer-has-file ;; when visiting a file
    (when (or (file-exist? "./makefile")
              (file-exist? "./Makefile")
              (file-exist? "./MAKEFILE")
              (file-exist? "./GNUMakefile"))
      (setf haskell-has-makefile? t))

    ;; don't ask - just compile
    (setq-local compilation-read-command nil)
    ;; don't ask - just save
    (setq-local compilation-ask-about-save nil)
    (setq-local compilation-auto-jump-to-first-error nil)
    ;; don't skip any messages
    (setq-local compilation-skip-threshold 0)

    ;; (add-hook 'compilation-finish-functions
    ;;           #'haskell-reload-on-successful-compilation)
    (add-hook 'compilation-finish-functions #'haskell-jump-to-error))

  (setf vim:normal-mode-local-keymap (make-keymap)
        vim:visual-mode-local-keymap (make-sparse-keymap)
        vim:insert-mode-local-keymap (make-sparse-keymap))

  (def-keys-for-map vim:normal-mode-local-keymap
    (", t"     haskell-type)
    (", i"     haskell-info)
    (", h"     haskell-haddock-identifier)
    (", m"     haskell-haddock-module)
    (", ."     haskell-find-definition)
    ("M-."     haskell-find-definition)
    (", g"     haskell-hoogle-at-point)
    (", y"     haskell-hayoo-at-point)
    (", c"     ghc-core-create-core)

    ("="        input-unicode)
    ;; check these out
    ;; (", f"     find-tag)
    ;; (", a"     tags-apropos)
    ;; (", v"     visit-tags-table)

    ("<f6>"    inferior-haskell-load-file)
    ("SPC SPC" switch-to-haskell)
    ("<f9>"    haskell-compile)
    ("S-<f9>"  hs-lint)

    ;; ("C-<right>" paredit-forward-slurp-sexp)
    ;; ("C-<left>"  paredit-forward-barf-sexp)
    ;; ("M-<up>"    paredit-splice-sexp-killing-backward)
    )

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("<tab>"   haskell-simple-indent)
    ("S-<tab>" haskell-simple-indent-backtab)
    ("<S-iso-lefttab>" haskell-simple-indent-backtab))


  (def-keys-for-map vim:visual-mode-local-keymap
    ("g a"     nil)
    ("g a ="   haskell-align-on-equals)
    ("g a - >" haskell-align-on-arrows)
    ("g a < -" haskell-align-on-left-arrows)
    ("g a |"   haskell-align-on-guards)
    ("g a ,"   haskell-align-on-commas)
    ("g a - -" haskell-align-on-comments)
    ("g a : :" haskell-align-on-double-colons))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("C-="       input-unicode)
    ;; ("C-<right>" paredit-forward-slurp-sexp)
    ;; ("C-<left>"  paredit-forward-barf-sexp)
    ;; ("M-<up>"    paredit-splice-sexp-killing-backward)
    )

  (haskell-setup-folding)
  (haskell-abbrev+-setup)

  ;; (enable-paredit-mode)
  ;; (setf paredit-mode-map nil)

  ;; declaration scanning
  (imenu-add-menubar-index)

  (setup-outline-headers :header-symbol "-"
                         :length-min 4
                         :length-max 9))

;;;; set up inferior-haskell-mode

(defun inferior-haskell-mode-setup ()
  ;; undo-tree is useless for ghci interaction
  ;; well I'm not sure now, I hope it's useful since it's proved itself useful
  ;; for other repls
  ;; (undo-tree-mode -1)
  (init-repl)
  (init-common :use-comment nil :use-yasnippet nil)
  (autopair-mode t)

  (setf vim:normal-mode-local-keymap (make-keymap)
        vim:insert-mode-local-keymap (make-sparse-keymap))

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
    ("C-<down>" compilation-jump-to-next-error)

    (", t"      haskell-type)
    (", i"      haskell-info)
    (", h"      haskell-haddock-identifier)
    (", m"      haskell-haddock-module)
    (", ."      haskell-find-definition)
    ("M-."     haskell-find-definition)
    (", g"      haskell-hoogle-at-point)
    (", y"      haskell-hayoo-at-point))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap
                     inferior-haskell-mode-map)
    ("M-p"   browse-kill-ring)
    ("C-M-p" browse-comint-input-history))

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
    ("S-<down>" compilation-jump-to-next-error)))

(add-hook 'inferior-haskell-mode-hook #'inferior-haskell-mode-setup)


(provide 'haskell-setup)

;; Local Variables:
;; End:

;; haskell-setup.el ends here
