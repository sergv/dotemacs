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

(require 'align)
(require 'eproj)
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
  (init-common :use-yasnippet t
               :use-nxhtml-menu nil
               :use-comment t
               :use-render-formula t)

  ;; ghci interaction uses comint - same as shell mode
  (turn-on-font-lock)
  (font-lock-add-keywords nil
                          '(("`[^`]+`" . font-lock-function-name-face)))

  ;; fix vim treatment of words for Haskell

  ;; note: do not include underscore into vim:word as this would cause
  ;; inefficiencies while navigating haskell identifiers
  (setq-local vim:word "[:word:]'")
  (modify-syntax-entry ?_ "_")
  (modify-syntax-entry ?\' "_")
  (modify-syntax-entry ?\@ "'")

  ;; (setq-local vim:word "[:word:]_'")

  ;; (modify-syntax-entry ?\` "\"")

  (setq-local indent-region-function #'ignore)
  (setq-local yas-indent-line 'fixed)
  (turn-on-haskell-indentation)
  (setf haskell-indentation-cycle-warn nil)
  ;; (turn-on-haskell-simple-indent)

  (turn-on-haskell-doc-mode)
  (setf haskell-doc-show-global-types t)

  ;; it's not always a good idea to wait
  (setf ;; inferior-haskell-wait-and-jump t
        inferior-haskell-module-alist-file (path-concat
                                            +prog-data-path+
                                            "inf-haskell-module-alist"))

  (setq-local yas-prompt-functions
              (list #'haskell-yas-completing-prompt))

  (if (platform-use? 'work)
    (progn
      (setq-local vim:shift-width 2)
      (setf haskell-indentation-left-offset       2
            haskell-indentation-layout-offset     2
            haskell-indentation-starter-offset    2
            haskell-indentation-ifte-offset       2
            haskell-indentation-where-pre-offset  2
            haskell-indentation-where-post-offset 2

            haskell-indentation-left-offset 2
            haskell-indent-offset 2)
      (font-lock-add-keywords nil
                              `((,(rx word-start (or "TRADETYPE:"
                                                     "TAG:"
                                                     "DESC:"))
                                 (0 'font-lock-preprocessor-face)))))
    (setf haskell-indentation-left-offset       4
          haskell-indentation-layout-offset     4
          haskell-indentation-starter-offset    4
          haskell-indentation-ifte-offset       4
          haskell-indentation-where-pre-offset  2
          haskell-indentation-where-post-offset 2

          haskell-indentation-left-offset 4
          haskell-indent-offset 4))

  (if-buffer-has-file ;; when visiting a file
    ;; don't ask - just compile
    (setq-local compilation-read-command nil)
    ;; don't ask - just save
    (setq-local compilation-ask-about-save nil)
    (setq-local compilation-auto-jump-to-first-error nil)
    ;; don't skip any messages
    (setq-local compilation-skip-threshold 0)

    (add-hook 'compilation-finish-functions #'haskell-jump-to-error))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("j"       inferior-haskell-send-decl)
    ("g c d"   comment-util-delete-commented-part)

    (", t"     haskell-type)
    (", i"     haskell-info)
    (", h"     haskell-haddock-identifier)
    (", m"     haskell-haddock-module)
    (", ."     haskell-find-definition)
    (", g"     haskell-hoogle-at-point)
    (", y"     haskell-hayoo-at-point)
    (", c"     ghc-core-create-core)

    ("="       input-unicode)
    ;; check these out
    ;; (", f"     find-tag)
    ;; (", a"     tags-apropos)
    ;; (", v"     visit-tags-table)
    ("SPC SPC" switch-to-haskell))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("<tab>"           (lambda () (interactive) (haskell-indentation-indent-line)))
    ("S-<tab>"         nil)
    ("<S-iso-lefttab>" nil)
    ("<return>"        haskell-sp-newline)
    ("<f6>"            inferior-haskell-load-file)
    ("<f9>"            haskell-compile)
    ("S-<f9>"          hs-lint))

  ;; (def-keys-for-map (vim:normal-mode-local-keymap
  ;;                    vim:insert-mode-local-keymap)
  ;;   ("<tab>"           haskell-simple-indent)
  ;;   ("S-<tab>"         haskell-simple-indent-backtab)
  ;;   ("<S-iso-lefttab>" haskell-simple-indent-backtab))


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
    ("C-="     input-unicode))

  (haskell-setup-folding)
  (haskell-abbrev+-setup)
  (setup-eproj-symbnav)
  (setup-outline-headers :header-symbol "-"
                         :length-min 3))

;;;; set up inferior-haskell-mode

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
    ("C-<down>" compilation-jump-to-next-error)

    (", ?"      haskell-help-for-symbol-at-point)
    (", t"      haskell-type)
    (", i"      haskell-info)
    (", h"      haskell-haddock-identifier)
    (", m"      haskell-haddock-module)
    (", ."      haskell-find-definition)
    ("M-."      haskell-find-definition)
    (", g"      haskell-hoogle-at-point)
    (", y"      haskell-hayoo-at-point))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap
                     inferior-haskell-mode-map)
    ("M-p"      browse-kill-ring)
    ("C-M-p"    browse-comint-input-history)
    ("<return>" inf-haskell-send-input-or-jump-to-error))

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

  (haskell-abbrev+-setup)
  (setup-eproj-symbnav))

(add-hook 'inferior-haskell-mode-hook #'inferior-haskell-mode-setup)


(provide 'haskell-setup)

;; Local Variables:
;; End:

;; haskell-setup.el ends here
