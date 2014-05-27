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
;; (require 'hi2)
(require 'shm)

(require 'haskell-abbrev+)
(require 'haskell-misc)
(require 'haskell-outline)
(require 'haskell-shm)

(setf shm-insert-space-after-comma t
      shm-indent-point-after-adding-where-clause t
      shm-colon-enabled t
      shm-indent-use-chris-done-if-indent-style nil
      inferior-haskell-find-project-root nil
      ghc-core-program-args '("-O2"
                              "-dsuppress-uniques"
                              "-dsuppress-idinfo"
                              "-dsuppress-module-prefixes"
                              ;; "-dsuppress-type-signatures"
                              "-dsuppress-type-applications"
                              "-dsuppress-coercions")
      )

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
  (modify-syntax-entry ?\' "_")
  (modify-syntax-entry ?\@ "'")

  ;; (setq-local vim:word "[:word:]_'")

  ;; (modify-syntax-entry ?\` "\"")

  (setq-local eproj-symbnav/identifier-type 'haskell-symbol)
  (setq-local indent-region-function #'ignore)
  (setq-local yas-indent-line 'fixed)

  (structured-haskell-mode +1)
  (setq-local indent-line-function #'ignore)
  (setq-local abbrev+-fallback-function #'haskell-abbrev+-fallback-space)

  ;; (turn-on-hi2)
  ;; (setf hi2-show-indentations nil
  ;;       hi2-show-indentations-after-eol nil
  ;;       hi2-dyn-show-indentations nil)
  ;;
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

  (let ((offset 2))
    (setq-local vim:shift-width       offset)
    (setq-local hi2-left-offset       offset)
    (setq-local hi2-layout-offset     offset)
    (setq-local hi2-starter-offset    offset)
    (setq-local hi2-ifte-offset       offset)
    (setq-local hi2-where-pre-offset  2)
    (setq-local hi2-where-post-offset 2)

    (setq-local hi2-left-offset       offset)
    (setq-local haskell-indent-offset offset)
    (setq-local haskell-indent-spaces offset)
    (setq-local shm-indent-spaces     offset))
  (when (platform-use? 'work)
    (font-lock-add-keywords nil
                            `((,(rx word-start (or "TRADETYPE:"
                                                   "TAG:"
                                                   "DESC:"))
                               (0 'font-lock-preprocessor-face)))))

  (if-buffer-has-file ;; when visiting a file
    ;; don't ask - just compile
    (setq-local compilation-read-command nil)
    ;; don't ask - just save
    (setq-local compilation-ask-about-save nil)
    (setq-local compilation-auto-jump-to-first-error nil)
    ;; don't skip any messages
    (setq-local compilation-skip-threshold 0))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("j"       inferior-haskell-send-decl)
    ("g c d"   comment-util-delete-commented-part)
    ("g c c"   haskell-comment-node)
    (", c"     ghc-core-create-core)
    ("="       input-unicode)
    ("SPC SPC" switch-to-haskell)
    ("g g ("   shm/wrap-parens)
    ("g w"     shm/goto-where))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("-"       shm/hyphen)
    ("#"       shm/hash)
    (","       shm/comma)
    (":"       shm/:)
    ("="       shm/=)

    ("C-="     input-unicode)

    ("+"       shm/+)
    ("*"       shm/*)
    ("="       shm/=)
    ("<"       shm/<)
    (">"       shm/>)
    ("!"       shm/!)
    ("@"       shm/@)
    ("$"       shm/$)
    ("%"       shm/%)
    ("^"       shm/^)
    ("&"       shm/&)

    ("/"       shm//)
    ("?"       shm/?)
    ("|"       shm/|)
    ("\\"      shm/\\)
    ("~"       shm/~))

  (def-keys-for-map (vim:visual-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("("       sp--self-insert-command)
    ("["       sp--self-insert-command)
    ("{"       sp--self-insert-command))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("C-w"             shm/backward-kill-word)
    ("M-u"             shm/insert-undefined)
    ("M-<up>"          shm/swing-up)
    ("M-<down>"        shm/swing-down)
    ("<tab>"           shm/tab)
    ("<backtab>"       shm/backtab)

    ("S-<tab>"         nil)
    ("<S-iso-lefttab>" nil)
    ("<return>"        haskell-newline)
    ("<f6>"            inferior-haskell-load-file)
    ("<f9>"            haskell-compile)
    ("S-<f9>"          hs-lint))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap)
    (", ?"     haskell-help-for-symbol-at-point)
    (", t"     haskell-type)
    (", i"     haskell-info)
    (", h"     haskell-haddock-identifier)
    (", m"     haskell-haddock-module)
    (", g"     haskell-hoogle-at-point)
    (", y"     haskell-hayoo-at-point)

    ("*"       search-for-haskell-symbol-at-point-forward)
    ("#"       search-for-haskell-symbol-at-point-backward)
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
    ("g a"     nil)
    ("g a ="   haskell-align-on-equals)
    ("g a - >" haskell-align-on-arrows)
    ("g a < -" haskell-align-on-left-arrows)
    ("g a |"   haskell-align-on-guards)
    ("g a ,"   haskell-align-on-commas)
    ("g a - -" haskell-align-on-comments)
    ("g a : :" haskell-align-on-double-colons))

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
    ("("          nil)
    ("["          nil)
    ("{"          nil)
    (")"          nil)
    ("]"          nil)
    ("}"          nil)
    ("C-w"        nil)
    ("M-w"        nil)
    ("C-y"        nil)
    ("M-y"        nil)
    ("M-a"        nil)
    ("M-k"        nil)
    ("C-k"        nil)
    ("C-<return>" shm/simple-indent-newline-same-col))

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
    ("C-<down>" compilation-jump-to-next-error)

    (", ."      haskell-find-definition)
    ("M-."      haskell-find-definition))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap
                     inferior-haskell-mode-map)
    ("M-p"      browse-kill-ring)
    ("C-M-p"    browse-comint-input-history)
    ("<return>" inf-haskell-send-input-or-jump-to-error))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap)
    (", ?"     haskell-help-for-symbol-at-point)
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

  (haskell-abbrev+-setup)
  (setup-eproj-symbnav))

(add-hook 'inferior-haskell-mode-hook #'inferior-haskell-mode-setup)


(defun haskell-compilation-setup ()
  (setq-local *compilation-jump-error-regexp*
              +haskell-compile-error-or-warning-regexp+)
  (def-keys-for-map haskell-compilation-mode-map
    ("<return>" compilation/goto-error)
    ("SPC"      compilation/goto-error-other-window)))

(add-hook 'haskell-compilation-mode-hook #'haskell-compilation-setup)

(defun haskell-cabal-setup ()
  (init-common :use-comment t :use-yasnippet nil)
  (setq-local vim:shift-width 2))

(add-hook 'haskell-cabal-mode-hook #'haskell-cabal-setup)

(provide 'haskell-setup)

;; Local Variables:
;; End:

;; haskell-setup.el ends here
