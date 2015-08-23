;; idris-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 18 July 2015
;; Description:

(require 'common)
(require 'agda-setup)

(autoload 'idris-mode "idris-mode" "" t)
(autoload 'eri-indent "eri" "" t)
(autoload 'eri-indent-reverse "eri" "" t)

(add-to-list ' auto-mode-alist '("\\.l?idr$" . idris-mode))

(modify-coding-system-alist 'file "\\.l?idr\\'" 'utf-8)

(vim:defcmd vim:idris-load-file (nonrepeatable)
  (idris-load-file))

(defun idris-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula t
               :use-fci t)
  (setq-local vim:shift-width 2)
  (setq-local standard-indent 2)
  (setq-local tab-always-indent t)
  (bind-tab-keys #'eri-indent
                 #'eri-indent-reverse
                 :enable-yasnippet t)
  (vim:local-emap "load" 'vim:idris-load-file)
  (vim:local-emap "lo"   'vim:idris-load-file)
  (def-keys-for-map vim:normal-mode-local-keymap
    ("C-t"             idris-previous-error)
    ("C-h"             idris-next-error)
    ("SPC SPC"         idris-pop-to-repl)
    ("g c d"           comment-util-delete-commented-part)
    ("+"               input-unicode)

    ("<f6>"            idris-load-file)
    ("- t"             idris-type-at-point)
    ("- d"             idris-docs-at-point)
    ("- / t"           idris-type-search)
    ("- c"             idris-case-split)
    ("- a"             idris-proof-search)
    ("- r"             idris-refine)
    ;; (define-key map (kbd "C-c C-m") 'idris-add-missing)
    ;; (define-key map (kbd "C-c C-e") 'idris-make-lemma)
    ;; (define-key map (kbd "C-c C-s") 'idris-add-clause)
    ;; (define-key map (kbd "C-c C-w") 'idris-make-with-block)
    ("j"               idris-normalize-term)
    ;; (define-key map (kbd "C-c C-m i") 'idris-show-term-implicits)
    ;; (define-key map (kbd "C-c C-m h") 'idris-hide-term-implicits)
    ;; (define-key map (kbd "C-c C-m c") 'idris-show-core-term)
    ;; (define-key map (kbd "C-c C-b C-p") 'idris-open-package-file)
    ;; (define-key map (kbd "C-c C-b p") 'idris-open-package-file)
    )
  (def-keys-for-map vim:visual-mode-local-keymap
    ("j"               idris-normalize-term)
    ("g a ="           agda-align-on-equals)
    ("g a - >"         agda-align-on-arrows)
    ("g a < -"         agda-align-on-left-arrows)
    ("g a |"           agda-align-on-pipes)
    ("g a ,"           agda-align-on-commas)
    ("g a - -"         agda-align-on-comments)
    ("g a :"           agda-align-on-colons)))

(add-hook 'idris-mode-hook #'idris-setup)

(defun idris-info-history-navigate-forward ()
  (interactive)
  (idris-info-history-forward)
  (idris-info-show))

(defun idris-info-history-navigate-backward ()
  (interactive)
  (idris-info-history-back)
  (idris-info-show))

(defun idris-info-setup ()
  (def-keys-for-map idis-info-mode-map
    ("<up>"   idris-info-history-navigate-backward)
    ("<down>" idris-info-history-navigate-forward)))

(add-hook 'idris-info-mode-hook #'idris-info-setup)

(defun idris-compiler-notes-setup ()
  (def-keys-for-map idris-compiler-notes-mode-map
    ("SPC" push-button)))

(add-hook 'idris-compiler-notes-mode-hook #'idris-compiler-notes-setup)

(provide 'idris-setup)

;; Local Variables:
;; End:

;; idris-setup.el ends here
