;; scala-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 12 June 2026
;; Description:

(eval-when-compile
  (require 'macro-util))

;;;###autoload
(defun scala-setup ()
  (init-common :use-render-formula t
               :use-yasnippet t
               :use-whitespace 'tabs-only)
  ;; (which-function-mode -1)
  (company-mode +1)

  (setq-local company-backends '(company-eproj)
              whitespace-line-column 80
              whitespace-style '(face tabs space-after-tab space-before-tab)
              hs-allow-nesting t)

  (setup-folding t '(:header-symbol "/" :length-min 3))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("C-SPC"       company-complete)
    ;; ("<backspace>" backward-delete-char)
    )

  (setup-eproj-symbnav))

;;;###autoload
(add-hook 'scala-ts-mode-hook #'scala-setup)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

(provide 'scala-setup)

;; Local Variables:
;; End:

;; scala-setup.el ends here
