;; kotlin-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 28 July 2024
;; Description:

;;;###autoload
(defun kotlin-setup ()
  (init-common :use-render-formula t
               :use-yasnippet t
               :use-whitespace 'tabs-only)
  (which-function-mode -1)
  (company-mode +1)

  (setq-local company-backends '(company-eproj)
              whitespace-line-column 80
              whitespace-style '(face tabs space-after-tab space-before-tab)
              hs-allow-nesting t)

  (setup-folding t '(:header-symbol "/" :length-min 3))

  (let ((proj (eproj-get-project-for-buf-lax (current-buffer))))
    ;; Override indenting with tabs since selecting style may have
    ;; changed it.
    (setq-local indent-tabs-mode (eproj-query/java/indent-tab proj t))
    (when indent-tabs-mode
      ;; Make single indent offset occupy single tab character,
      ;; otherwise mismatch between the two will make indentation mix tabs with spaces.
      (setf c-basic-offset tab-width
            vim-shift-width tab-width)))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("C-SPC"       company-complete)
    ("<backspace>" backward-delete-char))

  (setup-eproj-symbnav))

;;;###autoload
(add-hook 'kotlin-ts-mode-hook #'kotlin-setup)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode))

(provide 'kotlin-setup)

;; Local Variables:
;; End:

;; kotlin-setup.el ends here
