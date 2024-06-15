;; markdown-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 26 November 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

;;;###autoload
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mk?d\\'" . markdown-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(setf markdown-enable-math t
      markdown-list-indent-width 2
      markdown-asymmetric-header nil
      markdown-indent-on-enter nil
      markdown-fontify-code-blocks-natively t)

(defun markdown--yasnippet-indent-fallback ()
  (interactive)
  (let ((this-command 'markdown-cycle))
    (call-interactively #'markdown-cycle)))

;;;###autoload
(defun markdown-setup ()
  (init-common :use-yasnippet t
               :use-whitespace 'tabs-only)
  ;; (setq-local yas-indent-line 'fixed)
  (bind-tab-keys #'markdown-cycle
                 #'markdown-shifttab
                 :enable-yasnippet t
                 :yasnippet-fallback #'markdown--yasnippet-indent-fallback)
  (typography-setup)
  (flyspell-english)
  (setup-indent-size 4)
  (def-keys-for-map vim-normal-mode-local-keymap
    ("<f6>"       markdown-toggle-inline-images)
    ("C-h"        markdown-forward-paragraph)
    ("C-t"        markdown-backward-paragraph)
    ("M-h"        markdown-next-visible-heading)
    ("M-t"        markdown-previous-visible-heading)
    ("'"          markdown-up-heading)
    ("C-<down>"   flyspell-goto-next-error)
    ("C-<return>" ispell-word))
  (def-keys-for-map vim-visual-mode-local-keymap
    ("`"   vim-wrap-backticks)
    (("<tab>" "TAB") markdown-indent-region)))

;;;###autoload
(add-hook 'markdown-mode-hook #'markdown-setup)

(provide 'markdown-setup)

;; Local Variables:
;; End:

;; markdown-setup.el ends here
