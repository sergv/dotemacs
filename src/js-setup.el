;; js-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 25 April 2015
;; Description:

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setf js2-highlight-level 3
      js2-basic-offset 2
      js2-bounce-indent-p t)

(defun js-setup ()
  (init-common :use-whitespace 'tabs-only)
  (def-keys-for-map (vim:normal-mode-local-keymap)
    ("z c" js2-mode-hide-element)
    ("z o" js2-mode-show-element)
    ("z O" js2-mode-show-all)))

(add-hook 'js2-mode-hook #'js-setup)

(provide 'js-setup)

;; Local Variables:
;; End:

;; js-setup.el ends here
