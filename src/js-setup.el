;; js-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 25 April 2015
;; Description:

(require 'indentation)

;;;###autoload
(autoload 'js2-mode "js2-mode" nil t)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setf js2-highlight-level 3
      js2-basic-offset 2
      js2-bounce-indent-p t)

;;;###autoload
(defun js2-hide-indented-or-sexp ()
  (interactive)
  (if (haskell-outline-on-sexp?)
      (hs-hide-block)
    (js2-mode-hide-element)))

;;;###autoload
(defun js2-show-indented-or-sexp ()
  (interactive)
  (if (haskell-outline-on-sexp?)
      (hs-show-block)
    (js2-mode-show-element)))


;;;###autoload
(defun js2-setup ()
  (init-common :use-whitespace 'tabs-only)
  (setup-hs-minor-mode)
  (def-keys-for-map (vim:normal-mode-local-keymap)
    ("z c" js2-hide-indented-or-sexp)
    ("z o" js2-show-indented-or-sexp)
    ("z O" js2-mode-show-all)))

;;;###autoload
(add-hook 'js2-mode-hook #'js2-setup)

;;;###autoload
(defun js-setup ()
  (init-common :use-whitespace 'tabs-only
               :use-yasnippet t
               :use-comment t)
  (setup-hs-minor-mode))

;;;###autoload
(add-hook 'js-mode-hook #'js-setup)

(defun json-indent-buffer ()
  (interactive)
  (json-mode-pretty-print-dwim))

(puthash 'json-mode
         #'json-indent-buffer
         *mode-indent-functions-table*)

;;;###autoload
(defun json-setup ()
  (init-common :use-whitespace 'tabs-only
               :use-yasnippet t
               :use-comment t)
  (setup-hs-minor-mode)
  (def-keys-for-map vim:normal-mode-local-keymap
    ("- p" json-mode-show-path)))

;;;###autoload
(add-hook 'json-mode-hook #'json-setup)

(provide 'js-setup)

;; Local Variables:
;; End:

;; js-setup.el ends here
