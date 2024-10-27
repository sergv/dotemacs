;; js-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 25 April 2015
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'indentation)

;;;###autoload
(autoload 'js2-mode "js2-mode" nil t)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;;###autoload
(autoload 'json-mode-pretty-print-dwim "json-mode" nil t)

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

(defhydra-derive hydra-js-vim-normal-z hydra-vim-normal-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_c_: hide block
_o_: show block
_O_: show all blocks"
  ("c" js2-hide-indented-or-sexp)
  ("o" js2-show-indented-or-sexp)
  ("O" js2-mode-show-all))

;;;###autoload
(defun js2-setup ()
  (init-common :use-whitespace 'tabs-only)
  (setup-folding--impl t nil nil)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("z" hydra-js-vim-normal-z/body)))

;;;###autoload
(add-hook 'js2-mode-hook #'js2-setup)

;;;###autoload
(defun js-setup ()
  (init-common :use-whitespace 'tabs-only
               :use-yasnippet t
               :use-comment t)
  (setup-folding t nil))

;;;###autoload
(add-hook 'js-mode-hook #'js-setup)

(defun json-format-buffer (&optional alphabetical?)
  (interactive "P")
  (let ((json-encoding-default-indentation
         (make-string 2 ?\s)))
    (if (use-region-p)
        (with-region-bounds start end
          (funcall (if alphabetical?
                       #'json-pretty-print-ordered
                     #'json-pretty-print)
                   start
                   end))
      (funcall
       (if alphabetical?
           #'json-pretty-print-buffer-ordered
         #'json-pretty-print-buffer)))))

(dolist (mode '(json-mode json-ts-mode))
  (puthash mode
           #'json-format-buffer
           *mode-indent-functions-table*))

(defhydra-ext hydra-json (:exit t :foreign-keys warn :hint nil)
  "
show _p_ath to current element"
  ("p" json-mode-show-path))

(defun json-common-setup ()
  (init-common :use-whitespace 'tabs-and-trailing-only
               :use-yasnippet t
               :use-comment t)
  (setup-folding t nil)
  (def-keys-for-map vim-normal-mode-local-keymap
    ("-" hydra-json/body)))

;;;###autoload
(defun json-setup ()
  (json-common-setup)
  (json-error-mode))

;;;###autoload
(add-hook 'json-mode-hook #'json-setup)

;;;###autoload
(defun json-ts-setup ()
  (json-common-setup)
  ;; Don’t enable ‘json-error-mode’ - treesitter will detect errors for us.
  )

;;;###autoload
(add-hook 'json-ts-mode-hook #'json-ts-setup)

(provide 'js-setup)

;; Local Variables:
;; End:

;; js-setup.el ends here
