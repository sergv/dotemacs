;; ptx-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 23 August 2018
;; Description:

(require 'macro-util)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ptx\\'" . ptx-mode))

;;;###autoload
(add-hook 'ptx-mode-hook #'ptx-mode-setup)

;;;###autoload
(defun ptx-mode-setup ()
  (init-common
   :use-yasnippet nil
   :use-comment t
   :use-fci t))

;;;###autoload
(defun ptx-file-magic-function ()
  (when-buffer-has-file
    (save-match-data
      (--every-p
       (save-excursion (re-search-forward it nil t))
       `(,(rx bol ".version" symbol-end)
         ,(rx bol ".target" symbol-end)
         ,(rx bol ".address_size" symbol-end)
         ,(rx ".reg" symbol-end)
         ,(rx "."
              (or (seq (any ?b ?f ?u ?s)
                       (or "8" "16" "32" "64"))
                  "pred")
              eow))))))

;;;###autoload
(add-to-list 'magic-mode-alist (cons #'ptx-file-magic-function #'ptx-mode))

(provide 'ptx-mode-setup)

;; Local Variables:
;; End:

;; ptx-mode-setup.el ends here
