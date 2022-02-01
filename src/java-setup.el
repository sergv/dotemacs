;; java-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 17 January 2013
;; Description:

(eval-when-compile
  (require 'cl))

(require 'cc-setup)
(require 'common)
(require 'java-abbrev+)

(puthash 'java-mode
         #'c-format-buffer
         *mode-indent-functions-table*)

;;;###autoload
(defun java-setup ()
  (cc-setup :define-special-keys nil)
  (setup-folding t '(:header-symbol "/" :length-min 3))
  (setq-local c-basic-offset 4
              vim-shift-width 4
              c-indentation-indent-style "java-standard"
              ;; "java-clojure"
              whitespace-style '(face tabs space-after-tab space-before-tab))
  (let ((proj (eproj-get-project-for-buf-lax (current-buffer))))
    ;; indent with tabs?
    (setq-local indent-tabs-mode (eproj-query/java/indent-tab proj t)))
  (setup-eproj-symbnav)
  (java-abbrev+-setup))

;;;###autoload
(add-hook 'java-mode-hook #'java-setup)

(provide 'java-setup)

;; Local Variables:
;; End:

;; java-setup.el ends here
