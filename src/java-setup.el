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
    (awhen (eproj-query/java/indent-style proj nil)
      (message "Switching to ‘%s’ indentation style" it)
      (c-set-style it))

    ;; Override indenting with tabs since selecting style may have
    ;; changed it.
    (setq-local indent-tabs-mode (eproj-query/java/indent-tab proj t))
    (when indent-tabs-mode
      ;; Make single indent offset occupy single tab character,
      ;; otherwise mismatch between the two will make indentation mix tabs with spaces.
      (setf c-basic-offset tab-width
            vim-shift-width tab-width)))
  (setup-eproj-symbnav)
  (java-abbrev+-setup))

;;;###autoload
(add-hook 'java-mode-hook #'java-setup)

(provide 'java-setup)

;; Local Variables:
;; End:

;; java-setup.el ends here
