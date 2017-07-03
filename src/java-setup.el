;; java-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 17 January 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'cc-setup)
(require 'common)
(require 'java-abbrev+)

;;;###autoload
(defun java-setup ()
  (cc-setup :define-special-keys nil)
  (setf c-basic-offset 4)
  (setq-local vim:shift-width 4)
  (setq-local c-indentation-indent-style
              "java-standard"
              ;; "java-clojure"
              )
  ;; indent with tabs
  (setq-local whitespace-style '(face tabs space-after-tab space-before-tab))
  (setq-local indent-tabs-mode t)

  (setup-eproj-symbnav)
  (setup-outline-headers :header-start "/"
                         :header-symbol "*"
                         :length-min 3)
  (java-abbrev+-setup))

;;;###autoload
(add-hook 'java-mode-hook #'java-setup)

(provide 'java-setup)

;; Local Variables:
;; End:

;; java-setup.el ends here
