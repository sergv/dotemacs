;; c-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'common)
(require 'cc-setup)
(require 'c-abbrev+)

;;;###autoload
(defun c-setup ()
  (cc-setup :define-special-keys t)
  (setf tab-width 8)

  (cc-setup/set-up-c-basic-offset)

  (setf hs-forward-sexp-func #'c-hideshow-forward-sexp)

  (if-buffer-has-file
    (setq-local compile-command
                (let* ((fname  (file-name-nondirectory buffer-file-name))
                       (target (file-name-sans-extension fname)))
                  (setq compile-command
                        (join-lines (list "gcc"
                                          "-W"
                                          "-Wall"
                                          "-O2"
                                          "-I."
                                          "-o"
                                          target
                                          fname)
                                    " ")))))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("- m" c-macro-expand))
  (c-abbrev+-setup)
  (setup-eproj-symbnav)
  (setup-outline-headers :header-start "/"
                         :header-symbol "*"
                         :length-min 3))

;;;###autoload
(add-hook 'c-mode-hook #'c-setup)

(provide 'c-setup)

;; Local Variables:
;; End:

;; c-setup.el ends here
