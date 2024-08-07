;; c-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'common)
(require 'cc-setup)
(require 'c-abbrev+)

(puthash 'c-mode
         #'c-format-buffer
         *mode-indent-functions-table*)

(defun c-init ()
  (def-keys-for-map c-mode-map
    (("\(" "\)" "\[" "\]" "\{" "\}") nil)))

(eval-after-load 'cc-mode '(c-init))

;;;###autoload
(defun c-setup ()
  (cc-setup :define-special-keys t)
  (setup-folding 'enable-cpp '(:header-symbol "/" :length-min 3))
  (cc-setup/set-up-c-basic-offset)
  (setq-local company-backends
              '(company-clang
                company-files
                (company-eproj company-dabbrev-code company-keywords)
                company-dabbrev))

  (when-buffer-has-file
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

  (def-keys-for-map vim-visual-mode-local-keymap
    ("- m" c-macro-expand))
  (c-abbrev+-setup)
  (setup-eproj-symbnav))

;;;###autoload
(add-hook 'c-mode-hook #'c-setup)

(provide 'c-setup)

;; Local Variables:
;; End:

;; c-setup.el ends here
