;; prolog-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  4 December 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'browse-kill-ring-setup)

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)

(setf prolog-system 'swi)

(setf auto-mode-alist
      (cons '("\\.pl$" . prolog-mode)
            (cons '("\\.pro$" . prolog-mode)
                  (remove-if (lambda (x) (eq? 'perl-mode (cdr x)))
                             auto-mode-alist))))
;; (add-to-list 'auto-mode-alist '("\\.m$" . mercury-mode))


(defun prolog-setup ()
  (init-common :use-yasnippet nil :use-render-formula t)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("g <tab>" prolog-indent-predicate)
    ("- e"     prolog-consult-predicate)

    ("SPC SPC" switch-to-prolog)

    ("<f6>"    prolog-consult-file)
    ("`"       prolog-compile-file)))

(define-switch-to-interpreter
  switch-to-prolog
  ("*prolog*")
  (run-prolog nil)
  :doc "Pop to prolog interpreter."
  :save-buffer t
  :error-msg "Can't switch to prolog interpreter")


;;;

(defun prolog-inferior-setup ()
  (init-common :use-yasnippet nil :use-comment nil)
  (init-repl :create-keymaps nil)

  ;; changed in prolog.el itself
  ;; (modify-syntax-entry ?_ "_" prolog-mode-syntax-table)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC"  comint-clear-prompt)
    ("C-n"      comint-previous-prompt)
    ("C-t"      comint-next-prompt))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap
                     prolog-inferior-mode-map)
    ("C-w"      backward-delete-word)
    ("C-S-w"    backward-delete-word*)
    ("<tab>"    nil)
    ("C-SPC"    comint-clear-buffer-above-prompt)

    ("M-p"      browse-comint-input-history)
    ("<return>" comint-send-input)

    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)
    ("C-<up>"   comint-previous-prompt)
    ("C-<down>" comint-next-prompt)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt)))

(defun restart-prolog ()
  "Restart prolog process."
  (interactive)
  (run-prolog t))

(add-hook 'prolog-mode-hook #'prolog-setup)
(add-hook 'prolog-inferior-mode-hook #'prolog-inferior-setup)

(provide 'prolog-setup)

;; Local Variables:
;; End:

;; prolog-setup.el ends here
