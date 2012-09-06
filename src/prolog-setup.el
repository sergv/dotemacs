;;; prolog-setup.el ---

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

(add-to-list 'auto-mode-alist '("\\.pro$" . prolog-mode))
;; (add-to-list 'auto-mode-alist '("\\.m$" . mercury-mode))


(defun prolog-setup ()
  (init-common :use-yasnippet nil :use-render-formula t)
  (autopair-mode)

  (setf ;; vim:insert-mode-local-keymap (make-keymap)
   ;; vim:visual-mode-local-keymap (make-keymap)
   vim:normal-mode-local-keymap (make-keymap))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("g <tab>" prolog-indent-predicate)
    (", e"     prolog-consult-predicate)

    ("SPC SPC" switch-to-prolog)

    ("<f1>"    prolog-consult-file)
    ("<f9>"    prolog-compile-file)))

(define-switch-to-interpreter
    switch-to-prolog
  ("*prolog*")
  (run-prolog nil)
  :doc "Pop to prolog interpreter."
  :save-buffer t
  :error-msg "Can't switch to prolog interpreter")


;;;

(defun prolog-inferior-setup ()
  (init-common :use-yasnippet nil :use-nxhtml-menu nil :use-comment nil)
  (init-repl)
  (autopair-mode)

  ;; changed in prolog.el itself
  ;; (modify-syntax-entry ?_ "_" prolog-mode-syntax-table)

  (setf vim:normal-mode-local-keymap (make-keymap)
        vim:insert-mode-local-keymap (make-keymap))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("SPC SPC"  comint-clear-prompt))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap
                     prolog-inferior-mode-map)
    ("C-w"      backward-delete-word)
    ("C-S-w"    backward-delete-word*)
    ("<tab>"    nil)
    ("C-SPC"    comint-clear-buffer-above-prompt)

    ("M-p"      browse-kill-ring)
    ("M-P"      browse-comint-input-history)
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

;; Local Variables:
;; lexical-binding: t
;; End:

;;; prolog-setup.el ends here
