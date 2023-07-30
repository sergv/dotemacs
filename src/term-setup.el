;; term-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 10 January 2012
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'common)
(require 'solarized)

(setf term-buffer-maximum-size 0 ;; don't truncate anything
      ;; set pompt both for lisps and
      term-prompt-regexp
      "^[^#$%>\n]*\\(?:[#$%]\\|\\(?:<[0-9]*\\|-\\)?>+:?\\|\\*\\) *"
      term-scroll-to-bottom-on-output nil
      term-input-ignoredups t
      term-input-ring-size 1024)

(vimmize-motion
 (term-bol nil)
 :doc "Move the cursor to the first character after prompt\
on current line. See `term-bol'.")

(defun term-setup ()
  (vim:bind-local-keymaps)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("^" vim:term-bol-motion:interactive)
    ;; ("<up>" term-previous-input)
    ;; ("<down>" term-next-input)
    )

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("M-x"  counsel-M-x)
    ("M-:"  eval-expression)
    ("<f6>" term-paste))

  (def-keys-for-map vim-insert-mode-local-keymap
    ("SPC" term-send-raw)))

(add-hook 'term-mode-hook #'term-setup)

(provide 'term-setup)

;; Local Variables:
;; End:

;; term-setup.el ends here
