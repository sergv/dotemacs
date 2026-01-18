;; haskell-hsc-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 May 2018
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'haskell-mode)
(require 'haskell-ts-mode)
(require 'haskell-font-lock)
(require 'haskell-utils)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hsc\\'" . haskell-hsc-mode))

(defconst haskell-hsc-mode-syntax-table haskell-ts-mode-syntax-table)

;;;###autoload
(define-derived-mode haskell-hsc-mode haskell-ts-base-mode "HSC[ts]"
  "Mode for editing *.hsc files of the hsc2hs Haskell tool."
  (let ((res (treesit-language-available-p 'hsc t)))
    (unless (car res)
      (error "HSC treesitter not available: %s" (cdr res))))

  (setq-local haskell-ts-buffer-lang 'hsc)

  ;; Font locking
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules (haskell-ts-lang-selection-resolve haskell-ts-font-lock-rules)))

  ;; Associate parser with current buffer.
  (treesit-parser-create 'hsc (current-buffer))

  (treesit-major-mode-setup))

(provide 'haskell-hsc-mode)

;; Local Variables:
;; End:

;; haskell-hsc-mode.el ends here
