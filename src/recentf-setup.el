;; recentf-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 17 April 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'haskell-autoload)

(require 'recentf)

(setf recentf-max-saved-items 1024
      recentf-save-file (concat +prog-data-path+ "/recentf")
      recentf-exclude
      (list
       (eval-when-compile
         (concat "\\`.*"
                 (regexp-opt +ignored-file-extensions+)
                 "\\'")))
      recentf-arrange-rules
      (list
       (list "Rust files (%d)" (rx (or ".rs" "Cargo.toml") eos))
       (list "Haskell files (%d)" (eval-when-compile
                                    (concat ".\\."
                                            (regexp-opt (cons "cabal" +haskell-extensions+))
                                            "\\'")))
       '("Latex files (%d)" ".\\.tex\\'")
       '("Elisp files (%d)" ".\\.el\\'")
       '("C/C++ files (%d)" (eval-when-compile
                                    (concat ".\\."
                                            (regexp-opt +cpp-extensions+)
                                            "\\'")))
       '("Java files (%d)"  ".\\.java\\'")))

(recentf-mode +1)

(provide 'recentf-setup)

;; Local Variables:
;; End:

;; recentf-setup.el ends here
