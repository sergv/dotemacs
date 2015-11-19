;; ghc-profiling-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 19 November 2015
;; Description:

(define-derived-mode ghc-profiling-mode prog-mode "GHC-Prof")

(defun ghc-profiling-mode-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-fci nil
               :use-hl-line t)
  ;; Display every line as a single screen line.
  (setq-local truncate-lines t)
  (haskell-setup-folding))

(provide 'ghc-profiling-mode)

;; Local Variables:
;; End:

;; ghc-profiling-mode.el ends here
