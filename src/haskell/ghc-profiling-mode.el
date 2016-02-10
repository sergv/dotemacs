;; ghc-profiling-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 19 November 2015
;; Description:

(require 'solarized)

(defface ghc-profiling-expensive-face
  `((t (:underline (:style wave :color ,+solarized-orange+))))
  "Face to highlight expensive entries with in the profile")

(defparameter ghc-profiling-mode-expensive-cumulative-threshold 10
  "Highlight profile entries that take more percentage either in
cpu time or allocations that the value of this variable.")

(defparameter ghc-profiling-mode-font-lock-keywords
  `((,(rxx ((ws (any ?\s ?\t))
            (non-ws (not (any ?\s ?\t ?\n ?\r)))
            (integer (+ digit))
            (float (seq integer "." integer)))
        bol
        (* ws)
        (+ non-ws) ;; Function name
        (+ ws)
        (+ non-ws) ;; Module
        (+ ws)
        integer
        (+ ws)
        integer
        (+ ws)
        float
        (+ ws)
        float
        (+ ws)
        (group float)
        (+ ws)
        (group float)
        (* ws)
        eol)
     (0 (when (<= ghc-profiling-mode-expensive-cumulative-threshold
                  (max (string->number (match-string 1))
                       (string->number (match-string 2))))
          'ghc-profiling-expensive-face)))))

(define-derived-mode ghc-profiling-mode prog-mode "GHC-Prof"
  "Major mode for viewing Haskell profiling reports."
  (set (make-local-variable 'font-lock-defaults)
       '(ghc-profiling-mode-font-lock-keywords)))

(defun ghc-profiling-mode-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-fci nil
               :use-hl-line t)
  ;; Display every line as a single screen line.
  (setq-local truncate-lines t)
  (haskell-setup-folding :enable-hs-minor-mode nil))

(provide 'ghc-profiling-mode)

;; Local Variables:
;; End:

;; ghc-profiling-mode.el ends here
