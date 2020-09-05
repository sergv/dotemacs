;; ghc-profiling-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 19 November 2015
;; Description:

(require 'haskell-outline)
(require 'solarized)

(defface ghc-profiling-expensive-face
  `((t (:underline (:style wave :color ,+solarized-orange+))))
  "Face to highlight expensive entries with in the profile")

(defparameter ghc-profiling-mode-expensive-cumulative-threshold 8
  "Highlight profile entries that take more percentage either in
cpu time or allocations that the value of this variable.")

(defconst ghc-profiling-mode-entry-re
  (rx-let ((ws (any ?\s ?\t))
           (non-ws (not (any ?\s ?\t ?\n ?\r)))
           (integer (+ digit))
           (float (seq integer "." integer)))
    (rx bol
        (* ws)
        (+ non-ws) ;; Function name
        (+ ws)
        (+ non-ws) ;; Module
        (+ ws)
        (\?
         (+ non-ws) ;; SRC, from 8.0 onwards
         (+ ws))
        integer ;; no.
        (+ ws)
        integer ;; entries
        (+ ws)
        float ;; individual %time
        (+ ws)
        float ;; individual %alloc
        (+ ws)
        (group float) ;; inherited %time
        (+ ws)
        (group float) ;; inherited %alloc
        (* ws)
        eol)))

(defun ghc-profiling-mode--matched-expersive-entry? ()
  (let ((inherited-time (string->number (match-string 1)))
        (inherited-alloc (string->number (match-string 2))))
    (<= ghc-profiling-mode-expensive-cumulative-threshold
        (max inherited-time inherited-alloc))))

(defparameter ghc-profiling-mode-font-lock-keywords
  `((,ghc-profiling-mode-entry-re
     (0
      (when (ghc-profiling-mode--matched-expersive-entry?)
        'ghc-profiling-expensive-face)))))

(defun ghc-profiling-mode--generic-searh-for-expensive-entry (direction)
  (save-match-data
    (re-search-generic-matching
     direction
     #'ghc-profiling-mode--matched-expersive-entry?
     ghc-profiling-mode-entry-re
     nil
     t)))

(defun ghc-profiling-mode--search-for-expensive-entry (direction)
  (let ((start-point (point))
        (column (current-column)))
    (fold-direction direction
      (end-of-line)
      (beginning-of-line))
    (if (wrap-search-around
         direction
         (lambda () (ghc-profiling-mode--generic-searh-for-expensive-entry direction)))
        (move-to-column column)
      (goto-char start-point))))

(defun ghc-profiling-mode--search-for-expensive-entry-forward ()
  (interactive)
  (ghc-profiling-mode--search-for-expensive-entry 'forward))

(defun ghc-profiling-mode--search-for-expensive-entry-backward ()
  (interactive)
  (ghc-profiling-mode--search-for-expensive-entry 'backward))

;;;###autoload
(define-derived-mode ghc-profiling-mode prog-mode "GHC-Prof"
  "Major mode for viewing Haskell profiling reports."
  (set (make-local-variable 'font-lock-defaults)
       '(ghc-profiling-mode-font-lock-keywords)))

;;;###autoload
(defun ghc-profiling-mode-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-fci nil
               :use-hl-line t)
  ;; Display every line as a single screen line.
  (setq-local truncate-lines t)
  (haskell-setup-folding :enable-hs-minor-mode nil)
  (def-keys-for-map vim:normal-mode-local-keymap
    ("C-h" ghc-profiling-mode--search-for-expensive-entry-forward)
    ("C-t" ghc-profiling-mode--search-for-expensive-entry-backward)))

(provide 'ghc-profiling-mode)

;; Local Variables:
;; End:

;; ghc-profiling-mode.el ends here
