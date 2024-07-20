;; ghc-profiling-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 19 November 2015
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'current-column-fixed)
(require 'haskell-outline)
(require 'solarized)

(defface ghc-profiling-expensive-face
  `((t (:underline (:style wave :color ,+solarized-orange+))))
  "Face to highlight expensive entries with in the profile")

(defvar ghc-profiling-mode-expensive-cumulative-threshold 5
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
        (group-n 1 float) ;; inherited %time
        (+ ws)
        (group-n 2 float) ;; inherited %alloc
        (* ws)
        eol)))

(defun ghc-profiling-mode--matched-expensive-entry? ()
  (let ((inherited-time (string->number (match-string-no-properties 1)))
        (inherited-alloc (string->number (match-string-no-properties 2))))
    (<= ghc-profiling-mode-expensive-cumulative-threshold
        (max inherited-time inherited-alloc))))

(defvar ghc-profiling-mode-font-lock-keywords
  `((,ghc-profiling-mode-entry-re
     (0
      (when (ghc-profiling-mode--matched-expensive-entry?)
        'ghc-profiling-expensive-face)))))

(defun ghc-profiling-mode--generic-searh-for-expensive-entry (direction)
  (save-match-data
    (re-search-generic-matching
     direction
     #'ghc-profiling-mode--matched-expensive-entry?
     ghc-profiling-mode-entry-re
     nil
     t)))

(defmacro ghc-profiling-mode--search-for-expensive-entry (direction)
  `(let ((start-point (point))
         (column (current-column-fixed)))
     (fold-direction ,direction
       (end-of-line)
       (beginning-of-line))
     (if (wrap-search-around
             ,direction
           (ghc-profiling-mode--generic-searh-for-expensive-entry ',direction))
         (move-to-column column)
       (goto-char start-point))))

(defun ghc-profiling-mode-search-for-expensive-entry-forward ()
  (interactive)
  (ghc-profiling-mode--search-for-expensive-entry forward))

(defun ghc-profiling-mode-search-for-expensive-entry-backward ()
  (interactive)
  (ghc-profiling-mode--search-for-expensive-entry backward))

;;;###autoload
(define-derived-mode ghc-profiling-mode prog-mode "GHC-Prof"
  "Major mode for viewing Haskell profiling reports."
  (setq-local font-lock-defaults
              '(ghc-profiling-mode-font-lock-keywords)))

(defun ghc-profiling-mode-back-up-indent-level ()
  "Move back to the previous indentation level."
  (interactive)
  (indent-back-up-indent-level #'indent-on-blank-line?))

;;;###autoload
(defun ghc-profiling-mode-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-fci nil
               :use-hl-line t
               :smerge nil)
  ;; Display every line as a single screen line.
  (setq-local truncate-lines t)
  (haskell-setup-folding :enable-hideshow nil
                         :enable-cpp nil)
  (def-keys-for-map vim-normal-mode-local-keymap
    ("C-h"   ghc-profiling-mode-search-for-expensive-entry-forward)
    ("C-t"   ghc-profiling-mode-search-for-expensive-entry-backward)
    ("<tab>" yafolding-toggle-element)
    ("'"     ghc-profiling-mode-back-up-indent-level)))

(provide 'ghc-profiling-mode)

;; Local Variables:
;; End:

;; ghc-profiling-mode.el ends here
