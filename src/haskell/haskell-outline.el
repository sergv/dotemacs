;;;; outline for folding

(require 'haskell-misc)

(defconst +haskell-special-line-regexp+
  (mapconcat (lambda (x)
               (concat "\\(:?"
                       (if (char= (aref x 0) (string->char "^"))
                         (substring x 1)
                         x)
                       "\\)"))
             (list haskell-type-signature-regexp
                   haskell-main-function-regexp
                   haskell-toplevel-data-declaration-regexp
                   haskell-toplevel-class-declaration-regexp
                   haskell-toplevel-instance-declaration-regexp)
             "\\|"))

;; this gets called by outline to determine the level.
;; Just use the length of the whitespace
(defun haskell-outline-level ()
  (let* ((line (current-line))
         (is-special (string-match-pure? +haskell-special-line-regexp+ line))
         (level (let (buffer-invisibility-spec)
                  (save-excursion
                    (skip-chars-forward "\t ")
                    (current-column)))))
    (if is-special
        level
        (1+ level))))

(defun haskell-hide-all ()
  (interactive)
  (save-excursion
   (save-match-data
    (goto-char (point-min))
    (let ((re (mapconcat (lambda (x)
                           (concat "\\(?:" x "\\)"))
                         (list haskell-toplevel-signature-regexp
                               haskell-toplevel-data-declaration-regexp
                               haskell-toplevel-class-declaration-regexp
                               haskell-toplevel-instance-declaration-regexp
                               haskell-main-function-regexp)
                         "\\|")

              ;; (concat haskell-toplevel-signature-regexp
              ;;         "\\|"
              ;;         haskell-toplevel-data-declaration-regexp
              ;;         "\\|"
              ;;         haskell-toplevel-class-declaration-regexp
              ;;         "\\|"
              ;;         haskell-toplevel-instance-declaration-regexp
              ;;         "\\|"
              ;;         haskell-main-function-regexp)
              ))
      (while (re-search-forward re nil t)
        (hide-subtree))))))

(defun haskell-setup-folding ()
  (setq buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table
                          'selective-display
                          (string-to-vector " ...\n"))

  ;; outline uses this regexp to find headers.
  ;; I match lines with no indent and indented
  ;; some lines, such as "--" ... "class"
  (set (make-local-variable 'outline-regexp)
       ;; in case of problems with comments
       ;; try to remove everything before first \\| inclusively
       ;; "^[ \t]*-- \\|"
       (concat haskell-commented-line-regexp
               "\\|"
               "^[ \t]*[^-\n]\\{2\\}.*"
               (concat "\\(?:"
                       "[^:\n]::\\(?:[^:\n]\\|$\\)"
                       "\\|"
                       "[^=\n]=\\(?:[^=\n]\\|$\\|[ \t]+do\\>\\)"
                       "\\|"
                       "[^|]|[^|]+="
                       "\\)")
               "\\|"
               "^[ \t]*\\<\\(?:where\\|let\\|in\\|if\\|then\\|else\\|data\\|class\\|instance\\)\\>"))
  (set (make-local-variable 'outline-heading-end-regexp)
       "\\(?:[ \t]*[^ \t\n]+\\(?:[ \t]*\\(?:->\\)\\(?:[ \t]*--.*$\\)?\n\\)?\\)+\n")

  ;; enable our level computation
  (setq outline-level 'haskell-outline-level)
  ;; turn on outline mode
  (outline-minor-mode t)
  ;; initially hide all but the headers
  ;;(hide-body)

  (def-keys-for-map2 vim:normal-mode-local-keymap
    ("' b"     outline-up-heading)
    ("z C"     haskell-hide-all)
    ("z c"     hide-subtree)
    ("z O"     show-all)
    ("z o"     show-subtree)

    ("<left>"  outline-move-subtree-up)
    ("<right>" outline-move-subtree-down)
    ("<down>"  outline-forward-same-level)
    ("<up>"    outline-backward-same-level)
    ("="       outline-backward-same-level)))

(provide 'haskell-outline)


