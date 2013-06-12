;; hideshow-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 17 July 2012
;; Description:

(eval-after-load
    "hideshow"
  '(begin
     (defadvice byte-compile-file (around
                                   byte-compile-file-hideshow-off
                                   activate
                                   compile)
       (let ((hs-minor-mode-hook nil))
         ad-do-it))


     ;; hideshow works badly with these
     (add-hook 'ediff-prepare-buffer-hook 'turn-off-hideshow)
     (add-hook 'vc-before-checkin-hook 'turn-off-hideshow)

     (defun hs-show-sexps-in-region (begin end)
       (interactive "r")
       (save-excursion
         (goto-char begin)
         (while (<= (point) end)
           (forward-sexp 1)
           (backward-sexp 1)
           (hs-show-block t)        ;; show and reposition
           (skip-syntax-forward ")" ;; close delimiters
                                )))
       ;; turn visual mode off
       (when (or (region-active-p)
                 (run-if-fbound vim:visual-mode-p))
         (deactivate-mark)
         (run-if-fbound vim:visual-mode-exit)))

     (defun hs-hide-sexps-in-region (begin end)
       (interactive "r")
       (save-excursion
         (goto-char begin)
         (while (<= (point) end)
           (forward-sexp 1)
           (backward-sexp 1)
           (hs-hide-block t)        ;; hide and reposition at the end
           (skip-syntax-forward ")" ;; skip close delimiters
                                )))
       ;; turn visual mode off
       (when (or (region-active-p)
                 (run-if-fbound vim:visual-mode-p))
         (deactivate-mark)
         (run-if-fbound vim:visual-mode-exit)))

     ;; todo: these two are quite similar to `hs-hide-sexps-in-region' and
     ;; `hs-show-sexps-in-region'
     (defun hs-show-c-sexps-in-region (begin end)
       (interactive "r")
       (save-excursion
         (goto-char begin)
         (while (<= (point) end)
           (skip-chars-forward "^{" end)
           (when (char=? (char-after) ?\{)
             (hs-show-block t)
             (skip-chars-forward "}"))))
       ;; turn visual mode off
       (when (or (region-active-p)
                 (run-if-fbound vim:visual-mode-p))
         (deactivate-mark)
         (run-if-fbound vim:visual-mode-exit)))

     (defun hs-hide-c-sexps-in-region (begin end)
       (interactive "r")
       (save-excursion
         (goto-char begin)
         (while (progn
                  (skip-chars-forward "^{" end)
                  (and (char=? (char-after) ?\{)
                       (<= (point) end)))
           (hs-hide-block t)        ;; hide and reposition at the end
           (skip-chars-forward "}") ;; close delimiters
           ))
       ;; turn visual mode off
       (when (or (region-active-p)
                 (run-if-fbound vim:visual-mode-p))
         (deactivate-mark)
         (run-if-fbound vim:visual-mode-exit)))

     ;; this is a modification that handles case when
     ;; hs-hide-comments-when-hiding-all is nil better
     (defun hs-hide-all ()
       "Hide all top level blocks, displaying only first and last lines.
Move point to the beginning of the line, and run the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'.
If `hs-hide-comments-when-hiding-all' is non-nil, also hide the comments."
       (interactive)
       (hs-life-goes-on
        (save-excursion
          (unless hs-allow-nesting
            (hs-discard-overlays (point-min) (point-max)))
          (goto-char (point-min))
          (let ((spew (make-progress-reporter "Hiding all blocks..."
                                              (point-min) (point-max)))
                (re (concat "\\("
                            hs-block-start-regexp
                            "\\)"
                            (if hs-hide-comments-when-hiding-all
                              (concat "\\|\\("
                                      hs-c-start-regexp
                                      "\\)")
                              ""))))
            (while (re-search-forward re (point-max) t)

              (cond
                ((and (not hs-hide-comments-when-hiding-all)
                      (lisp-point-inside-comment?))
                 (forward-comment (point-max)))

                ((not (null? (match-beginning 1)))
                 ;; we have found a block beginning
                 (goto-char (match-beginning 1))
                 (if hs-hide-all-non-comment-function
                   (funcall hs-hide-all-non-comment-function)
                   (hs-hide-block-at-point t)))

                (else
                 ;; found a comment, probably
                 (let ((c-reg (hs-inside-comment-p)))
                   (when (and c-reg (car c-reg))
                     (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                       (hs-hide-block-at-point t c-reg)
                       (goto-char (nth 1 c-reg)))))))
              (progress-reporter-update spew (point)))
            (progress-reporter-done spew)))
        (beginning-of-line)
        (run-hooks 'hs-hide-hook)))

     ;; add check hs-block-start-mdata-select
     (redefun hs-forward-sexp (match-data arg)
       "Adjust point based on MATCH-DATA and call `hs-forward-sexp-func' w/ ARG.
Original match data is restored upon return."
       (save-match-data
         (set-match-data match-data)
         (when hs-block-start-mdata-select
           (goto-char (match-beginning hs-block-start-mdata-select)))
         (funcall hs-forward-sexp-func arg)))))


(provide 'hideshow-setup)

;; Local Variables:
;; End:

;; hideshow-setup.el ends here
