;; hideshow-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 17 July 2012
;; Description:

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
  (when (region-active?)
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
  (when (region-active?)
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
  (when (region-active?)
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
  (when (region-active?)
    (deactivate-mark)
    (run-if-fbound vim:visual-mode-exit)))

;; add check hs-block-start-mdata-select
(redefun hs-forward-sexp (match-data arg)
  "Adjust point based on MATCH-DATA and call `hs-forward-sexp-func' w/ ARG.
Original match data is restored upon return."
  (save-match-data
    (set-match-data match-data)
    (when hs-block-start-mdata-select
      (goto-char (match-beginning hs-block-start-mdata-select)))
    (funcall hs-forward-sexp-func arg)))


(provide 'hideshow-setup)

;; Local Variables:
;; End:

;; hideshow-setup.el ends here
