;; c-preprocessor.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  5 August 2022
;; Description:

(require 'dash)

(defun c-preprocessor-forward-conditional (count)
  "Clone of `c-scan-conditionals' with interface of `forward-sexp' for matching
#if with #else and #else with #endif."
  (let* ((forward (> count 0))
         (increment (if forward -1 1))
         (search-function (if forward #'re-search-forward #'re-search-backward))
         (target-depth 0)
         result)
    (while (/= count 0)
      (let ((depth 0)
            (found nil))
        (save-excursion
          ;; Find the "next" significant line in the proper direction.
          (while (and (not found)
                      (funcall search-function
                               +c-preprocessor-directives-re+
                               nil
                               t))
            (beginning-of-line)
            (let* ((directive (match-string-no-properties 1))
                   (dchange (cond ((or (string= directive "if")
                                       (string= directive "ifdef")
                                       (string= directive "ifndef"))
                                   (- increment))
                                  ((string= directive "endif")
                                   increment)
                                  ((= depth 0)
                                   +1)
                                  ((= depth +1)
                                   -1))))
              (when dchange
                (setq depth (+ depth dchange))
                ;; If we are trying to move across, and we find an
                ;; end before we find a beginning, get an error.
                (when (and (< depth target-depth)
                           (< dchange 0))
                  (error (concat (if forward
                                     "No following conditional at this level"
                                   "No previous conditional at this level")
                                 ", depth = %s, target-depth = %s")
                         depth
                         target-depth)))
              ;; When searching forward, start from next line so
              ;; that we don't find the same line again.
              (when forward (forward-line 1))
              ;; We found something if we've arrived at the
              ;; target depth.
              (when (and dchange (= depth target-depth))
                (setq found (point))))))
        (or found
            (error "No containing preprocessor conditional"))
        (goto-char (setq result found)))
      (setq count (+ count increment)))))

(defconst +c-preprocessor-directives-re+
  (rx (seq bol
           "#"
           (* (any ?\s ?\t))
           (group-n 1
                    (or "ifdef"
                        "ifndef"
                        "if"
                        "elif"
                        "endif"
                        "else"))
           symbol-end)))

(defconst +c-preprocessor-open-hideshow-re+
  (rx (or (seq bol
               "#"
               (* (any ?\s ?\t))
               (or "ifdef"
                   "ifndef"
                   "if"
                   "elif"
                   "else")
               symbol-end)
          (syntax ?\()))
  "Regexp matching CPP directives that could serve as beginning of folded region.")

(defconst +c-preprocessor-close-hideshow-re+
  (rx (or (seq bol
               "#"
               (* (any ?\s ?\t))
               (or "endif"
                   "elif"
                   "else")
               symbol-end)
          (syntax ?\))))
  "Regexp matching CPP directives that could serve as beginning of folded region.")

(defun c-preprocessor-hideshow-forward-sexp (&optional arg)
  "Special version of `forward-sexp' for hideshow in c-mode and others where C
preprocessor may be used."
  (if (eq (char-syntax (char-after)) ?\()
      (forward-sexp arg)
    (let ((start (point))
          (case-fold-search nil))
      (save-match-data
        (c-preprocessor-forward-conditional (or arg 1))
        (re-search-backward +c-preprocessor-directives-re+ start t)))))

(provide 'c-preprocessor)

;; Local Variables:
;; End:

;; c-preprocessor.el ends here
