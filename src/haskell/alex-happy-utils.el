;; alex-happy-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 24 February 2025
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 'polymode)

;;;; Detection of bounds of Haskell sources within Happy/Alex files

(defconst haskell-blocks-default-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?\( "."     tbl)
    (modify-syntax-entry ?\) "."     tbl)
    (modify-syntax-entry ?\[ "."     tbl)
    (modify-syntax-entry ?\] "."     tbl)
    (modify-syntax-entry ?\{ "(}1nb" tbl)
    (modify-syntax-entry ?\} "){4nb" tbl)
    (modify-syntax-entry ?\' "."     tbl)
    (modify-syntax-entry ?\" "\""    tbl)
    (modify-syntax-entry ?-  ". 123" tbl)
    (modify-syntax-entry ?\n ">"     tbl)
    tbl)
  "Syntax table to help detecting Haskell regions in Alex and Happy files.")

(defun poly-alex-happy-find-front (direction enable-alex-heuristinc?)
  (let* ((fmt (comment-util-current-format))
         (continue t)
         (result nil)
         (search-forward? (< 0 direction))
         (search (if search-forward?
                     #'re-search-forward
                   #'re-search-backward)))
    (save-match-data
      (with-syntax-table haskell-blocks-default-syntax-table
        (let ((parse-sexp-lookup-properties nil))
          (while continue
            (if (funcall search (if enable-alex-heuristinc?
                                    "\\(?:\\`\\|[^> \t\r\n][ \t\r\n]*\\){[ \t\r\n%]"
                                  "{[ \t\r\n%]")
                         nil
                         t)
                (progn
                  (goto-char (match-end 0))
                  (if (or (and (not (eq (char-before) ?\n))
                               (comment-util--on-commented-line? fmt))
                          ;; (and enable-alex-heuristinc?
                          ;;      (save-excursion
                          ;;        (goto-char (match-beginning 0))
                          ;;        (skip-whitespace-backward)
                          ;;        (eq (char-before) ?>)))
                          )
                      (goto-char (if search-forward?
                                     (line-end-position)
                                   (line-beginning-position)))
                    (let* ((syntax-state (parse-partial-sexp (point-min) (point)))
                           (in-comment-or-string? (nth 8 syntax-state)))
                      (if in-comment-or-string?
                          (if search-forward?
                              (parse-partial-sexp (point)
                                                  (point-max)
                                                  nil
                                                  nil
                                                  syntax-state
                                                  'syntax-table ;; stop when comment ends
                                                  )
                            (goto-char in-comment-or-string?))
                        (let ((open-positions (nth 9 syntax-state)))
                          (when open-positions
                            (let ((outermost (car open-positions)))
                              (setf result (cons outermost (+ 2 outermost)))))
                          (setf continue nil))))))
              (setf continue nil))))))
    result))

(defun poly-alex-happy-find-tail (_direction)
  (skip-chars-backward " \t\r\n%")
  (when-let ((prev (char-before)))
    (when (and (eq prev ?\{)
               (not (eq (char-after) ?-)))
      (forward-char -1)
      (with-syntax-table haskell-blocks-default-syntax-table
        (let ((parse-sexp-lookup-properties nil))
          (forward-sexp 1)))
      (cons (1- (point)) (point)))))

(provide 'alex-happy-utils)

;; Local Variables:
;; End:

;; alex-happy-utils.el ends here
