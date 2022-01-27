;; smart-operators-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 29 November 2019
;; Description:

(defun smart-operators--point-surrounded-by2 (before2 before1 after1 after2)
  "Check if previous 2 characters before point are BEFORE2 and BEFORE1 and
that next 2 characters are AFTER1 and AFTER2."
  (let* ((pt-before    (save-excursion
                         (skip-syntax-backward " ")
                         (point)))
         (pt-after     (save-excursion
                         (skip-syntax-forward " ")
                         (point)))
         (real-before2 (char-before (- pt-before 1)))
         (real-before1 (char-before pt-before))
         (real-after1  (char-after pt-after))
         (real-after2  (char-after (+ pt-after 1))))
    (list
     pt-before
     pt-after
     (and real-before2
          real-before1
          real-after1
          real-after2
          (char-equal real-before2 before2)
          (char-equal real-before1 before1)
          (char-equal real-after1  after1)
          (char-equal real-after2  after2)))))

(defun smart-operators--point-surrounded-by (before after)
  "Check if point is surrounded by BEFORE and AFTER characters."
  (let* ((pt-before   (save-excursion
                        (skip-syntax-backward " ")
                        (point)))
         (pt-after    (save-excursion
                        (skip-syntax-forward " ")
                        (point)))
         (real-before (char-before pt-before))
         (real-after  (char-after pt-after))
         (is-before? (and real-before
                          (char-equal real-before before)))
         (is-after? (and real-after
                         (char-equal real-after after))))
    (list pt-before
          pt-after
          is-before?
          is-after?
          (and is-before?
               is-after?))))

(defun smart-operators--in-string-syntax? ()
  (if (bobp)
      nil
    (and (eq (syntax-class (syntax-after (1- (point)))) 7)
         (eq (syntax-class (syntax-after (point))) 7))))

;;;###autoload
(defun smart-operators--in-string-or-comment? (&optional disable-comment-check?)
  "Are we in string or comment?"
  (if (smart-operators--in-string-syntax?)
      t
    (let* ((state (parse-partial-sexp (line-beginning-position)
                                      (point)))
           (inside-string? (elt state 3))
           (inside-comment? (if disable-comment-check?
                                nil
                              (elt state 4))))
      (or inside-string?
          inside-comment?
          (and (eq 'font-lock-string-face
                   (get-text-property (point) 'face))
               (if (and (char-equal (char-after) ?\")
                        (/= (point-min) (point)))
                   (eq 'font-lock-string-face
                       (get-text-property (- (point) 1) 'face))
                 t))))))

;;;###autoload
(defun smart-operators--literal-insertion? (&optional disable-comment-check?)
  "Should a node have literal insertion?"
  (or (smart-operators--in-string-or-comment? disable-comment-check?)
      (let ((before (char-before))
            (after (char-after)))
        (and before
             after
             (or
              ;; Test for positions: '_|_', \\_|_'
              (and (or (eq before ?\')
                       (eq before ?\\))
                   (eq after ?\'))
              ;; Test for positions: "_|_", \\_|_"
              (and (or (eq before ?\")
                       (eq before ?\\))
                   (eq after ?\")))))))

(defun smart-operators--on-empty-string? ()
  (let ((start (line-beginning-position)))
    (or (= start (line-end-position))
        (save-excursion
          (goto-char start)
          (looking-at-p "^[ \t]*$")))))

;;;###autoload
(defun smart-operators-comma ()
  "Insert comma followed by space."
  (interactive "*")
  (let ((next-char (char-after)))
    (insert-char ?\,)
    (when (or (not next-char)
              (not (memq next-char '(?\s ?\t ?\'))))
      (insert-char ?\s))))

(provide 'smart-operators-utils)

;; Local Variables:
;; End:

;; smart-operators-utils.el ends here
