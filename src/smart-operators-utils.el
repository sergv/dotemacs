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
    (point-inside-string? (point))))

;;;###autoload
(defun smart-operators--in-string-or-comment? (&optional disable-comment-check?)
  "Are we in string or comment?"
  (if disable-comment-check?
      (point-inside-string? (point))
    (point-inside-string-or-comment? (point))))

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

(defun smart-operators--insert-pair (open close insert-space-before? insert-space-after?)
  (let ((before (char-before)))
    (when (and (funcall insert-space-before? before)
               (not (whitespace-char? before)))
      (insert-char ?\s))
    (insert-char open)
    (insert-char close)
    (let ((after (char-after)))
      (if (or (not (funcall insert-space-after? after))
              (whitespace-char? after))
          (forward-char -1)
        (progn
          (insert-char ?\s)
          (forward-char -2))))))

;;;###autoload
(defun smart-operators-double-quote (literal-insertion?)
  (interactive "P")
  (cond
    (literal-insertion?
     (insert-char ?\"))
    ((eq (char-after) ?\")
     (forward-char))
    (t
     (smart-operators--insert-pair ?\"
                                   ?\"
                                   (lambda (before)
                                     (not (or (eq before ?\()
                                              (eq before ?\[)
                                              (eq before ?\\))))
                                   (lambda (after)
                                     (not (or (eq after ?\))
                                              (eq after ?\]))))))))

;;;###autoload
(defun smart-operators-open-paren ()
  (interactive)
  (smart-operators--insert-pair ?\(
                                ?\)
                                (lambda (before)
                                  (not (or (eq before ?\()
                                           (eq before ?\[)
                                           (eq before ?\\))))
                                (lambda (after)
                                  (not (or (eq after ?\))
                                           (eq after ?\]))))))

;;;###autoload
(defun smart-operators-open-bracket ()
  (interactive)
  (smart-operators--insert-pair ?\[
                                ?\]
                                (lambda (before)
                                  (not (or (eq before ?\()
                                           (eq before ?\[)
                                           (eq before ?\\))))
                                (lambda (after)
                                  (not (or (eq after ?\))
                                           (eq after ?\]))))))

;;;###autoload
(defun smart-operators-close-paren (literal-insertion?)
  (interactive "P")
  (if (and (not literal-insertion?)
           (eq (char-after) ?\)))
      (forward-char 1)
    (insert-char ?\))))

;;;###autoload
(defun smart-operators-close-bracket (literal-insertion?)
  (interactive "P")
  (if (and (not literal-insertion?)
           (eq (char-after) ?\]))
      (forward-char 1)
    (insert-char ?\])))

;;;###autoload
(defun smart-operators-close-brace (literal-insertion?)
  (interactive "P")
  (if (and (not literal-insertion?)
           (eq (char-after) ?\}))
      (forward-char 1)
    (insert-char ?\})))

(provide 'smart-operators-utils)

;; Local Variables:
;; End:

;; smart-operators-utils.el ends here
