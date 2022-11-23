;; pseudoparedit.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 22 January 2022
;; Description:

(require 'common-whitespace)

(defun pseudoparedit--insert-pair (open close enable-quotes? enable-double-quotes? insert-space-before? insert-space-after?)
  (let* ((pt (point))
         (before (char-before pt))
         (before2 (and enable-double-quotes?
                       (< (point-min) pt)
                       (char-before (1- pt)))))
    (cond
      ((and enable-quotes?
            (eq before ?\\))
       (insert-char open)
       (insert-char ?\\)
       (if (eq before2 ?\\)
           (progn
             (insert-char ?\\)
             (insert-char close)
             (forward-char -3))
         (progn
           (insert-char close)
           (forward-char -2))))
      (t
       (when (and (if (functionp insert-space-before?)
                      (funcall insert-space-before? before)
                    insert-space-before?)
                  (not (whitespace-char? before)))
         (insert-char ?\s))
       (insert-char open)
       (insert-char close)
       (let ((after (char-after)))
         (if (or (not (if (functionp insert-space-after?)
                          (funcall insert-space-after? after)
                        insert-space-after?))
                 (whitespace-char? after))
             (forward-char -1)
           (progn
             (insert-char ?\s)
             (forward-char -2))))))))

;;;###autoload
(defun pseudoparedit-insert-paren ()
  "Smart insertion of paired () delimiters."
  (interactive)
  (pseudoparedit--insert-pair ?\( ?\) t t nil nil))

;;;###autoload
(defun pseudoparedit-insert-bracket ()
  "Smart insertion of paired [] delimiters."
  (interactive)
  (pseudoparedit--insert-pair ?\[ ?\] t nil nil nil))

;;;###autoload
(defun pseudoparedit-insert-brace ()
  "Smart insertion of paired { delimiters."
  (interactive)
  (pseudoparedit--insert-pair ?\{ ?\} nil nil nil nil))

;;;###autoload
(defun pseudoparedit-insert-double-quote (&optional literal-insertion?)
  "Smart insertion of paired \"\" delimiters."
  (interactive "P")
  (cond
    (literal-insertion?
     (insert-char ?\"))
    ((eq (char-after) ?\")
     (forward-char))
    (t
     (pseudoparedit--insert-pair ?\" ?\" t nil nil nil))))

;;;###autoload
(defun pseudoparedit-backspace (&optional count)
  (interactive "*p")
  (dotimes (_ (or count 1))
    (let* ((before (char-before))
           (start (1- (point)))
           (end nil)
           (after (save-excursion
                    (skip-chars-forward " \t\n\r")
                    (unless (eobp)
                      (setq end (1+ (point))))
                    (char-after))))
      (if (and end
               (eq after (cdr (assq before '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\}) (?\" . ?\"))))))
          (delete-region start end)
        (delete-char -1)))))

(provide 'pseudoparedit)

;; Local Variables:
;; End:

;; pseudoparedit.el ends here
