;; pseudoparedit.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 22 January 2022
;; Description:

;;;###autoload
(defun pseudoparedit-insert-round ()
  "Smart insertion of paired () delimiters."
  (interactive)
  (let ((before (char-before)))
    (cond
      ((eq before ?\\)
       (insert-char ?\()
       (insert-char ?\\)
       (insert-char ?\))
       (forward-char -2))
      (t
       (insert-char ?\()
       (insert-char ?\))
       (forward-char -1)))))

;;;###autoload
(defun pseudoparedit-insert-square ()
  "Smart insertion of paired [] delimiters."
  (interactive)
  (insert-char ?\[)
  (insert-char ?\])
  (forward-char -1))

;;;###autoload
(defun pseudoparedit-insert-curly ()
  "Smart insertion of paired { delimiters."
  (interactive)
  (insert-char ?\{)
  (insert-char ?\})
  (forward-char -1))

;;;###autoload
(defun pseudoparedit-backspace ()
  (interactive)
  (let* ((before (char-before))
         (start (1- (point)))
         (end nil)
         (after (save-excursion
                  (skip-chars-forward " \t\n\r")
                  (unless (eobp)
                    (setq end (1+ (point))))
                  (char-after))))
    (if (and end
             (eq after (cdr (assq before '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\}))))))
        (delete-region start end)
      (delete-char -1))))

(provide 'pseudoparedit)

;; Local Variables:
;; End:

;; pseudoparedit.el ends here
