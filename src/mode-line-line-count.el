;; mode-line-line-count.el -- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday,  7 November 2016
;; Description:


(defvar-local mode-line--buffer-line-count nil)

(defun mode-line-show-line-count ()
  (if (and (not (buffer-modified-p))
           mode-line--buffer-line-count)
      mode-line--buffer-line-count
    "?"))

(defun mode-line--count-lines ()
  (setf mode-line--buffer-line-count
        (int-to-string
         (count-lines (point-min) (point-max)))))

(add-hook 'find-file-hook 'mode-line--count-lines)
(add-hook 'after-save-hook 'mode-line--count-lines)
(add-hook 'after-revert-hook 'mode-line--count-lines)
(add-hook 'dired-after-readin-hook 'mode-line--count-lines)

(provide 'mode-line-line-count)

;; Local Variables:
;; End:

;; mode-line-line-count.el ends here
