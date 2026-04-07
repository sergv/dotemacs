;; cycle-on-lines.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'cl))

;;; special definitons for occur

(defun custom-occur-find-match (n is-forward? &optional linewise)
  (setq n (or n 1))
  (let ((search-func (if is-forward?
                         #'next-single-property-change
                       #'previous-single-property-change))
        (r nil))
    (unless search-func
      (error "custom-occur-find-match: unknown direction"))
    (while (> n 0)
      (setq r (funcall search-func (point) 'occur-match))
      (and linewise
           r
           (get-text-property r 'occur-match)
           (setq r (funcall search-func r 'occur-match)))
      (if r
          (goto-char r)
        (progn
          (goto-char (if is-forward?
                         (point-min)
                       (point-max)))
          ;; we found nothing so this try doesn't actually counts
          (cl-incf n)))
      (cl-incf n -1))))

(defun custom-occur-next (&optional n)
  "Move to the Nth (default 1) next match in an Occur mode buffer."
  (interactive "p")
  (custom-occur-find-match n t t))

(defun custom-occur-prev (&optional n)
  "Move to the Nth (default 1) previous match in an Occur mode buffer."
  (interactive "p")
  (custom-occur-find-match n nil t))

(provide 'cycle-on-lines)

;; Local Variables:
;; End:

;; cycle-on-lines.el ends here
