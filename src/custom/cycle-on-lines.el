;; cycle-on-lines.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


(defmacro make-cycle-on-lines-in-region
    (begin end is-forward?
           &optional forward-func backward-func)
  "Return function than will
go to COUNTh next or previous line in range [BEGIN, lines - END) where
lines is line count in the current buffer. Never leaves point not within
range [BEGIN, lines - END) if, of course, there's enough lines.

DIRECTION may have value either 'forward or 'backward"
  (setq forward-func (or forward-func '#'forward-line))
  (setq backward-func (or backward-func '#'backward-line))
  `(lambda (count)
     (let* ((lines-in-buf (count-lines1 (point-min) (point-max)))
            (range-end (- lines-in-buf ,end))
            (lines-in-region (- lines-in-buf (+ ,begin ,end)))
            (current (count-lines1 (point-min) (point))))
       (setq count (% count lines-in-region))
       (cond ((or ;; not in cycling range
               (< current ,begin)
               (> current range-end))
              ,@(if is-forward?
                    `((goto-char (point-min))
                      (funcall ,forward-func ,begin))
                  `((goto-char (point-max))
                    (funcall ,backward-func ,end))))

             ,(append
               (list
                (if is-forward?
                    `(>= (+ current count) range-end)
                  `(<= (- current count) ,begin)))

               (if is-forward?
                   `((incf count (- (- range-end current)))

                     (goto-char (point-min))
                     (funcall ,forward-func ,begin)
                     (dotimes (i count)
                       (funcall ,forward-func +1)))
                 `((incf count (- (- current ,begin)))

                   (goto-char (point-max))
                   (funcall ,backward-func ,(abs end))
                   (dotimes (i count)
                     (funcall ,backward-func -1)))))
             (t
              (funcall ,(if is-forward?
                            forward-func
                          backward-func)
                       count))))))

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
          (incf n)))
      (incf n -1))))

(defun custom-occur-next (&optional n)
  "Move to the Nth (default 1) next match in an Occur mode buffer."
  (interactive "p")
  (custom-occur-find-match n t t))

(defun custom-occur-prev (&optional n)
  "Move to the Nth (default 1) previous match in an Occur mode buffer."
  (interactive "p")
  (custom-occur-find-match n nil t))

;; Local Variables:
;; End:

;; cycle-on-lines.el ends here
