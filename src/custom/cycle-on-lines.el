;; cycle-on-lines.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


(defmacro make-cycle-on-lines-in-region
  (begin end direction
         &optional forward-func backward-func)
  "Return function than will
go to COUNTh next or previous line in range [BEGIN, lines - END) where
lines is line count in the current buffer. Never leaves point not within
range [BEGIN, lines - END) if, of course, there's enough lines.

DIRECTION may have value either 'forward or 'backward"
  (setq forward-func (or forward-func '#'forward-line))
  (setq backward-func (or backward-func '#'backward-line))
  (assert (memq direction '(forward backward)))
  `(lambda (count)
     (let* ((lines-in-buf (count-lines1 (point-min) (point-max)))
            (range-end (- lines-in-buf ,end))
            (lines-in-region (- lines-in-buf (+ ,begin ,end)))
            (current (count-lines1 (point-min) (point))))
       (setq count (% count lines-in-region))
       (cond ((or ;; not in cycling range
               (< current ,begin)
               (> current range-end))
              ,@(if (eq direction 'forward)
                  `((goto-char (point-min))
                    (funcall ,forward-func ,begin))
                  `((goto-char (point-max))
                    (funcall ,backward-func ,end))))

             ,(append
               (list
                (if (eq direction 'forward)
                  `(>= (+ current count) range-end)
                  `(<= (- current count) ,begin)))

               (if (eq direction 'forward)
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
              (funcall ,(if (eq direction 'forward)
                          forward-func
                          backward-func)
                       count))))))

(defun dired-cycle-files-forward (count)
  "Cycle through file list forward selecting next entry"
  (interactive "p")
  (funcall
   (make-cycle-on-lines-in-region 2 -1 forward
                                  #'dired-next-line #'dired-previous-line)
   count))

(defun dired-cycle-files-backward (count)
  "Cycle through file list backward selecting next entry"
  (interactive "p")
  (funcall
   (make-cycle-on-lines-in-region 2 -1 backward
                                  #'dired-next-line #'dired-previous-line)
   count))


;;;; special definitons for occur


(defun custom-occur-find-match (n direction &optional linewise)
  (setq n (or n 1))
  (let ((search-func (case direction
                       (forward #'next-single-property-change)
                       (backward #'previous-single-property-change)))
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
          (goto-char (case direction
                       (forward (point-min))
                       (backward (point-max))))
          ;; we found nothing so this try doesn't actually counts
          (incf n)))
      (incf n -1))))

(defun custom-occur-next (&optional n)
  "Move to the Nth (default 1) next match in an Occur mode buffer."
  (interactive "p")
  (custom-occur-find-match n 'forward t))

(defun custom-occur-prev (&optional n)
  "Move to the Nth (default 1) previous match in an Occur mode buffer."
  (interactive "p")
  (custom-occur-find-match n 'backward t))


;; Local Variables:
;; End:

;; cycle-on-lines.el ends here
