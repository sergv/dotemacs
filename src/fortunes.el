;; fortunes.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'comment-util)
(require 'common)
(require 'persistent-store)

(defun fortunes--read-nth-fortune (buf n)
  (with-current-buffer buf
    (goto-char (point-min))
    (let ((prev-pt (point)))
      (skip-chars-forward "^\0")
      (forward-char 1)
      (dotimes (_ n)
        (setf prev-pt (point))
        (skip-chars-forward "^\0")
        (forward-char 1))
      (decode-coding-string
       (buffer-substring-no-properties prev-pt (1- (point)))
       'utf-8))))

(defun fortunes--count-fortunes (buf)
  (with-current-buffer buf
    (let ((res 0))
      (goto-char (point-min))
      (while (not (eobp))
        (skip-chars-forward "^\0")
        (unless (eobp)
          (forward-char 1)
          (cl-incf res)))
      res)))

;;;###autoload
(defun fortunes-get-current-fortune ()
  (with-temp-buffer
    (insert-file-contents (concat +resources-path+ "/good-fortunes.txt"))
    (let ((current-seconds (time-convert (current-time) 'integer))
          (total-fortunes (fortunes--count-fortunes (current-buffer))))
      (fortunes--read-nth-fortune (current-buffer)
                                  (mod current-seconds total-fortunes)))))

;;;###autoload
(defun fortunes-init-scratch-buffer ()
  "Put fortune into scratch buffer."
  (random t)
  (setf initial-scratch-message
        (fortunes-comment-out-fortune (fortunes-get-current-fortune)))
  (awhen (get-buffer "*scratch*")
    (set-buffer-major-mode it)))

;;;###autoload
(defun fortunes-comment-out-fortune (fortune-text)
  (mapconcat (lambda (x) (concat ";; " x))
             (split-into-lines fortune-text)
             "\n"))

(provide 'fortunes)

;; Local Variables:
;; End:

;; fortunes.el ends here
