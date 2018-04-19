;; mode-line-setup.el -- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday,  7 November 2016
;; Description:

(defvar-local mode-line--buffer-line-count nil)

(defun mode-line-show-line-count ()
  "Show number of lines in the `mode-line-format'."
  (if (and (not (buffer-modified-p))
           mode-line--buffer-line-count)
      mode-line--buffer-line-count
    "?"))

(defun mode-line--count-lines ()
  (setf mode-line--buffer-line-count
        (mode-line-show-line-count-uncached)))

(defun mode-line-show-line-count-uncached ()
  (int-to-string
   (count-lines (point-min) (point-max))))

(add-hook 'find-file-hook 'mode-line--count-lines)
(add-hook 'after-save-hook 'mode-line--count-lines)
(add-hook 'after-revert-hook 'mode-line--count-lines)
(add-hook 'dired-after-readin-hook 'mode-line--count-lines)

(defun mode-line-show-region-size ()
  "Format region size for `mode-line-format'."
  (when (region-active-p)
    (multiple-value-bind (start end) (get-region-bounds)
      (format " [%s]"
              (count-lines start end)))))


(defun* use-repl-modeline (&key (show-column t) (show-directory nil))
  "Set up `mode-line' for use in vairous repl."
  (setf mode-line-format
        `(" %[%b%] "
          ("(%m"
           mode-line-process
           ")")
          (:eval
           (when (buffer-narrowed?)
             "(Narrowed)"))
          " "
          (line-number-mode
           ("%l"
            (column-number-mode
             ":%c")
            "/"
            (:eval (mode-line-show-line-count-uncached))))
          (:eval (mode-line-show-region-size))

          ,@(when show-directory
              '("  "
                (:eval default-directory)))
          global-mode-string)))

(defvar-local modeline--syntax-check-result 'disabled
  "Possible values: 'disabled, 'ok, 'error, 'unknown.")
(defvar-local modeline--syntax-check-cache nil
  "String representation of syntax check outcome specified in
`modeline--syntax-check-result'.")

;;;###autoload
(defun modeline-set-syntax-check-result (new-result &optional properties)
  "Update syntax check status so it will be visible in the modeline."
  (declare (indent 1))
  (cl-assert (symbolp new-result))
  (unless (eq new-result modeline--syntax-check-result)
    (setf modeline--syntax-check-cache
          (if (eq new-result 'disabled)
              nil
            (let ((str
                   (pcase new-result
                     (`ok      "Syntax:OK")
                     (`error   "Syntax:Error")
                     (`unknown "Syntax:?")
                     (invalid
                      (error "Invalid syntax check result: %s" new-result)))))
              (concat (apply #'propertize str properties)
                      " ")))
          modeline--syntax-check-result new-result)
    (force-mode-line-update)))

(setq-default
 mode-line-format
 '("%[%b%] "
   modeline--syntax-check-cache
   (:eval (cond
            (buffer-read-only
             "(RO) ")
            ;; if buffer has assigned file and is modified
            ((and buffer-file-name
                  (buffer-modified-p))
             "(+) ")
            (t
             nil)))
   mode-name
   mode-line-process
   (:eval
    (case (coding-system-eol-type buffer-file-coding-system)
      ;; here should be unix but it is most of the time so
      ;; there's no reason to say obvious things
      (0 "")
      (1 "(dos)")
      (2 "(mac)")))
   (:eval
    (when (buffer-narrowed?)
      "(Narrowed)"))
   vc-mode
   (:eval
    (awhen (and (fboundp #'flycheck-pretty-mode-line)
                (flycheck-pretty-mode-line))
      (concat " " it)))
   " "
   (line-number-mode
    ("%l"
     (column-number-mode
      ":%c")
     "/"
     (:eval (mode-line-show-line-count))))
   (:eval (mode-line-show-region-size))
   ;; (which-func-mode (" (" which-func-format ")"))
   global-mode-string))

(provide 'mode-line-setup)

;; Local Variables:
;; End:

;; mode-line-line-count.el ends here
