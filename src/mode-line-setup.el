;; mode-line-setup.el -- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday,  7 November 2016
;; Description:

(eval-when-compile
  (require 'common))

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
    (with-region-bounds start end
      (format " [%s]"
              (count-lines start end)))))


(defun* use-repl-modeline (&key (show-directory nil))
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

(defun default-mode-line-format (&rest checkers-specs)
  `("%[%b%] "
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
    ,@checkers-specs
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

(setq-default mode-line-format (default-mode-line-format))

(provide 'mode-line-setup)

;; Local Variables:
;; End:

;; mode-line-line-count.el ends here
