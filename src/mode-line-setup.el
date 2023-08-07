;; mode-line-setup.el -- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday,  7 November 2016
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'common))

;; Display line numbers in mode line even in buffers that have lines
;; below this length, on average.
(setf line-number-display-limit-width 4096)

(defvar-local mode-line--buffer-line-count nil)

(defun mode-line-show-line-count ()
  "Show number of lines in the `mode-line-format'."
  (cond
    ((buffer-narrowed-p)
     ;; Don’t cache - cannot reliably capture hook all events of
     ;; narrowing and widening (widening can happen via
     ;; ‘save-restriction’ which cannot be advised).
     (int-to-string (line-number-at-pos (point-max) nil)))
    ((and (not (buffer-modified-p))
          mode-line--buffer-line-count)
     mode-line--buffer-line-count)
    (t
     "?")))

(defun mode-line--count-lines ()
  (setf mode-line--buffer-line-count
        (mode-line-show-line-count-uncached)))

(defun mode-line-show-line-count-uncached ()
  (int-to-string (line-number-at-pos (point-max) t)))

(add-hook 'find-file-hook #'mode-line--count-lines)
(add-hook 'after-save-hook #'mode-line--count-lines)
(add-hook 'after-revert-hook #'mode-line--count-lines)
(add-hook 'dired-after-readin-hook #'mode-line--count-lines)

(defun mode-line-show-region-size ()
  "Format region size for `mode-line-format'."
  (when (region-active-p)
    (with-region-bounds-unadj start end
      (format " [%s]"
              (count-lines-fixed start end)))))

(defun mode-line-show-search-matches ()
  "Show number of items that matched last search `mode-line-format'."
  search--mode-line-matches)

(cl-defun use-repl-modeline (&key (show-directory nil))
  "Set up `mode-line' for use in vairous repl."
  (setf mode-line-format
        `(" %[%b%] "
          ("(%m"
           mode-line-process
           ")")
          (:eval
           (when (buffer-narrowed-p)
             "(Narrowed)"))
          " "
          (line-number-mode
           ("%l"
            (column-number-mode
             ":%c")
            "/"
            (:eval (mode-line-show-line-count-uncached))))
          (:eval (mode-line-show-region-size))
          (:eval (mode-line-show-search-matches))

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
     (when (buffer-narrowed-p)
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
    (:eval (mode-line-show-search-matches))
    ;; (which-func-mode (" (" which-func-format ")"))
    global-mode-string))

(setq-default mode-line-format (default-mode-line-format))

(provide 'mode-line-setup)

;; Local Variables:
;; End:

;; mode-line-line-count.el ends here
