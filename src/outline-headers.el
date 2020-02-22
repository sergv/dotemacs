;; outline-headers.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday,  9 May 2012
;; Description:

(require 'comment-util)

(defvar-local outline-headers/min-header-length nil
  "Minimum number of `outline-headers/section-symbol''s allowed in header.")

(defvar-local outline-headers/section-start
  "^"
  "Beginning part of `outline-headers/header-re'.")

(defvar-local outline-headers/section-symbol
  nil
  "Main part of `outline-headers/header-re' that defines headers of different length.")

(defvar-local outline-headers/section-end
  "\\(?: \\|$\\)"
  "End part of `outline-headers/header-re'.")



(defvar-local outline-headers/header-re
  nil
  "Regular expression that defines headers")

;;; Setup function

;;;###autoload
(defun* setup-outline-headers (&key
                               (header-start "^")
                               (header-symbol nil)
                               (header-end "\\(?: \\|$\\)")
                               (length-min 3))
  (unless header-symbol
    (setf header-symbol
          (assq 'one-line
                (v-assq major-mode
                        +comment-util-comment-format-alist+)))
    (when (< 1 (length header-symbol))
      (error "setup-outline-headers: error: fetched header-symbol from comment-util but it's length is greater than 1: \"%s\" and no other header-symbol was provided"
             header-symbol)))
  (cl-assert (and (string? header-symbol)
                  (= 1 (length header-symbol)))
             nil
             "header-symbol must be string of length 1")
  (cl-assert (string? header-start)
             nil
             "header-start must be string")
  (cl-assert (string? header-end)
             nil
             "header-end must be string")
  (cl-assert (and (integer? length-min)
                  (>= length-min 1))
             nil
             "length-min must be integer >= 1")

  (setf outline-headers/section-start     header-start
        outline-headers/section-symbol    (regexp-quote header-symbol)
        outline-headers/section-end       header-end
        outline-headers/min-header-length length-min)

  (setf outline-headers/header-re
        (format "%s\\(?1:%s\\{%d,\\}\\)%s"
                outline-headers/section-start
                outline-headers/section-symbol
                outline-headers/min-header-length
                outline-headers/section-end))

  (setf buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table
                          'selective-display
                          (string-to-vector " ..."))

  (setq-local outline-regexp
              outline-headers/header-re)
  (setq-local outline-heading-end-regexp
              (concat "\\(?:"
                      outline-headers/header-re
                      ".*?"
                      "\\(?:\\\\\n.*\\)?"
                      "\n"
                      "\\)+"))

  (outline-minor-mode +1)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("z F" outline-hide-body)
    ("z f" outline-hide-subtree)
    ("z U" outline-show-all)
    ("z u" outline-show-subtree)))

(provide 'outline-headers)

;; Local Variables:
;; End:

;; outline-headers.el ends here
