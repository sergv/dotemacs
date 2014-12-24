;; gitconfig-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 28 December 2013
;; Description:

(require 'common)

(defparameter gitconfig-mode-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?\" "\"")
    (modify-syntax-entry ?\# "<")
    (modify-syntax-entry ?\n ">")
    tbl))

(define-derived-mode gitconfig-mode text-mode "Gitconfig"
  "Major mode to edit .gitconfig files"
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *")
  (setq-local indent-tabs-mode nil)
  (setq-local font-lock-defaults
              '(gitconfig-font-lock-keywords
                nil ;; perform syntactic fontification (e.g. strings, comments)
                )))

(defparameter gitconfig-font-lock-keywords
  `(("\\(?:^\\|\\s-+\\)\\(#.*\\)$"
     (1 'font-lock-comment-face))
    ("\"[^\"]*\""
     (0 'font-lock-string-face prepend))
    ("\\[\\([^\]]+\\)\\]"
     (1 'font-lock-keyword-face keep))))

;; Local Variables:
;; End:

;; gitconfig-mode.el ends here
