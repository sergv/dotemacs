;; gitconfig-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 28 December 2013
;; Description:

(require 'conf-mode)
(require 'common)

(define-derived-mode gitconfig-mode text-mode "Gitconfig"
  "Major mode to edit .gitconfig files"
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *")
  (setq-local indent-tabs-mode nil)
  (setq-local font-lock-defaults
              '(gitconfig-font-lock-keywords
                nil ;; perform syntactic fontification (e.g. strings, comments)
                )))

(defvar gitconfig-font-lock-keywords
  `(;; ("\\(?:^\\|\\s-+\\)\\(#.*\\)$"
    ;;  (1 'font-lock-comment-face))
    ("\\[\\([^\]]+\\)\\]"
     (1 'font-lock-keyword-face))))

;; Local Variables:
;; End:

;; gitconfig-mode.el ends here
