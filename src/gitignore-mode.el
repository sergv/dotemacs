;; gitignore-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 28 July 2012
;; Description:

(define-derived-mode gitignore-mode text-mode "Gitignore"
  "Simple mode to edit .gitignore files"
  (setq-local comment-start "# "
              comment-start-skip "#+ *"
              indent-tabs-mode nil
              font-lock-defaults
              '(gitignore-font-lock-keywords)))

(defparameter gitignore-font-lock-keywords
  `(("\\(?:^\\|\\s-+\\)\\(#.*\\)$"
     (1 'font-lock-comment-face))))

(provide 'gitignore-mode)

;; Local Variables:
;; End:

;; gitignore-mode.el ends here
