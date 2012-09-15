;; gitignore-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 28 July 2012
;; Description:

(define-derived-mode gitignore-mode text-mode "Gitignore"
  "Simple mode to edit .gitignore files"
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'font-lock-defaults)
       '(gitignore-font-lock-keywords)))

(defvar gitignore-font-lock-keywords
  `(("\\(?:^\\|\\s-+\\)\\(#.*\\)$"
     (1 font-lock-comment-face))))

(provide 'gitignore-mode)

;; Local Variables:
;; End:

;; gitignore-mode.el ends here
