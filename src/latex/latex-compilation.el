;; latex-compilation.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 30 August 2011
;; Keywords:
;; Requirements:
;; Status:

(defconst latex-compile-error-regexp
  "^\\(\\(?:/[^/\n\t]+\\)*?/?[^/\n\t]+\\):\\([0-9]+\\): ")

(defconst latex-compile-warning-regexp
  "\\(?:Latex\\|LaTeX\\|Package\\).*Warning:")

(defun latex-compile ()
  "Start compilation of LaTeX file."
  (interactive)
  (save-buffer-if-modified)
  (compilation-start compile-command
                     #'latex-compilation-mode))

(define-compilation-mode latex-compilation-mode "LaTeX"
  (setq-local compilation-scroll-output 'first-error

              compilation-error-regexp-alist
              (list
               (list latex-compile-warning-regexp nil)
               (list latex-compile-error-regexp 1 2))))

(provide 'latex-compilation)

;; Local Variables:
;; End:

;; latex-compilation.el ends here
