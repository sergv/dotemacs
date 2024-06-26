;; latex-autoloads.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 28 August 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'tex-site)

(setq auto-mode-alist
      (remove '("\\.[tT]e[xX]\\'" . tex-mode) auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.[tT][eE][xX]\\'" . LaTeX-mode))

(autoload 'latex-compile
          "latex-compilation"
          "Start compilation of LaTeX file."
          t)

(add-hook 'latex-mode-hook #'latex-setup)
(add-hook 'LaTeX-mode-hook #'latex-setup)

(provide 'latex-autoloads)

;; Local Variables:
;; End:

;; latex-autoloads.el ends here
