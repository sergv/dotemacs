;; cmake-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  6 November 2012
;; Description:

(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

(autoload 'cmake-mode "cmake-mode" nil t)

(defun cmake-setup ()
  (init-common :use-yasnippet nil))

(add-hook 'cmake-mode-hook #'cmake-setup)


(provide 'cmake-setup)

;; Local Variables:
;; End:

;; cmake-setup.el ends here
