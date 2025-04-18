;; llvm-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 18 November 2012
;; Description:

;;;###autoload
(autoload 'llvm-mode "llvm-mode" "Major mode for editing LLVM source files." t)

;;;###autoload
(setq auto-mode-alist
      (cons '("\\.ll\\'" . llvm-mode)
            (cons '("\\.dump-llvm\\'" . llvm-mode)
                  auto-mode-alist)))

;;;###autoload
(defun llvm-mode-setup ()
  (init-common :use-yasnippet nil
               :use-comment t
               :use-whitespace 'tabs-only
               :use-fci t
               :use-render-formula t))

;;;###autoload
(add-hook 'llvm-mode-hook #'llvm-mode-setup)

;;;###autoload
(autoload 'tablegen-mode "tablegen-mode" "Major mode for editing TableGen description files." t)

;;;###autoload
(setq auto-mode-alist
      (cons '("\\.td\\'" . tablegen-mode)
            auto-mode-alist))

;;;###autoload
(defun tablegen-mode-setup ()
  (init-common :use-yasnippet nil
               :use-comment t
               :use-whitespace 'tabs-only
               :use-fci t
               :use-render-formula t))

;;;###autoload
(add-hook 'tablegen-mode-hook #'tablegen-mode-setup)

(provide 'llvm-setup)

;; Local Variables:
;; End:

;; llvm-setup.el ends here
