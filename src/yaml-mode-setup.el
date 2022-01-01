;; yaml-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 20 April 2012
;; Description:

(eval-when-compile
  (require 'macro-util))

;;;###autoload
(autoload 'yaml-mode "yaml-mode.el" "Simple mode to edit YAML." t)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.clang-format$" . yaml-mode))

;;;###autoload
(defun yaml-setup ()
  (init-common :use-render-formula nil)
  (bind-tab-keys #'indent-relative-forward
                 #'indent-relative-backward
                 :enable-yasnippet t)
  (when-buffer-has-file
    (when (string-match-p "^stack.*\\.yaml$"
                          (file-name-nondirectory (buffer-file-name)))
      (vim:local-emap "compile"  #'vim:haskell-compile)
      (vim:local-emap "c"        #'vim:haskell-compile)
      (vim:local-emap "ccompile" #'vim:haskell-compile-choosing-command)
      (vim:local-emap "cc"       #'vim:haskell-compile-choosing-command)

      (def-keys-for-map (vim:normal-mode-local-keymap
                         vim:insert-mode-local-keymap)
        (("C-m" "<f9>") haskell-start-compilation)))))

;;;###autoload
(add-hook 'yaml-mode-hook #'yaml-setup)

(provide 'yaml-mode-setup)

;; Local Variables:
;; End:

;; yaml-mode-setup.el ends here
