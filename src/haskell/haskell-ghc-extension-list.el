;; haskell-ghc-extension-list.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 17 January 2020
;; Description:

;;;###autoload (autoload 'get-haskell-language-extensions "haskell-ghc-extension-list" nil t)
(defun-once get-haskell-language-extensions
  ;; Make this list from documentation, e.g.
  ;; http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/flag-reference.html
  ;; command: '<,'>s/^-X\([^\t]+\)\t\([^\t]+\)\t[^\t]+\t-\(?:X\(.*\)\)?/("\1" "\2" "\3")/
  (if-let (ghc-exec (cached-executable-find "ghc"))
      (with-temp-buffer
        (call-process ghc-exec
                      nil
                      (current-buffer)
                      nil
                      "--supported-extensions")
        (sort
         (split-string (buffer-substring-no-properties (point-min) (point-max))
                       "[\n\r]+"
                       t
                       "[ \t]+")
         #'string<))
    (error "Cannot find ghc in PATH - refusing to proceed")))


(provide 'haskell-ghc-extension-list)

;; Local Variables:
;; End:

;; haskell-ghc-extension-list.el ends here
