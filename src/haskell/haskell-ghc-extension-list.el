;; haskell-ghc-extension-list.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 17 January 2020
;; Description:

(defvar haskell-language-extensions nil
  "List of Haskell extensions for current GHC in the PATH.

See http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/flag-reference.html
for more information.")

(defvar haskell-language-extensions--initialized? nil
  "Whether `haskell-language-extensions' has been properly initialized.")

;;;###autoload
(defun get-haskell-language-extensions ()
  ;; make this list from documentation, e.g.
  ;; http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/flag-reference.html
  ;; command: '<,'>s/^-X\([^\t]+\)\t\([^\t]+\)\t[^\t]+\t-\(?:X\(.*\)\)?/("\1" "\2" "\3")/
  (unless haskell-language-extensions--initialized?
    (if-let (ghc-exec (cached-executable-find "ghc"))
      (with-temp-buffer
        (call-process ghc-exec
                      nil
                      (current-buffer)
                      nil
                      "--supported-extensions")
        (setf haskell-language-extensions
              (sort
               (split-string (buffer-substring-no-properties (point-min) (point-max))
                             "[\n\r]+"
                             t
                             "[ \t]+")
               #'string<)
              haskell-language-extensions--initialized? t)

        ;; ;; Propagate info to `attrap.el'
        ;; (setf attrap-haskell-extensions haskell-language-extensions)
        )
      (error "Cannot find ghc in PATH - refusing to proceed")))
  haskell-language-extensions)


(provide 'haskell-ghc-etxension-list)

;; Local Variables:
;; End:

;; haskell-ghc-extension-list.el ends here
