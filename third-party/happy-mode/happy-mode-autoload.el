;; happy-mode-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 27 May 2014
;; Description:

(require 'mmm-auto)

(mmm-add-classes
 '((haskell-blocks
    :submode haskell-mode
    ;; :face font-lock-function-name-face ;; mmm-output-submode-face
    :front "{%?"
    :front-verify haskell-blocks-verify-front
    :back haskell-blocks-find-back
    :back-verify haskell-blocks-verify-back
    ;;:include-front t
    ;;:front-offset 5
    )))

(autoload 'happy-mode "happy-mode" nil t)
(autoload 'haskell-blocks-verify-front "happy-mode" nil nil)
(autoload 'haskell-blocks-find-back "happy-mode" nil nil)
(autoload 'haskell-blocks-verify-back "happy-mode" nil nil)

(provide 'happy-mode-autoload)

;; Local Variables:
;; End:

;; happy-mode-autoload.el ends here
