;; yasnippet-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 18 May 2013
;; Description:

(autoload 'yas-global-mode "yasnippet" "" t)
(autoload 'yas-minor-mode "yasnippet" "" t)
(autoload 'yas-minor-mode-on "yasnippet" "" t)
(autoload 'yas-expand "yasnippet" "" t)
(autoload 'yas-load-directory "yasnippet" "" t)
(autoload 'yas-insert-snippet "yasnippet" "" t)
(autoload 'yas-visit-snippet-file "yasnippet" "" t)
(autoload 'yas-new-snippet "yasnippet" "" t)
(autoload 'yas-load-snippet-buffer "yasnippet" "" t)
(autoload 'yas-tryout-snippet "yasnippet" "" t)
(autoload 'yas-describe-tables "yasnippet" "" t)

(eval-after-load "yasnippet"
  '(progn
     (require 'yasnippet-setup)))

(provide 'yasnippet-autoload)

;; Local Variables:
;; End:

;; yasnippet-autoload.el ends here
