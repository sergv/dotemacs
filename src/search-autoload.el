;; search-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 19 August 2015
;; Description:

(autoload 'search-start-forward "search" nil t)
(autoload 'search-start-backward "search" nil t)
(autoload 'search-start-forward-new-color "search" nil t)
(autoload 'search-start-backward-new-color "search" nil t)
(autoload 'search-next "search" nil t)
(autoload 'search-prev "search" nil t)
(autoload 'search-return-to-start "search" nil t)
(autoload 'search-abort "search" nil t)
(autoload 'search-done "search" nil t)
(autoload 'search-toggle-highlighting "search" nil t)
(autoload 'search-def-autoexpand-advices "search" nil nil 'macro)
(autoload 'search-for-haskell-symbol-at-point-forward "search" nil t)
(autoload 'search-for-haskell-symbol-at-point-forward-new-color "search" nil t)
(autoload 'search-for-haskell-symbol-at-point-backward "search" nil t)
(autoload 'search-for-haskell-symbol-at-point-backward-new-color "search" nil t)
(autoload 'search-for-symbol-at-point-forward "search" nil t)
(autoload 'search-for-symbol-at-point-forward-new-color "search" nil t)
(autoload 'search-for-symbol-at-point-backward "search" nil t)
(autoload 'search-for-symbol-at-point-backward-new-color "search" nil t)
(autoload 'search-for-word-at-point-forward "search" nil t)
(autoload 'search-for-word-at-point-forward-new-color "search" nil t)
(autoload 'search-for-word-at-point-backward "search" nil t)
(autoload 'search-for-word-at-point-backward-new-color "search" nil t)

(provide 'search-autoload)

;; Local Variables:
;; End:

;; search-autoload.el ends here
