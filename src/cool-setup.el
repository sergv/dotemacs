;; cool-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  8 May 2014
;; Description:

(autoload 'cool-mode "cool-mode"
          ""
          t)

(add-to-list 'auto-mode-alist
             (cons (rx (or ".cl"
                           ".cool")
                       eol)
                   'cool-mode))

(provide 'cool-setup)

;; Local Variables:
;; End:

;; cool-setup.el ends here
