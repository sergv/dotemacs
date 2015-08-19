;; hideshow-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 19 August 2015
;; Description:

(autoload 'hs-show-sexps-in-region "hideshow-setup" nil t)
(autoload 'hs-hide-sexps-in-region "hideshow-setup" nil t)
(autoload 'hs-show-c-sexps-in-region "hideshow-setup" nil t)
(autoload 'hs-hide-c-sexps-in-region "hideshow-setup" nil t)

(eval-after-load "hideshow" '(require 'hideshow-setup))

(provide 'hideshow-autoload)

;; Local Variables:
;; End:

;; hideshow-autoload.el ends here
