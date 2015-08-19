;; dired-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 19 August 2015
;; Description:


(autoload 'dired-single-buffer "dired-single" "" t)

(eval-after-load "dired" '(require 'dired-setup))

(provide 'dired-autoload)

;; Local Variables:
;; End:

;; dired-autoload.el ends here
