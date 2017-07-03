;; set-up-environment.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  2 July 2017
;; Description:

(require 'set-up-platform)
(require 'set-up-environment-variables)
(require 'set-up-paths)
(unless noninteractive
  (require 'set-up-font))

(provide 'set-up-enivronment)

;; Local Variables:
;; End:

;; set-up-environment.el ends here
