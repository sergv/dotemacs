;; fakecygpty-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 18 December 2021
;; Description:

(eval-when-compile
  (require 'set-up-platform))

(when-windows
 (require 'fakecygpty)
 (fakecygpty-activate))

(provide 'fakecygpty-setup)

;; Local Variables:
;; End:

;; fakecygpty-setup.el ends here
