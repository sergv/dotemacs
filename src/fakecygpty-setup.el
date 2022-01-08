;; fakecygpty-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 18 December 2021
;; Description:

(require 'fakecygpty)

(when-windows
 (fakecygpty-activate))

(provide 'fakecygpty-setup)

;; Local Variables:
;; End:

;; fakecygpty-setup.el ends here
