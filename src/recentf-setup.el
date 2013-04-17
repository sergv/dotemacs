;; recentf-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 17 April 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)

(require 'recentf)
(setf recentf-max-saved-items 100
      recentf-save-file (concat +prog-data-path+ "/recentf")
      recentf-exclude
      (list (concat "^.*"
                    (regexp-opt *ignored-file-name-endings*)
                    "$")))
(recentf-mode +1)

(provide 'recentf-setup)

;; Local Variables:
;; End:

;; recentf-setup.el ends here
