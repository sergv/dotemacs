;; calendar-mode-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 11 September 2012
;; Description:


(defun calendar-mode-setup ()
  (def-keys-for-map calendar-mode-map
    +control-x-prefix+
    +vim-special-keys+))

(add-hook 'calendar-mode-hook #'calendar-mode-setup)

(provide 'calendar-mode-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;; calendar-mode-setup.el ends here
