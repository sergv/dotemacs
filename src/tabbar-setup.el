;; tabbar-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 21 October 2021
;; Description:

(defsubst tabbar--at-least-2-elements? (x)
  (cddr x))

;;;###autoload
(defun next-tab-or-frame (arg)
  (interactive "p")
  (if (tabbar--at-least-2-elements? (tab-bar-tabs))
      (tab-next arg)
    (next-f arg))

;;;###autoload
(defun prev-tab-or-frame (arg)
  (interactive "p")
  (if (tabbar--at-least-2-elements? (tab-bar-tabs))
      (tab-previous arg)
    (prev-f arg))

(provide 'tabbar-setup)

;; Local Variables:
;; End:

;; tabbar-setup.el ends here
