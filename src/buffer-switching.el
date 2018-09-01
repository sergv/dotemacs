;; buffer-switching.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  1 September 2018
;; Description:

(require 'common)
(require 'eproj)

(add-to-list 'ivy-sort-matches-functions-alist
             '(switch-to-buffer-with-completion . ivy-sort-function-buffer))

;;;###autoload
(defun switch-to-buffer-with-completion ()
  "Switch to another buffer, query user for a buffer to switch to.."
  (interactive)
  (let ((this-command 'switch-to-buffer-with-completion))
    (ivy-read "Switch to buffer: " 'internal-complete-buffer
              :matcher #'ivy--switch-buffer-matcher
              :preselect (buffer-name (other-buffer (current-buffer)))
              :action #'switch-to-buffer-create-if-missing
              :keymap ivy-switch-buffer-map
              :caller 'switch-to-buffer-with-completion)))

;;;###autoload
(defun switch-to-buffer-create-if-missing (buffer-name)
  (interactive)
  (let ((buf (get-buffer buffer-name)))
    (cond
      (buf (switch-to-buffer buf))
      ((string-match-p "^\\*shell.*$" buffer-name)
       (let ((shell-buf (get-buffer-create buffer-name)))
         (switch-to-buffer shell-buf nil t)
         (shell shell-buf)))
      (t
       (switch-to-buffer (get-buffer-create buffer-name) t)))))

;;;###autoload
(defun switch-to-buffer-or-file-in-current-project (&optional include-all-buffers?)
  "Like `switch-to-buffer' but includes files from eproj project assigned to
current buffer."
  (interactive "P")
  (let ((proj (eproj-get-project-for-buf-lax (current-buffer))))
    (if proj
        (eproj-switch-to-file-or-buffer proj include-all-buffers?)
      (progn
        (message "No project for current buffer: %s" (current-buffer))
        (call-interactively #'switch-to-buffer-with-completion)))))

(provide 'buffer-switching)

;; Local Variables:
;; End:

;; buffer-switching.el ends here
