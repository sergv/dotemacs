;; buffer-switching.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  1 September 2018
;; Description:

(require 'common)
(require 'hydra-setup)
(require 'eproj)

(defvar eshell-buffer-name)
(defvar this-command)

(add-to-list 'ivy-sort-matches-functions-alist
             '(switch-to-buffer-with-completion . ivy--flx-sort))
(add-to-list 'ivy-re-builders-alist
             '(switch-to-buffer-with-completion . ivy--regex-fuzzy))

;;;###autoload
(defun switch-to-buffer-with-completion (include-all-buffers?)
  "Switch to another buffer, query user for a buffer to switch to.."
  (interactive "P")
  (let* ((this-command 'switch-to-buffer-with-completion)
         (ivy-use-ignore (not include-all-buffers?))
         (buffers (if include-all-buffers?
                      (buffer-list)
                    (-filter (lambda (buf)
                               (not (--any (string-match-p it (buffer-name buf)) ivy-ignore-buffers)))
                             (visible-buffers))))
         (buffers-with-files (-filter #'buffer-file-name buffers))
         (buffer-names
          (--map (cons (buffer-name it) it)
                 buffers))
         (buffer-files
          (--map (cons (buffer-file-name it) it)
                 (if ivy-use-ignore
                     (-filter (lambda (buf)
                                (not (--any (string-match-p it (buffer-name buf)) ivy-ignore-buffers)))
                              buffers-with-files)
                   buffers-with-files)))
         (all-candidates (append buffer-names buffer-files)))
    (ivy-read (if include-all-buffers? "Switch to buffer (no filter): " "Switch to buffer (only visible): ")
              (lambda (string predicate flag)
                (cond
                  ((eq flag t)
                   (all-completions string all-candidates predicate))
                  ((eq flag nil)
                   (try-completion string all-candidates predicate))
                  (t
                   (test-completion string all-candidates predicate))))
              :matcher #'ivy--switch-buffer-matcher
              :preselect (buffer-name (other-buffer (current-buffer)))
              :action (lambda (str)
                        (switch-to-buffer-create-if-missing
                         (aif (assoc str all-candidates)
                             (cdr it)
                           str)))
              :keymap ivy-switch-buffer-map
              :caller 'switch-to-buffer-with-completion)))

;;;###autoload
(defun switch-to-buffer-create-if-missing (buffer-name)
  (interactive)
  (let ((buf (get-buffer buffer-name)))
    (cond
      (buf (switch-to-buffer buf))
      ((string-match-p (rx bol
                           (? "*")
                           "sh"
                           (or (seq "ell" (* any))
                               (seq (? (seq (or "-"
                                                (char (?0 . ?9)))
                                            (* any)))))
                           eol)
                       buffer-name)
       (let ((buf (get-buffer-create buffer-name)))
         (switch-to-buffer buf nil t)
         (shell buf)))
      ((string-match-p (rx bol
                           (? "*")
                           "esh"
                           (or (seq "ell" (* any))
                               (seq (? (seq (or "-"
                                                (char (?0 . ?9)))
                                            (* any)))))
                           eol)
                       buffer-name)
       (let* ((buf (get-buffer-create buffer-name))
              (eshell-buffer-name buf))
         ;; Eshell will switch to new buffer automatically,
         ;; no need to do it ourselves.
         (eshell)))
      (t
       (switch-to-buffer (get-buffer-create buffer-name) t)))))

;;;###autoload
(defun switch-to-buffer-or-file-in-current-project (&optional include-all-buffers?)
  "Like `switch-to-buffer' but includes files from eproj project assigned to
current buffer."
  (interactive "P")
  (if-let ((proj (eproj-get-project-for-buf-lax (current-buffer))))
      (eproj-switch-to-file-or-buffer proj nil include-all-buffers?)
    (error "No project for current buffer: %s" (current-buffer))))

;;;###autoload
(defun switch-to-buffer-or-file-in-current-or-related-projects (&optional include-all-buffers?)
  "Like `switch-to-buffer' but includes files from eproj project assigned to
current buffer."
  (interactive "P")
  (let ((proj (eproj-get-project-for-buf-lax (current-buffer))))
    (if proj
        (eproj-switch-to-file-or-buffer proj t include-all-buffers?)
      (error "No project for current buffer: %s" (current-buffer)))))

;;;###autoload (autoload 'hydra-switching/body "buffer-switching" nil t)
(defhydra-ext hydra-switching (:exit t :foreign-keys warn :hint nil)
  "
Switch to:
_b_uffer
_f_ile
buffer/file in _c_urrent project
buffer/file in current or _r_elated projects"
  ("c" switch-to-buffer-or-file-in-current-project)
  ("r" switch-to-buffer-or-file-in-current-or-related-projects)
  ("b" switch-to-buffer-with-completion)
  ("f" counsel-find-file))

(provide 'buffer-switching)

;; Local Variables:
;; End:

;; buffer-switching.el ends here
