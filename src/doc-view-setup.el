;; doc-view-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 25 August 2012
;; Description:

(require 'persistent-store)

(eval-after-load
    "doc-view"
  '(progn
     ;; don't bind nor vi-keys nor vim's word-motion keys here as the're useless
     ;; when navigating pdfs
     (def-keys-for-map doc-view-mode-map
       +control-x-prefix+
       +vim-special-keys+
       ("d" image-backward-hscroll)
       ("h" doc-view-next-line-or-next-page)
       ("t" doc-view-previous-line-or-previous-page)
       ("n" image-forward-hscroll)
       ("p" nil))))

(autoload 'doc-view-current-page "doc-view" "" nil 'macro)
(defun doc-view-save-page ()
  (if-buffer-has-file
    (let ((fname (file-name-nondirectory (buffer-file-name))))
      (persistent-store-put
       'doc-view-documents
       (cons (cons fname
                   (doc-view-current-page))
             (filter (lambda (entry)
                       (not (string=? fname
                                      (car entry))))
                     (persistent-store-get 'doc-view-documents
                                           nil)))))))

(defun doc-view-setup ()
  (setf mode-line-format
        '(" %[%b%] "
          ;; if buffer has assigned file and is modified
          ("("
           mode-name
           mode-line-process
           ")")
          " "
          (:eval
           (when vc-mode
             (concat vc-mode " ")))
          " "
          (:eval (number-to-string (doc-view-current-page)))
          "/"
          (:eval (number-to-string (doc-view-last-page-number)))))
  (if-buffer-has-file
    (awhen (assoc (file-name-nondirectory (buffer-file-name))
                  (persistent-store-get 'doc-view-documents nil))
      (doc-view-goto-page (cdr it))))

  (add-hook 'kill-buffer-hook 'doc-view-save-page nil t))
;; this hook actually exists in doc-view.el, albeit undeclared
(add-hook 'doc-view-mode-hook 'doc-view-setup)

(defun doc-view-save-pages-on-kill ()
  (for-buffers-with-mode 'doc-view-mode
                         (lambda (buf)
                           (with-current-buffer buf
                             (doc-view-save-page)))))

(add-hook 'kill-emacs-hook 'doc-view-save-pages-on-kill)

(provide 'doc-view-setup)

;; Local Variables:
;; End:

;; doc-view-setup.el ends here
