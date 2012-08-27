;; doc-view-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 25 August 2012
;; Description:

(require 'persistent-store)

(eval-after-load
 "doc-view"
 '(progn
   (def-keys-for-map1 doc-view-mode-map +control-x-prefix+)
   (def-keys-for-map1 doc-view-mode-map +vim-special-keys+)
   ;; don't bind nor vi-keys nor vim's word-motion keys here as the're useless
   ;; when navigating pdfs
   (def-keys-for-map1 doc-view-mode-map
     (("h" image-backward-hscroll)
      ("t" doc-view-next-line-or-next-page)
      ("n" doc-view-previous-line-or-previous-page)
      ("s" image-forward-hscroll)
      ("p" nil)))))

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
  (if-buffer-has-file
   (aif (assoc (file-name-nondirectory (buffer-file-name))
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
;; lexical-binding: t
;; End:

;; doc-view-setup.el ends here
