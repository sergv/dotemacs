;; magit-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 26 April 2012
;; Description:

(add-to-list 'load-path (concat +emacs-standalone-path+
                                "/magit-1.1.1"))

(require 'vim-mock)
(require 'magit)

;;;; gitignore

(autoload 'gitignore-mode "gitignore-mode"
          "Major mode for editing .gitignore files"
          t)

(add-to-list 'auto-mode-alist
             '("\\.gitignore$" . gitignore-mode))

(defun gitignore-setup ()
  (init-common :use-yasnippet  nil
               :use-comment    t
               :use-whitespace t))

(add-hook 'gitignore-mode-hook
          'gitignore-setup)

;;;; magit

(eval-after-load
 "magit.el"
 '(progn
   (redefun magit-log-edit-cleanup ()
     (delete-trailing-whitespace+)
     (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "[ \t\n]*\\'" nil t)
        (replace-match "\n" nil nil))))))

(defun magit-mode-setup ()
  ;; here we do have vim mode enabled

  ;; (def-keys-for-map1 magit-mode-map
  ;;   +control-x-prefix+)
  ;; (def-keys-for-map1 magit-mode-map
  ;;   +vim-special-keys+)
  (def-keys-for-map2 magit-mode-map
    ("r"        magit-refresh)
    ("R"        magit-refresh-all)))

(add-hook 'magit-mode-hook #'magit-mode-setup)

(defun magit-status-mode-setup ()
  ;; don't do (init-common) here since it's not so common mode

  (def-keys-for-map1 magit-status-mode-map
    +control-x-prefix+)
  (def-keys-for-map1 magit-status-mode-map
    +vim-special-keys+)
  (def-keys-for-map2 magit-status-mode-map
    ("r"        magit-refresh)
    ("R"        magit-refresh-all)

    ("p"        nil)
    ("<down>"   magit-goto-next-section)
    ("<up>"     magit-goto-previous-section)
    ("t"        magit-goto-next-section)
    ("n"        magit-goto-previous-section)
    ;; ("t"        vim-mock:motion-down)
    ;; ("n"        vim-mock:motion-up)
    ))

(add-hook 'magit-status-mode-hook #'magit-status-mode-setup)

(defun magit-log-mode-setup ()
  (magit-status-mode-setup))

(add-hook 'magit-log-mode-hook #'magit-log-mode-setup)

(defun magit-log-edit-mode-setup ()
  (init-common :use-yasnippet nil :use-comment nil)

  (define-key magit-log-edit-mode-map
      (kbd "C-c C-q")
    'magit-log-edit-cancel-log-message))

(add-hook 'magit-log-edit-mode-hook #'magit-log-edit-mode-setup)

(provide 'magit-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;; magit-setup.el ends here
