;; magit-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 26 April 2012
;; Description:

(add-to-list 'load-path (concat +emacs-standalone-path+
                                "/magit-1.1.1"))

(require 'vim-mock)
(require 'magit)

(setf magit-completing-read-function
      (lambda (prompt collection
               &optional predicate require-match initial-input hist def)
        (completing-read-vanilla
         (if (and def (> (length prompt) 2)
                  (string-equal ": " (substring prompt -2)))
           (format "%s (default %s): " (substring prompt 0 -2) def)
           prompt)
         collection predicate require-match initial-input hist def)))

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

(defadvice magit-log-edit-cleanup
    (before
     magit-log-edit-cleanup-clean-trailing-whitespace
     activate
     compile)
     (delete-trailing-whitespace+))

(defun magit-mode-setup ()
  ;; here we do have vim mode enabled

  ;; (def-keys-for-map magit-mode-map
  ;;   +control-x-prefix+)
  ;; (def-keys-for-map magit-mode-map
  ;;   +vim-special-keys+)
  (def-keys-for-map magit-mode-map
    ("r"        magit-refresh)
    ("R"        magit-refresh-all)))

(add-hook 'magit-mode-hook #'magit-mode-setup)

(defun magit-status-mode-setup ()
  ;; don't do (init-common) here since it's not so common mode

  (def-keys-for-map magit-status-mode-map
    +control-x-prefix+
    +vim-special-keys+
    ("r"        magit-refresh)
    ("R"        magit-refresh-all)

    ("p"        magit-key-mode-popup-stashing)
    ("<down>"   magit-goto-next-section)
    ("<up>"     magit-goto-previous-section)
    ("t"        magit-goto-next-section)
    ("n"        magit-goto-previous-section)))

(add-hook 'magit-status-mode-hook #'magit-status-mode-setup)

(defun magit-log-mode-setup ()
  (def-keys-for-map magit-log-mode-map
    +control-x-prefix+
    +vim-special-keys+
    ("r"      magit-refresh)
    ("R"      magit-refresh-all)

    ("p"      nil)
    ("<down>" magit-goto-next-section)
    ("<up>"   magit-goto-previous-section)
    ("t"      vim-mock:motion-down)
    ("n"      vim-mock:motion-up)))

(add-hook 'magit-log-mode-hook #'magit-log-mode-setup)

(defun magit-log-edit-mode-setup ()
  (init-common :use-yasnippet nil :use-comment nil)

  (define-key magit-log-edit-mode-map
      (kbd "C-c C-q")
    'magit-log-edit-cancel-log-message))

(add-hook 'magit-log-edit-mode-hook #'magit-log-edit-mode-setup)

(defun magit-show-branches-mode-setup ()
  (def-keys-for-map magit-show-branches-mode-map
    +control-x-prefix+
    +vim-special-keys+
    ("r"      magit-refresh)
    ("R"      magit-refresh-all)

    ("p"      nil)
    ("<down>" magit-goto-next-section)
    ("<up>"   magit-goto-previous-section)
    ("t"      vim-mock:motion-down)
    ("n"      vim-mock:motion-up)))

(add-hook 'magit-show-branches-mode-hook #'magit-show-branches-mode-setup)

;; this mode has no hook
(eval-after-load
 "magit-key-mode"
 '(progn
   (redefun magit-key-mode-build-keymap (for-group)
     "Construct a normal looking keymap for the key mode to use and
put it in magit-key-mode-key-maps for fast lookup."
     (let* ((options (magit-key-mode-options-for-group for-group))
            (actions (cdr (assoc 'actions options)))
            (switches (cdr (assoc 'switches options)))
            (arguments (cdr (assoc 'arguments options)))
            (map (make-sparse-keymap)))
       (suppress-keymap map 'nodigits)
       ;; ret dwim
       (define-key map (kbd "RET") 'magit-key-mode-exec-at-point)

       ;; all maps should `quit' with `C-g' or `q' or `ESC'
       (define-key map (kbd "ESC") `(lambda ()
                                      (interactive)
                                      (magit-key-mode-command nil)))
       (define-key map (kbd "<escape>") `(lambda ()
                                           (interactive)
                                           (magit-key-mode-command nil)))
       (define-key map (kbd "C-g") `(lambda ()
                                      (interactive)
                                      (magit-key-mode-command nil)))
       (define-key map (kbd "q")   `(lambda ()
                                      (interactive)
                                      (magit-key-mode-command nil)))
       ;; run help
       (define-key map (kbd "?") `(lambda ()
                                    (interactive)
                                    (magit-key-mode-help ',for-group)))

       (flet ((defkey (k action)
                (when (and (lookup-key map (car k))
                           (not (numberp (lookup-key map (car k)))))
                  (message "Warning: overriding binding for `%s' in %S"
                           (car k) for-group)
                  (ding)
                  (sit-for 2))
                (define-key map (car k)
                  `(lambda () (interactive) ,action))))
         (when actions
           (dolist (k actions)
             (defkey k `(magit-key-mode-command ',(nth 2 k)))))
         (when switches
           (dolist (k switches)
             (defkey k `(magit-key-mode-add-option ',for-group ,(nth 2 k)))))
         (when arguments
           (dolist (k arguments)
             (defkey k `(magit-key-mode-add-argument
                         ',for-group ,(nth 2 k) ',(nth 3 k))))))

       (aput 'magit-key-mode-key-maps for-group map)
       map))))


(provide 'magit-setup)

;; Local Variables:
;; End:

;; magit-setup.el ends here
