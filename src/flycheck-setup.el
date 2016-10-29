;; flycheck-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 May 2016
;; Description:

(eval-when-compile
  (require 'let-alist))

(provide 'flycheck-setup)

;;;###autoload
(eval-after-load "flycheck" '(require 'flycheck-setup))

;;;###autoload
(autoload 'flycheck-haskell-setup "flycheck-haskell" nil nil)

(eval-after-load "flycheck"
  '(progn
     ;; Don't show errors on fringes.
     (setf flycheck-indication-mode nil
           ;; Highlight whole line with error
           flycheck-highlighting-mode 'lines
           flycheck-completing-read-function #'ido-completing-read
           flycheck-display-errors-delay 0)))

(defun flycheck-pretty-mode-line ()
  (when (and (boundp 'flycheck-mode)
             flycheck-mode
             (boundp 'flycheck-last-status-change))
    (pcase flycheck-last-status-change
      (`not-checked "unchecked")
      (`no-checker  "no-checker")
      (`running     "running")
      (`errored     "error while checking")
      (`finished
       (let-alist (flycheck-count-errors flycheck-current-errors)
         (when (or .error .warning .info)
           (format "e %d/w %d/i %d" (or .error 0) (or .warning 0) (or .info 0)))))
      (`interrupted "interrupted")
      (`suspicious  "suspicious output from checker")
      (_ "unknown"))))

;;;###autoload
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

;;;###autoload
(defun flycheck-error-list-setup ()
  (def-keys-for-map flycheck-error-list-mode-map
    +vim-search-keys+
    +vim-special-keys+
    (("h" "<down>") flycheck-error-list-next-error)
    (("t" "<up>")   flycheck-error-list-previous-error)
    ("H"            flycheck-error-list-check-source)
    ("<escape>"     quit-window)))

;;;###autoload
(add-hook 'flycheck-error-list-mode-hook #'flycheck-error-list-setup)

(defadvice flycheck-display-error-messages
    (before
     flycheck-display-error-messages-set-mode-in-error-buffer
     activate
     compile)
  (unless (buffer-live-p flycheck-error-message-buffer)
    (with-current-buffer (get-buffer-create flycheck-error-message-buffer)
      (text-mode))))

;; Local Variables:
;; End:

;; flycheck-setup.el ends here
