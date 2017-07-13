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


(vim:defcmd vim:flycheck-run (nonrepeatable)
  (flycheck-buffer))

(vim:defcmd vim:flycheck-compile (nonrepeatable)
  (call-interactively #'flycheck-compile))

(vim:defcmd vim:flycheck-clear (nonrepeatable)
  (flycheck-clear
   t ;; interrupt running process
   ))

(vim:defcmd vim:flycheck-list-errors (nonrepeatable)
  (flycheck-list-errors))

;;;###autoload
(defun* flycheck-install-ex-commands! (&key compile-func load-func)
  (vim:local-emap "ff" #'vim:flycheck-compile)
  (dolist (cmd '("check" "ch"))
    (vim:local-emap cmd #'vim:flycheck-run))
  (dolist (cmd '("clear" "reset"))
    (vim:local-emap cmd #'vim:flycheck-clear))
  (dolist (cmd '("errors" "errs"))
    (vim:local-emap cmd #'vim:flycheck-list-errors))
  (when compile-func
    (dolist (cmd '("c" "compile"))
      (vim:local-emap cmd compile-func)))
  (when load-func
    (dolist (cmd '("load" "lo" "l"))
      (vim:local-emap cmd load-func))))

(defun flycheck-enhancements--goto-error-with-wraparound (forward?)
  (interactive)
  (let* ((direction-code (if forward? 1 -1))
         (wrapped? nil)
         (error-position
          (let ((pos (flycheck-next-error-pos direction-code nil)))
            (if pos
                pos
              (save-excursion
                (goto-char (if forward?
                               (point-min)
                             (point-max)))
                (setf wrapped? t)
                (flycheck-next-error-pos direction-code nil))))))
    (unless error-position
      (error "No more errors"))
    (when wrapped?
      (message "Wrapped at the %s of buffer"
               (if forward?
                   "end"
                 "beginning")))
    (goto-char error-position)
    (flycheck-display-error-at-point)))

(defun flycheck-enhancements-previous-error-with-wraparound ()
  (interactive)
  (flycheck-enhancements--goto-error-with-wraparound nil))

(defun flycheck-enhancements-next-error-with-wraparound ()
  (interactive)
  (flycheck-enhancements--goto-error-with-wraparound t))

(provide 'flycheck-setup)

;; Local Variables:
;; End:

;; flycheck-setup.el ends here
