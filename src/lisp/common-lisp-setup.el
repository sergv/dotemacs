;;; common-lisp-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  8 December 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'keys-def)

(require 'general-lisp-setup)
(require 'slime-setup)
(require 'ansi-lisp-highlight)
(require 'common-lisp-abbrev+)
(require 'browse-kill-ring-setup)
;; (require 'hyperspec)

(autoload 'cltl2-lookup "cltl2" nil t)



(defadvice cldoc-print-current-symbol-info
    (around
     cldoc-print-current-symbol-info-disable-in-comment-and-string
     activate
     compile)
  (multiple-value-bind (insidep start)
      (lisp-position-inside-string-or-comment)
    (unless insidep
      ad-do-it)))

;; (define-switch-to-interpreter
;;     switch-to-messages
;;   ("*Messages*")
;;   (ignore)
;;   :doc "Pop to messages buffer."
;;   :save-buffer nil
;;   :error-msg "Can't switch to *Messages*"
;;   :try-count 1)

(defun common-lisp-setup ()
  (lisp-setup)
  (common-lisp-set-style "my-style")

  (slime-mode 1)
  (turn-on-cldoc-mode)

  (def-keys-for-map2 (vim:normal-mode-local-keymap
                      vim:visual-mode-local-keymap)
    ("*" search-for-slime-symbol-at-point-forward)
    ("#" search-for-slime-symbol-at-point-backward))

  (def-keys-for-map2 vim:normal-mode-local-keymap
    ("j"       slime-eval-last-expression)
    ("J"       slime-pprint-eval-last-expression)

    ("SPC SPC" switch-to-slime)
    ("S"       slime-selector)

    ("M-."     slime-edit-definition)
    ("M-,"     slime-pop-find-definition-stack)

    (", m"     slime-macroexpand-1)
    (", M"     slime-macroexpand-all)
    (", d"     slime-describe-symbol)
    (", i"     slime-inspect)
    (", h"     slime-hyperspec-lookup)
    (", c"     cltl2-lookup))

  (def-keys-for-map2 (vim:normal-mode-local-keymap
                      vim:insert-mode-local-keymap)
    ("<f1>"  common-lisp-load-file)
    ("<f9>"  common-lisp-compile-and-load-file)

    ("M-/"   slime-complete-symbol)
    ("M-:"   slime-interactive-eval))

  (def-keys-for-map2 vim:visual-mode-local-keymap
    (", m"     slime-macroexpand-1)
    (", M"     slime-macroexpand-all))

  (common-lisp-abbrev+-setup))

(cond
  ((eq? +platform+ 'netbook-linux)
   (defun common-lisp-load-file (&optional switch)
     "Load buffers' current file into SLIME. If buffer happens to have no file
then it's content will be evaluated by SLIME."
     (interactive "P")
     (write-region (point-min) (point-max) *cl-tmp-file*)
     ;; suppres write-region's message
     (message "")
     (slime-load-file *cl-tmp-file*)
     (when switch
       (switch-to-slime))))
  ((eq? +platform+ 'asus-netbook)
   (defun common-lisp-load-file (&optional noswitch)
     "Load buffers' current file into SLIME. If buffer happens to have no file
then it's content will be evaluated by SLIME."
     (interactive "P")
     ;; if buffer has file
     (if (buffer-file-name)
       (progn
         (when (buffer-modified-p)
           (save-buffer))
         (slime-load-file (buffer-file-name)))
       (slime-eval-buffer))

     (when switch
       (switch-to-slime))))
  ((eq? +platform+ 'home-linux)
   (defun common-lisp-load-file (&optional switch)
     "Load buffers' current file into SLIME. If buffer happens to have no file
then it's content will be evaluated by SLIME."
     (interactive "P")
     ;; if buffer has file
     (if (buffer-file-name)
       (progn
         (when (buffer-modified-p)
           (save-buffer))
         (slime-load-file (buffer-file-name)))
       (slime-eval-buffer))

     (unless noswitch
       (switch-to-slime))))
  (else
   (error "invalid +platform+: %s" +platform+)))


(defun common-lisp-compile-and-load-file (&optional policy)
  "This functions saves current buffer and calls `slime-compile-and-load-file'."
  (interactive "P")
  (labels ((switch-to-compilation-results (tries)
             (cond
               ((= tries 0)
                (error "Can't switch to compilation buffer"))
               ;; slime repl buffer, one of extensions
               ((buffer-live-p (get-buffer "*slime-compilation*"))
                (pop-to-buffer (get-buffer "*slime-compilation*") t))
               (t
                (sleep-for 1)
                (switch-to-compilation-results (1- tries))))))
    (unless (eq +platform+ 'netbook-linux)
      (when (buffer-modified-p)
        (save-buffer)))
    (slime-compile-and-load-file policy)
    ;; switch only if there're any notes
    (if (slime-compiler-notes)
      (switch-to-compilation-results 100)
      (message "No notes produced."))))


;;;; ********************************


(provide 'common-lisp-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; common-lisp-setup.el ends here
