;; scheme-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  8 December 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'general-lisp-setup)
(require 'slime-setup)
(require 'scheme-highlight)
(require 'scheme-abbrev+)
(require 'comint-setup)
(require 'more-scheme)

(define-common-lisp-style "scheme"
  "Custom indent style for scheme."
  (:inherit "modern")
  (:variables
   (lisp-indent-maximum-backtracking 10)
   (lisp-align-keywords-in-calls t)
   (lisp-loop-indent-subclauses nil)
   (lisp-lambda-list-keyword-parameter-indentation 0)

   (indent-tabs-mode nil)
   (comment-fill-column nil))

  (:indentation
   (if             (4 4 4))
   (define         (nil &body))
   (define-macro   (as define))
   (define-syntax  (as define))
   (define-method  (as define))
   (define-generic (as define))
   (module         (nil nil 0))
   (syntax-rules   (as define))))


(require 'scheme-complete)

(defun scheme-setup ()
  (lisp-setup)
  (scheme-highlight)
  (common-lisp-set-style "scheme")

  (slime-mode 1)
  (set (make-local-variable 'slime-default-lisp)
       'chicken)

  (set (make-local-variable 'lisp-indent-function)
       #'common-lisp-indent-function)

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:visual-mode-local-keymap)
    ("*" search-for-slime-symbol-at-point-forward)
    ("#" search-for-slime-symbol-at-point-backward))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("j"       slime-eval-last-expression)
    ("J"       slime-pprint-eval-last-expression)

    ("SPC SPC" switch-to-slime)
    ("S"       slime-selector)

    ("M-."     slime-edit-definition)
    ("M-,"     slime-pop-find-definition-stack)

    (", m"     slime-macroexpand-1)
    (", M"     slime-macroexpand-all)
    (", d"     slime-describe-symbol)
    (", i"     slime-inspect))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    ("<f1>"    scheme-load-current-file)
    ("<f9>"    common-lisp-compile-and-load-file)

    ("M-/"     slime-complete-symbol)
    ("M-:"     slime-interactive-eval))

  (def-keys-for-map vim:visual-mode-local-keymap
    (", m"     slime-macroexpand-1)
    (", M"     slime-macroexpand-all))

  (scheme-abbrev+-setup))

;; define scheme-load-current-file
(if (and (platform-os-type? 'linux)
         (platform-use? 'asus-netbook))
  (defun scheme-load-current-file (&optional switch)
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
      (switch-to-slime)))

  (defun scheme-load-current-file (&optional noswitch)
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

(provide 'scheme-setup)

;; Local Variables:
;; End:

;; scheme-setup.el ends here
