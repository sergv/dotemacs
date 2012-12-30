;; c-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'cc-setup)
(require 'c-abbrev+)

(defun c-indent-buffer ()
  (interactive)
  (unless (executable-find "astyle")
    (error "Command astyle is not available"))
  (let ((file (make-temp-file "c-indent")))
    (write-region (point-min) (point-max) file)
    (erase-buffer)
    (shell-command
     (mapconcat #'identity
                (list "astyle"
                      "--style=linux"
                      "--indent=spaces=8"
                      "--brackets=linux"
                      "--pad-oper"
                      "--pad-header"
                      "--unpad-paren"
                      "--keep-one-line-statements"
                      "--keep-one-line-blocks"
                      "--convert-tabs"
                      "--align-pointer=name"
                      "--mode=c"
                      "--suffix=none"
                      "--lineend=linux"
                      (format "<%s" file))
                " ")
     (current-buffer))))

(push (cons 'c-mode #'c-indent-buffer) *mode-buffer-indent-function-alist*)

(defun c-hideshow-forward-sexp (&optional arg)
  "Special version of `forward-sexp' for hideshow in c-mode."
  (if (looking-at-pure? "{")
    (forward-sexp arg)
    (let ((on-start? (looking-at-pure? (rx (seq "#"
                                                (* (syntax whitespace))
                                                symbol-start
                                                (or "ifdef"
                                                    "ifndef"
                                                    "if")
                                                symbol-end)))))
      (c-forward-conditional (or arg 1))
      (when on-start?
        (forward-char -1)))))

(setf hs-special-modes-alist
      (cons `(c-mode ,(rx (or (seq "#"
                                   (* (syntax whitespace))
                                   symbol-start
                                   (or "ifdef"
                                       "ifndef"
                                       "if"
                                       "endif"
                                       "else")
                                   symbol-end)
                              "{"))
                     nil
                     "/[*/]"
                     c-hideshow-forward-sexp
                     nil)
            (remove* 'c-mode
                     hs-special-modes-alist
                     :key #'car
                     :test #'eq?)))



(defun c-setup ()
  (cc-setup :define-special-keys t)

  (if-buffer-has-file
   (setq-local compile-command
               (let* ((fname  (file-name-nondirectory buffer-file-name))
                      (target (file-name-sans-extension fname)))
                 (setq compile-command
                       (mapconcat #'identity
                                  (list "gcc"
                                        "-W"
                                        "-Wall"
                                        "-O2"
                                        "-I."
                                        "-o"
                                        target
                                        fname)
                                  " ")))))

  (if-has-makefile-command
   (setq-local compile-command
               (concat "make " (file-name-sans-extension
                                (file-name-nondirectory buffer-file-name)))))

  (def-keys-for-map vim:visual-mode-local-keymap
    (", m" c-macro-expand))
  (c-abbrev+-setup))

(provide 'c-setup)

;; Local Variables:
;; End:

;; c-setup.el ends here
