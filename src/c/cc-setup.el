;; cc-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday,  8 October 2012
;; Description:

(require 'custom)

(require 'c-eldoc)
(require 'ctypes)
(require 'find-file)
(require 'etags)

(require 'dtrt-indent) ;; indent offset guesser

(setf dtrt-indent-verbosity 2
      dtrt-indent-max-relevant-lines 10000)

(make-align-function c-align-on-equals
                     "\\([+*|&/!%]\\|-\\|\\^\\)?=[^=]"
                     :require-one-or-more-spaces t)

(defun c-hideshow-forward-sexp (&optional arg)
  "Special version of `forward-sexp' for hideshow in c-mode."
  (if (char=? (char-after) ?\{)
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

(let ((hs-spec (list (rx (or (seq "#"
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
                     #'c-hideshow-forward-sexp
                     nil)))
  (setf hs-special-modes-alist
        (cons `(c-mode ,hs-spec)
              (cons `(c++-mode ,hs-spec)
                    (remove-if (lambda (entry)
                                 (memq (car entry) '(c-mode c++-mode)))
                               hs-special-modes-alist)))))



(defun* cc-setup (&key (define-special-keys t)
                       (use-c-eldoc (not (platform-use? 'work))))
  (init-common :use-render-formula t)
  (autopair-mode 1)
  (hs-minor-mode 1)
  (dtrt-indent-mode 1)
  (which-function-mode -1)
  (when use-c-eldoc
    (c-turn-on-eldoc-mode)
    (set (make-variable-buffer-local 'eldoc-idle-delay) 0.2))

  (modify-syntax-entry ?_ "_")

  (setf whitespace-line-column 80
        whitespace-style '(face tabs lines-tail)
        ;; affects only tab display
        tab-width 4)

  (let ((dtrt-indent-verbosity 0))
    (unless (dtrt-indent-try-set-offset)
      (when (platform-use? 'work)
        (set (make-variable-buffer-local 'c-basic-offset) 4))

      (set (make-variable-buffer-local 'vim:shift-width)
           (cond ((integer? c-basic-offset) c-basic-offset)
                 ((platform-use? 'work)     4)
                 (t                         8)))))


  (setf c-tab-always-indent t)
  (c-toggle-hungry-state 1)
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)

  (setq vim:normal-mode-local-keymap           (make-keymap)
        vim:insert-mode-local-keymap           (make-sparse-keymap)
        vim:visual-mode-local-keymap           (make-sparse-keymap)
        vim:operator-pending-mode-local-keymap (make-sparse-keymap))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("g TAB" c-indent-defun)

    ("g t"   c-end-of-defun)
    ("g n"   c-beginning-of-defun)

    ("z o"   hs-show-block)
    ("z v"   hs-show-block)
    ("z c"   hs-hide-block)
    ("z C"   hs-hide-all)
    ("z O"   hs-show-all)

    ("M-."   find-tag)
    ("M-,"   pop-tag-mark))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("g a a" align)
    ("g a =" c-align-on-equals)
    ("g t"   c-end-of-defun)
    ("g n"   c-beginning-of-defun))

  (when define-special-keys
    (def-keys-for-map vim:normal-mode-local-keymap
      ("SPC SPC" ff-find-related-file)))
  (when use-c-eldoc
    (def-keys-for-map vim:normal-mode-local-keymap
      (", ?"     c-eldoc-show-current-symbol-declaration))))

(provide 'cc-setup)

;; Local Variables:
;; End:

;; cc-setup.el ends here
