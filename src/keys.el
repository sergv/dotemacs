;; keys.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago (almost since the august inception)
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'common)
(require 'keys-def)

(def-keys-for-map help-map
  ("h" nil))

(def-keys-for-map global-map
  ;; Unbind so that I don’t accidetally invoke tutorial, news or other such nonsense.
  ("C-h"      nil)

  ("<f1>"     help-command)
  ("<f1> t"   nil)
  ("<f2>"     universal-argument)

  ("<f5>"     revert-buffer)
  ("<f6>"     revert-buffer)

  ("<f9>"     compile)
  ("<f12>"    start-profiler-or-report)

  ("C-<prior>" scroll-down-command-fast)
  ("C-<next>"  scroll-up-command-fast)

  ;; use abort, don't exit here
  ("<escape>" abort-recursive-edit)
  ("C-!"      shell-command)
  ("C-|"      shell-command-on-region)
  ("M-!"      nil)
  ("M-|"      nil)
  ("C-v"      nil)
  ("C-r"      nil)
  ("C-v"      set-mark-command)
  ("C-y"      copy-region-as-kill)
  ("C-f"      read-and-insert-filename)
  ("C-S-p"    browse-kill-ring)
  ("C-w"      backward-delete-word)
  ("C-S-w"    backward-delete-word*)
  ("C-s"      save-buffer)

  ("C-t"      nil)
  ("C-<down>" nil)
  ("C-<up>"   nil)
  ("C-b"      hydra-switching/body)
  ("M-b"      switch-to-prev-buffer-in-window)

  ;; for icicle
  ("<M-tab>"  nil)
  ("M-<tab>"  nil)
  ("M-TAB"    nil)
  ("C-SPC"    nil)
  ("M-SPC"    nil)
  ("M-."      nil)
  ("C-/"      nil)
  ("M-/"      nil)
  ("M-f"      nil)
  ("M-m"      nil)
  ("M-M"      nil)

  ("M-x"      counsel-M-x)

  ("C-x <f1>" nil)
  ("C-z"      nil)
  ("C-x f"    find-file)
  ("C-k"      nil)
  ("C-S-k"    nil)
  ("<print>"  render-formula-toggle-formulae)

  ("<right>"  forward-char)
  ("<left>"   backward-char)

  ("C-M-k"    remove-buffer)

  ("<mouse-3>" mouse-open-file-at-point-other-window)

  ("C-<home>" nil)
  ("C-<end>"  nil)
  ("C-<tab>"  nil)

  (("DEL" "<backspace>") pseudoparedit-backspace)
  ("\""                  pseudoparedit-insert-double-quote)
  ("\("                  pseudoparedit-insert-paren)
  ("\)"                  smart-operators-close-paren)
  ("\["                  pseudoparedit-insert-bracket)
  ("\]"                  smart-operators-close-bracket)
  ("\{"                  pseudoparedit-insert-brace)
  ("\}"                  smart-operators-close-brace)

  ("C-<tab>"                         indent-relative-forward)
  (("C-S-<tab>" "C-S-<iso-lefttab>") indent-relative-backward)

  ;; It’s bound to ‘keyboard-escape-quit’ by default which I never use.
  ("\e\e\e" nil))

(def-keys-for-map universal-argument-map
  ("<f2>" universal-argument-more))

(defun help-mode-init ()
  (def-keys-for-map help-mode-map
    +vi-keys+
    +vim-search-keys+
    +vim-search-extended-keys+
    +vim-word-motion-keys+
    +vim-special-keys+
    ("SPC"      help-follow)
    ("<up>"     help-go-back)
    ("<down>"   help-go-forward)
    ("C-."      elisp-slime-nav-find-elisp-thing-at-point)
    ("C-,"      pop-tag-mark)
    ("v"        set-mark-command)
    ("y"        copy-region-as-kill)))

(eval-after-load "help-mode"
  '(help-mode-init))

;; view-mode
(eval-after-load "view"
  '(progn
     (def-keys-for-map view-mode-map
       +vi-keys+)))

(eval-after-load "cus-edit"
  '(progn
     (def-keys-for-map custom-mode-map
       +vi-keys+
       +vim-special-keys+
       +vim-word-motion-keys+
       ("- w"      customize-save-customized)
       ("w"        vim:motion-fwd-word:interactive)
       ("b"        vim:motion-bwd-word:interactive)
       ("e"        vim:motion-fwd-word-end:interactive)

       ("<down>"   widget-forward)
       ("<up>"     widget-backward))

     (def-keys-for-map Custom-mode-map
       +vi-keys+
       +vim-special-keys+
       +vim-word-motion-keys+
       ("- w"      customize-save-customized)
       ("w"        vim:motion-fwd-word:interactive)
       ("b"        vim:motion-bwd-word:interactive)
       ("e"        vim:motion-fwd-word-end:interactive)

       ("<down>"   widget-forward)
       ("<up>"     widget-backward))))

(def-keys-for-map completion-list-mode-map
  ("<escape>" quit-window))

(def-keys-for-map occur-mode-map
  (("t" "<up>")   custom-occur-prev)
  (("h" "<down>") custom-occur-next)
  ("q"            remove-buffer))

(eval-after-load "doc-view"
  '(progn
     (def-keys-for-map doc-view-mode-map
       ("S-<up>"   doc-view-previous-page)
       ("S-<down>" doc-view-next-page))))

(defun define-cyrillic-keys ()
  ;; just all choices from set, 2**n, n - sets' power
  (letrec ((choices (lambda (list)
                      (cond
                        ((null list)
                         '(nil))
                        (t
                         (let ((p (funcall choices (cdr list))))
                           (append p
                                   (--map (cons (car list) it) p))))))))
    (let ((modifiers (remove nil
                             (funcall choices '(control meta super hyper)))))
      (-zip-with
       (lambda (r e) ;; R and E are matching Russian and English keysyms
         ;; iterate over modifiers
         (dolist (mods modifiers)
           (define-key input-decode-map
             (vector (append mods (list r))) (vector (append mods (list e)))))
         ;; finally, if Russian key maps nowhere,
         ;; remap it to the English key without any modifiers
         ;; note: local-function-key-map may come in handy
         (define-key function-key-map (vector r) (vector e)))
       (string->list
        "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ")
       (string->list
        "',.pyfgcrl/=aoeuidhtns-;qjkxbmwv\"<>PYFGCRL?+AOEUIDHTNS_:QJKXBMWV")
       ;; "йцукенгшщзхъфывапролджэячсмитьбю"
       ;; "qwertyuiop[]asdfghjkl;'zxcvbnm,."
       ))))

(define-cyrillic-keys)

(def-keys-for-map completion-list-mode-map
  ("<up>"   previous-completion)
  ("<down>" next-completion))

(def-keys-for-map read-expression-map
  ("<tab>"   completion-at-point)
  ("C-w"     backward-delete-word)
  ("C-S-w"   backward-delete-word*))

(def-keys-for-map read-regexp-map
  ("M-h" next-complete-history-element)
  ("M-t" previous-complete-history-element))

(def-keys-for-map (minibuffer-mode-map
                   ivy-minibuffer-map)
  ("C-'"  typopunct-insert-single-quotation-mark)
  ("C-\"" typopunct-insert-quotation-mark))

(provide 'keys)

;; Local Variables:
;; End:

;; keys.el ends here
