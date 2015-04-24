;; keys.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago (almost since the august inception)
;; Description:


(require 'custom)
(require 'keys-def)

(def-keys-for-map global-map
  ("<f1>"     help-command)
  ("<f1> t"   nil)
  ("<f2>"     universal-argument)
  ("<f3>"     prev-w)
  ("<f4>"     next-w)

  ("<f5>"     revert-buffer)
  ("<f6>"     nil)
  ("<f7>"     prev-f)
  ("<f8>"     next-f)

  ("<f9>"     compile)
  ("<f10>"    shell)

  ("S-<f3>"   swap-buffers-backward)
  ("S-<f4>"   swap-buffers-forward)

  ("S-<f7>"   swap-buffers-forward-through-frame)
  ("S-<f8>"   swap-buffers-backward-through-frames)

  ("C-<f9>"   enlarge-window)
  ("C-<f10>"  shrink-window)
  ("C-<f11>"  enlarge-window-horizontally)
  ("C-<f12>"  shrink-window-horizontally)

  ;; use abort, don't exit here
  ("<escape>" abort-recursive-edit)
  ("M-x"      execute-extended-command)
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

  ("C-t"      nil)
  ("C-<down>" nil)
  ("C-<up>"   nil)
  ("C-b"      ido-switch-buffer)

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

  ("M-x"      smex)
  ("M-X"      smex-major-mode-commands)
  ("C-x <f1>" nil)
  ("C-z"      nil)
  ("C-x f"    find-file)
  ("C-k"      remove-buffer)
  ("C-S-k"    remove-buffer-and-window)
  ("<print>"  render-formula-toggle-formulae)

  ("<right>"  forward-char)
  ("<left>"   backward-char))

(def-keys-for-map universal-argument-map
  ("<f2>" universal-argument-more))


(eval-after-load "help-mode"
  '(progn
     (def-keys-for-map help-mode-map
       +vi-keys+
       +vi-search-keys+
       +vim-word-motion-keys+
       +vim-special-keys+
       ("SPC"      help-follow)
       ("<up>"     help-go-back)
       ("<down>"   help-go-forward)
       ("C-."      elisp-slime-nav-find-elisp-thing-at-point)
       ("C-,"      pop-tag-mark)
       ("v"        set-mark-command)
       ("y"        copy-region-as-kill))))

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
       ("w"        vim:motion-fwd-word)
       ("b"        vim:motion-bwd-word)
       ("e"        vim:motion-fwd-word-end)

       ("<down>"   widget-forward)
       ("<up>"     widget-backward))

     (def-keys-for-map Custom-mode-map
       +vi-keys+
       +vim-special-keys+
       +vim-word-motion-keys+
       ("- w"      customize-save-customized)
       ("w"        vim:motion-fwd-word)
       ("b"        vim:motion-bwd-word)
       ("e"        vim:motion-fwd-word-end)

       ("<down>"   widget-forward)
       ("<up>"     widget-backward))))


(def-keys-for-map occur-mode-map
  ("<up>"   custom-occur-prev)
  ("<down>" custom-occur-next)
  ("t"      custom-occur-prev)
  ("h"      custom-occur-next)
  ("q"      remove-buffer))

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
                                   (map (lambda (x) (cons (car list) x))
                                        p))))))))
    (let ((modifiers (remove nil
                             (funcall choices '(control meta super hyper)))))
      (mapcar*
       (lambda (r e) ;; R and E are matching Russian and English keysyms
         ;; iterate over modifiers
         (mapc (lambda (mods)
                 (define-key input-decode-map
                   (vector (append mods (list r))) (vector (append mods (list e)))))
               modifiers)
         ;; finally, if Russian key maps nowhere,
         ;; remap it to the English key without any modifiers
         ;; note: local-function-key-map may come in handy
         (define-key function-key-map (vector r) (vector e)))
       "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ"
       "',.pyfgcrl/=aoeuidhtns-;qjkxbmwv\"<>PYFGCRL?+AOEUIDHTNS_:QJKXBMWV"

       ;; "йцукенгшщзхъфывапролджэячсмитьбю"
       ;; "qwertyuiop[]asdfghjkl;'zxcvbnm,."
       ))))

(define-cyrillic-keys)



(provide 'keys)

;; Local Variables:
;; End:

;; keys.el ends here
