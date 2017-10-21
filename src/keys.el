;; keys.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago (almost since the august inception)
;; Description:


(require 'common)
(require 'keys-def)

(def-keys-for-map global-map
  ("<f1>"     help-command)
  ("<f1> t"   nil)
  ("<f2>"     universal-argument)

  ("<f5>"     revert-buffer)
  ("<f6>"     nil)

  ("<f9>"     compile)
  ("<f10>"    shell)

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
  ("M-p"      browse-kill-ring)
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
  ("C-k"      nil)
  ("C-S-k"    nil)
  ("<print>"  render-formula-toggle-formulae)

  ("<right>"  forward-char)
  ("<left>"   backward-char))

(def-keys-for-map universal-argument-map
  ("<f2>" universal-argument-more))


(eval-after-load "help-mode"
  '(progn
     (def-keys-for-map help-mode-map
       +vi-keys+
       +vim-search-keys+
       +vim-search-extended-keys+
       +vim-mock:word-motion-keys+
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
       +vim-mock:word-motion-keys+
       ("- w"      customize-save-customized)
       ("w"        vim:motion-fwd-word)
       ("b"        vim:motion-bwd-word)
       ("e"        vim:motion-fwd-word-end)

       ("<down>"   widget-forward)
       ("<up>"     widget-backward))

     (def-keys-for-map Custom-mode-map
       +vi-keys+
       +vim-special-keys+
       +vim-mock:word-motion-keys+
       ("- w"      customize-save-customized)
       ("w"        vim:motion-fwd-word)
       ("b"        vim:motion-bwd-word)
       ("e"        vim:motion-fwd-word-end)

       ("<down>"   widget-forward)
       ("<up>"     widget-backward))))

(def-keys-for-map completion-list-mode-map
  ("<escape>" quit-window))

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

(def-keys-for-map (minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-filename-completion-map
                   minibuffer-local-filename-must-match-map
                   minibuffer-local-map
                   minibuffer-local-isearch-map
                   minibuffer-inactive-mode-map)
  ("<escape>"          abort-recursive-edit)
  ;; ("?"                 self-insert-command)

  ("C-w"               backward-delete-word)
  ("C-S-w"             backward-delete-word*)
  ("C-p"               vim:cmd-paste-before)
  ("M-p"               browse-kill-ring)

  ("C-/"               nil)
  ("C-v"               set-mark-command)
  ("C-y"               copy-region-as-kill)
  ("C-d"               kill-region)
  ("C-f"               read-and-insert-filename)

  ("M-<tab>"           icicle-narrow-candidates)
  ("C-<tab>"           icicle-apropos-complete-and-narrow)

  ("<delete>"          delete-char)
  ("<home>"            beginning-of-line)
  ("<end>"             end-of-line)
  ;; ("<next>"            icicle-next-prefix-candidate)
  ;; ("<prior>"           icicle-previous-prefix-candidate)

  ("C-SPC"             delete-minibuffer-contents)
  ;; ("SPC"               self-insert-command)

  ("S-<delete>"        delete-whitespace-forward)
  ("S-<backspace>"     delete-whitespace-backward)
  ("<backspace>"       delete-backward-char))

;; (def-keys-for-map minibuffer-inactive-mode-map
;;   ("?" self-insert-command))

(def-keys-for-map (minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-filename-completion-map
                   minibuffer-local-filename-must-match-map)
  ("SPC" self-insert-command)
  ("?"   self-insert-command))

(def-keys-for-map (minibuffer-local-map)
  ;; in sexp-related situations "''" is quite rare pair
  ("'"         self-insert-command)
  ("M-<up>"    sp-splice-sexp-killing-backward)
  ("M-<down>"  sp-splice-sexp-killing-forward)
  ("C-)"       sp-forward-slurp-sexp)
  ("C-<right>" sp-forward-slurp-sexp)
  ("C-<left>"  sp-forward-barf-sexp)
  ("C-("       sp-backward-slurp-sexp)
  ("M-<left>"  sp-backward-slurp-sexp)
  ("M-<right>" sp-backward-barf-sexp))

(def-keys-for-map (completion-list-mode-map)
  ("<up>"   previous-completion)
  ("<down>" next-completion))

(provide 'keys)

;; Local Variables:
;; End:

;; keys.el ends here
