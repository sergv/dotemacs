;; icicles-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Keywords:
;; Requirements:
;; Status:

;; Huh, now (Thu Dec 15 18:26:37 FET 2011) icicles proved itself completely
;; inadequate as vehicle for carrying out completions for me -
;; I cannot withstand it's source code due to awful style, 100 column width, etc.
;; and unfortunately modifications to source are neccessary in order to
;; make it work as desired, especially after hacking in swank completion


(require 'advices-util)
(require 'icicles)
(require 'el-swank-fuzzy)
(require 'keys-def)
(require 'common)
(require 'icicles-util)


(eval-when-compile
  (defmacro icicle-setup/icicle-do-not-insert-default-value (func)
    `(defadvice ,func (around
                       ,(util/make-joined-name func "-do-not-insert-default-value")
                       activate
                       compile)
       (let ((icicle-default-value nil))
         ad-do-it))))


(redefun icicle-fuzzy-candidates (input)
  "Return fuzzy matches for INPUT.  Handles also swank fuzzy symbol match."
  (condition-case err
      (let ((candidates ()))
        (cond ((vectorp minibuffer-completion-table)
               (mapatoms (lambda (symb) (when (or (null minibuffer-completion-predicate)
                                                  (funcall minibuffer-completion-predicate symb))
                                          (push (symbol-name symb) candidates)))
                         minibuffer-completion-table))

              ((and (consp minibuffer-completion-table)
                    (consp (car minibuffer-completion-table)))
               (dolist (cand minibuffer-completion-table)
                 (when (or (null minibuffer-completion-predicate)
                           (funcall minibuffer-completion-predicate cand))
                   (push (car cand) candidates)))))
        (when candidates
          (setf candidates
                (el-swank-fuzzy-matches input
                                        candidates
                                        :timeout icicle-swank-timeout
                                        :prefix-length icicle-swank-prefix-length)))

        (let ((icicle-extra-candidates
               (icicle-remove-if-not
                (lambda (cand) (string-match-p input cand)) icicle-extra-candidates))
              (icicle-proxy-candidates
               (icicle-remove-if-not
                (lambda (cand) (string-match-p input cand)) icicle-proxy-candidates))
              (filtered-candidates
               (icicle-transform-candidates
                (append icicle-extra-candidates icicle-proxy-candidates
                        (icicle-remove-if-not
                         (lambda (cand)
                           (let ((case-fold-search
                                  completion-ignore-case))
                             (and (icicle-filter-wo-input cand)
                                  (or (not icicle-must-pass-after-match-predicate)
                                      (funcall icicle-must-pass-after-match-predicate cand)))))
                         candidates)))))
          (when (consp filtered-candidates)
            (setq icicle-common-match-string  (icicle-expanded-common-match input filtered-candidates)))
          (unless filtered-candidates  (setq icicle-common-match-string  nil))
          filtered-candidates))
    ((debug error)
     (message "ERROR: %S %S" (car err) (cdr err))
     (quit (top-level)))
    (quit (top-level))))

(redefun icicle-bind-completion-keys (map)
  "Bind keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map' or
`minibuffer-local-must-match-map'."
  ;; protect this map
  (when (or (not (boundp 'vim:ex-keymap))
            (not (eq? map vim:ex-keymap)))
    ;; Remap some commands for completion.
    (dolist (entry '((self-insert-command           icicle-self-insert)
                     (universal-argument            icicle-universal-argument)
                     (negative-argument             icicle-negative-argument)
                     (digit-argument                icicle-digit-argument)
                     (backward-delete-char-untabify icicle-backward-delete-char-untabify)
                     (delete-backward-char          icicle-delete-backward-char)
                     (delete-char                   icicle-delete-char)
                     (backward-kill-word            icicle-backward-kill-word)
                     (kill-word                     icicle-kill-word)
                     (backward-kill-sexp            icicle-backward-kill-sexp)
                     (kill-sexp                     icicle-kill-sexp)
                     (backward-kill-sentence        icicle-backward-kill-sentence)
                     (backward-kill-paragraph       icicle-backward-kill-paragraph)
                     (kill-paragraph                icicle-kill-paragraph)
                     (kill-line                     icicle-kill-line)
                     (reposition-window             icicle-goto/kill-failed-input)
                     (transpose-chars               icicle-transpose-chars)
                     (transpose-words               icicle-transpose-words)
                     (transpose-sexps               icicle-transpose-sexps)
                     (yank-pop                      icicle-yank-pop)
                     (mouse-yank-secondary          icicle-mouse-yank-secondary)))
      (destructuring-bind (old new) entry
        (icicle-remap old new map (current-global-map))))

    ;; Bind additional keys.
    (dolist (key  icicle-candidate-action-keys)
      (define-key map key 'icicle-candidate-action))
    (dolist (key  icicle-candidate-help-keys)
      (define-key map key 'icicle-help-on-candidate))

    (dolist (key  icicle-word-completion-keys)
      (define-key map key 'icicle-prefix-word-complete))
    (dolist (key  icicle-apropos-complete-keys)
      (define-key map key 'icicle-apropos-complete))
    (dolist (key  icicle-prefix-complete-keys) (define-key map key 'icicle-prefix-complete))
    (dolist (key  icicle-apropos-complete-no-display-keys)
      (define-key map key 'icicle-apropos-complete-no-display))
    (dolist (key  icicle-prefix-complete-no-display-keys)
      (define-key map key 'icicle-prefix-complete-no-display))

    (icicle-define-cycling-keys map)

    (def-keys-for-map map
      ("M-<return>"   icicle-candidate-read-fn-invoke)
      ("C-M-m"        icicle-candidate-read-fn-invoke)
      ("C-S-<return>" icicle-candidate-alt-action)
      ("C-w"          backward-delete-word)
      ("C-S-w"        backward-delete-word*)
      ("C-!"          icicle-all-candidates-action)
      ("C-|"          icicle-all-candidates-alt-action)
      ("M-!"          icicle-all-candidates-list-action)
      ("M-|"          icicle-all-candidates-list-alt-action)
      ("M-h"          icicle-history)
      ("C-<insert>"   icicle-switch-to-Completions-buf)
      ("<insert>"     icicle-save/unsave-candidate))

    ;; In Emacs 22+, local is parent of local-completion
    (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
      (def-keys-for-map map
        ("C-M-v"           icicle-scroll-forward)
        ("C-M-S-v"         icicle-scroll-backward)
        ("C-="             icicle-insert-string-from-variable)
        ("M-i"             icicle-clear-current-history)
        ("M-k"             icicle-erase-minibuffer-or-history-element)
        ("M-."             icicle-insert-string-at-point)
        ("M-:"             icicle-pp-eval-expression-in-minibuffer))
      (when (fboundp 'yank-secondary)   ; In `second-sel.el'.
        (def-keys-for-map map
          ("C-M-y" 'icicle-yank-secondary)))
      (dolist (key icicle-completing-read+insert-keys)
        (define-key map key 'icicle-completing-read+insert)))

    ;; Need `C-g', even if `minibuffer-local-completion-map' inherits from `minibuffer-local-map'.
    (def-keys-for-map map
      ("C-,"   icicle-change-sort-order)
      ("M-,"   icicle-reverse-sort-order)
      ("C-g"   icicle-abort-recursive-edit)
      ("M-q"   icicle-dispatch-M-q)
      ("M-$"   icicle-candidate-set-truncate)
      ("C-~"   icicle-candidate-set-complement)
      ("C--"   icicle-candidate-set-difference)
      ("C-+"   icicle-candidate-set-union)
      ("C-*"   icicle-candidate-set-intersection)
      ("C->"   icicle-candidate-set-save-more)
      ("C-M->" icicle-candidate-set-save)
      ("C-("   icicle-next-TAB-completion-method)
      ("M-("   icicle-next-S-TAB-completion-method))
    (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
      (define-key map (kbd "C-?")     'icicle-minibuffer-help))
    (def-keys-for-map map
      ("C-#"           icicle-toggle-incremental-completion)
      ("M-~"           icicle-toggle-~-for-home-dir)
      ("C-M-~"         icicle-toggle-search-complementing-domain)
      ("C-M-,"         icicle-toggle-alternative-sorting)
      ("M-+"           icicle-widen-candidates)
      ("M-&"           icicle-narrow-candidates-with-predicate)
      ("S-<backspace>" icicle-apropos-complete-and-widen)
      ("."             icicle-insert-dot-command)
      ("M-m"           icicle-toggle-show-multi-completion))
    (when (fboundp 'icicle-cycle-image-file-thumbnail) ; Emacs 23+
      (define-key map (kbd "C-x t")   'icicle-cycle-image-file-thumbnail))

    (define-key map (kbd "?")         'icicle-self-insert)
    (define-key map (kbd "SPC")       'icicle-self-insert)
    ;; In Emacs 22+, local is parent of local-completion
    (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
      (def-keys-for-map map
        ("C-j" 'icicle-insert-newline-in-minibuffer)))

    (icicles-util/bind-minibuffer-keys map :sexp-keys nil)))

;; important: call icy-mode only after necessary functions were redefined
(icy-mode +1)


;; (fset 'read-shell-command 'icicle-read-shell-command-completing)

;; (defadvice:icicle-on-sole-completion icicle-read-shell-command-completing)

(icicle-setup/icicle-do-not-insert-default-value icicle-file)
(icicle-setup/icicle-do-not-insert-default-value icicle-locate-file)
(icicle-setup/icicle-do-not-insert-default-value describe-function)
(icicle-setup/icicle-do-not-insert-default-value describe-variable)
(icicle-setup/icicle-do-not-insert-default-value icicle-buffer)


(setf icicle-TAB-completion-methods '(vanilla
                                      ;; swank
                                      ;; fuzzy
                                      )

      icicle-S-TAB-completion-methods-alist
      '(("apropos" . string-match)
        ;; ("scatter" . icicle-scatter-match)
        )

      icicle-dot-string "." ;; icicle-anychar-regexp
      icicle-reverse-sort-p nil)


(icicle-set-S-TAB-methods-for-command 'icicle-file
                                      icicle-S-TAB-completion-methods-alist)
(icicle-set-S-TAB-methods-for-command 'icicle-locate-file
                                      icicle-S-TAB-completion-methods-alist)
(icicle-set-S-TAB-methods-for-command 'icicle-locate-file-other-window
                                      icicle-S-TAB-completion-methods-alist)


(dolist (map (list minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-filename-completion-map
                   minibuffer-local-filename-must-match-map
                   minibuffer-local-isearch-map))
  (icicles-util/bind-minibuffer-keys map :sexp-keys nil))
(icicles-util/bind-minibuffer-keys minibuffer-local-map :sexp-keys t)
(icicles-util/bind-minibuffer-keys icicle-read-expression-map :sexp-keys t)

(def-keys-for-map icicle-read-expression-map
  ("M-/" lisp-complete-symbol))



(def-keys-for-map completion-list-mode-map
  +control-x-prefix+
  +vim-special-keys+
  ("<up>"     previous-completion)
  ("<down>"   next-completion)
  ("<escape>" remove-buffer))

(defun completion-list-setup ()
  ;; empty for now
  )


(provide 'icicles-setup)

;; Local Variables:
;; End:

;; icicles-setup.el ends here
