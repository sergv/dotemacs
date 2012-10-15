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


(icy-mode 1)

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
  ;; Remap some commands for completion.
  (icicle-remap 'self-insert-command           'icicle-self-insert map (current-global-map))
  (icicle-remap 'universal-argument            'icicle-universal-argument
                map (current-global-map))
  (icicle-remap 'negative-argument             'icicle-negative-argument
                map (current-global-map))
  (icicle-remap 'digit-argument                'icicle-digit-argument
                map (current-global-map))
  (icicle-remap 'backward-delete-char-untabify 'icicle-backward-delete-char-untabify
                map (current-global-map))
  (icicle-remap 'delete-backward-char          'icicle-delete-backward-char
                map (current-global-map))
  (icicle-remap 'delete-char                   'icicle-delete-char
                map (current-global-map))
  (icicle-remap 'backward-kill-word            'icicle-backward-kill-word
                map (current-global-map))
  (icicle-remap 'kill-word                     'icicle-kill-word
                map (current-global-map))
  (icicle-remap 'backward-kill-sexp            'icicle-backward-kill-sexp
                map (current-global-map))
  (icicle-remap 'kill-sexp                     'icicle-kill-sexp
                map (current-global-map))
  (icicle-remap 'backward-kill-sentence        'icicle-backward-kill-sentence
                map (current-global-map))
  (icicle-remap 'backward-kill-paragraph       'icicle-backward-kill-paragraph
                map (current-global-map))
  (icicle-remap 'kill-paragraph                'icicle-kill-paragraph
                map (current-global-map))
  (icicle-remap 'kill-line                     'icicle-kill-line
                map (current-global-map))
  (icicle-remap 'reposition-window             'icicle-goto/kill-failed-input
                map (current-global-map))
  (icicle-remap 'transpose-chars               'icicle-transpose-chars
                map (current-global-map))
  (icicle-remap 'transpose-words               'icicle-transpose-words
                map (current-global-map))
  (icicle-remap 'transpose-sexps               'icicle-transpose-sexps
                map (current-global-map))
  (icicle-remap 'yank-pop                      'icicle-yank-pop
                map (current-global-map))
  (icicle-remap 'mouse-yank-secondary          'icicle-mouse-yank-secondary
                map (current-global-map))

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
    (when (fboundp 'yank-secondary)     ; In `second-sel.el'.
      (def-keys-for-map map
        ("C-M-y" 'icicle-yank-secondary)))
    (olist (key  icicle-completing-read+insert-keys)
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

  (icicles-util-bind-my-keys map))


(defun icicles-util-bind-my-keys (map)
  "Utility function that binds my custom keys and is used in several places."
  (def-keys-for-map map
    ("<escape>"      abort-recursive-edit)

    ("C-w"           backward-delete-word)
    ("C-S-w"         backward-delete-word*)
    ("C-p"           vim:cmd-paste-before)
    ("M-p"           browse-kill-ring)

    ("C-v"           set-mark-command)
    ("C-y"           copy-region-as-kill)
    ("C-d"           kill-region)
    ("C-f"           icicle-read+insert-file-name)

    ("M-<tab>"       icicle-narrow-candidates)
    ("C-<tab>"       icicle-apropos-complete-and-narrow)

    ("<delete>"      delete-char)
    ("<home>"        beginning-of-line)
    ("<end>"         end-of-line)
    ("<next>"        icicle-next-prefix-candidate)
    ("<prior>"       icicle-previous-prefix-candidate)
    ("S-<next>"      icicle-next-apropos-candidate)
    ("S-<prior>"     icicle-previous-apropos-candidate)

    ("C-n"           icicle-next-TAB-completion-method)
    ("C-p"           icicle-narrow-candidates-with-predicate)

    ("S-M-<tab>"         icicle-candidate-set-complement)
    ("<M-S-iso-lefttab>" icicle-candidate-set-complement)

    ("M-SPC"         toggle-icicle-hiding-common-match)
    ("C-M-SPC"       icicle-retrieve-previous-input)
    ("C-SPC"         icicle-erase-minibuffer)

    ("<backspace>"   delete-backward-char)
    ("S-<backspace>" backward-delete-word)))

;; (fset 'read-shell-command 'icicle-read-shell-command-completing)

;; (defadvice:icicle-on-sole-completion icicle-read-shell-command-completing)

(defadvice:icicle-do-not-insert-default-value icicle-file)
(defadvice:icicle-do-not-insert-default-value icicle-locate-file)
(defadvice:icicle-do-not-insert-default-value describe-function)
(defadvice:icicle-do-not-insert-default-value describe-variable)
(defadvice:icicle-do-not-insert-default-value icicle-buffer)


(setf icicle-TAB-completion-methods '(vanilla
                                      swank
                                      ;; fuzzy
                                      )

      icicle-S-TAB-completion-methods-alist
      '(("apropos" . string-match))

      ;; tweak vanilla emacs-23 completion to make
      ;; it more powerful

      icicle-dot-string "." ;; icicle-anychar-regexp
      icicle-reverse-sort-p t)


(icicle-set-S-TAB-methods-for-command 'icicle-file
                                      icicle-S-TAB-completion-methods-alist)
(icicle-set-S-TAB-methods-for-command 'icicle-locate-file
                                      icicle-S-TAB-completion-methods-alist)
(icicle-set-S-TAB-methods-for-command 'icicle-locate-file-other-window
                                      icicle-S-TAB-completion-methods-alist)


(dolist (map (list minibuffer-local-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-filename-completion-map
                   minibuffer-local-filename-must-match-map
                   minibuffer-local-isearch-map
                   icicle-read-expression-map))
  (icicles-util-bind-my-keys map))

(def-keys-for-map icicle-read-expression-map
  ("M-/" lisp-complete-symbol))



(def-keys-for-map completion-list-mode-map
  +control-x-prefix+
  +vim-special-keys+
  ("<up>"     previous-completion)
  ("<down>"   next-completion)
  ("<escape>" remove-buffer))

(defun completion-list-setup ()
  (setq autopair-dont-activate t))

(add-hook 'completion-list-mode-hook #'completion-list-setup)


(provide 'icicles-setup)

;; Local Variables:
;; End:

;; icicles-setup.el ends here
