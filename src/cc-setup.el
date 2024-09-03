;; cc-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday,  8 October 2012
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util)
  (defvar whitespace-line-column))

(require 'align-util)
(require 'c-preprocessor)
(require 'common)
(require 'indentation)
(require 'find-file)
(require 'company-eproj)
(require 'vim-setup)

(require 'dtrt-indent) ;; indent offset guesser

(setf dtrt-indent-verbosity 2
      dtrt-indent-max-relevant-lines 10000)

;;;###autoload (autoload 'c-align-on-equals "cc-setup")
(defalign c-align-on-equals
  (rx (? (or (any ?+ ?* ?| ?& ?/ ?! ?% ?- ?^)
             "<<"
             ">>"))
      "="
      (not ?=))
  :require-one-or-more-spaces t)

(defun cc-setup/set-up-c-basic-offset ()
  "Try to guess offset (`c-basic-offset') for current buffer or use value
dictated by code standard at work if use-work-code-style is non-nil.
Also propagate new offset to `vim-shift-width'."
  (let ((dtrt-indent-verbosity 0))
    (unless (dtrt-indent-try-set-offset)
      (awhen (eproj-query/any-mode/indent-offset (eproj-get-project-for-buf-lax (current-buffer))
                                                 major-mode
                                                 nil)
        (setq-local c-basic-offset it)))
    (setup-indent-size
     (if (integerp c-basic-offset)
         c-basic-offset
       4))))


(defhydra-ext hydra-c-align (:exit t :foreign-keys nil :hint nil)
  "
_a_:   generic
_=_: on equals"
  ("a" align)
  ("=" c-align-on-equals))

(defhydra-derive hydra-c-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_<tab>_: reindent function  _t_: jump to function start
                            _h_: jump to function end"
  ("TAB" c-indent-defun)

  ("t"   c-beginning-of-defun)
  ("h"   c-end-of-defun))

(defhydra-derive hydra-c-vim-visual-g-ext hydra-vim-visual-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_a_lign  _t_: jump to function start
       _h_: jump to function end"
  ("a" hydra-c-align/body)

  ("t" c-beginning-of-defun)
  ("h" c-end-of-defun))

(defhydra-derive hydra-c-vim-visual-z-ext hydra-vim-visual-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_c_: hide c sexps in region
_o_: show c sexps in region"
  ("c" hs-hide-c-sexps-in-region)
  ("o" hs-show-c-sexps-in-region))

;;;###autoload
(cl-defun cc-setup (&key (define-special-keys t))
  (init-common :use-render-formula t
               :use-yasnippet t
               :use-whitespace 'tabs-only)
  (which-function-mode -1)
  (company-mode +1)

  (modify-syntax-entry ?_ "_")
  (modify-syntax-entry ?# ".")

  (setq-local company-backends '(company-eproj)
              whitespace-line-column 80
              whitespace-style '(face tabs lines-tail)
              hs-allow-nesting t)

  (setf c-tab-always-indent t)
  (c-toggle-hungry-state 1)
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("g" hydra-c-vim-normal-g-ext/body))

  (def-keys-for-map vim-visual-mode-local-keymap
    ("z" hydra-c-vim-visual-z-ext/body)
    ("g" hydra-c-vim-visual-g-ext/body))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("C-SPC"               company-complete)

    ;; Override c-mode’s electric keybindings.
    (("DEL" "<backspace>") pseudoparedit-backspace)
    ("\""                  pseudoparedit-insert-double-quote)
    ("\("                  pseudoparedit-insert-paren)
    ("\)"                  smart-operators-close-paren)
    ("\["                  pseudoparedit-insert-bracket)
    ("\]"                  smart-operators-close-bracket)
    ("\{"                  pseudoparedit-insert-brace)
    ("\}"                  smart-operators-close-brace))

  (bind-tab-keys #'indent-for-tab-command
                 #'tab-to-tab-stop-backward
                 :enable-yasnippet t)

  (when define-special-keys
    (def-keys-for-map vim-normal-mode-local-keymap
      ("SPC SPC" ff-find-related-file))))

(defun c-format-buffer ()
  (interactive)
  (clang-format-buffer (format "{ IndentWidth: %s }" c-basic-offset)))

(defun c-format-file (filename &optional style)
  "Indent FILENAME according to STYLE by running astyle on it."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((astyle-indent-style (or style
                                   astyle-indent-style)))
      (c-format-buffer)
      (write-region (point-min) (point-max) filename))))

(provide 'cc-setup)

;; Local Variables:
;; End:

;; cc-setup.el ends here


