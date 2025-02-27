;; minibuffer-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 22 January 2022
;; Description:

(eval-when-compile
  (require 'macro-util))

(def-keys-for-map (minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-filename-completion-map
                   minibuffer-local-map
                   minibuffer-local-isearch-map
                   minibuffer-inactive-mode-map)
  ("<escape>"          abort-recursive-edit)

  ("C-w"               backward-delete-word)
  ("C-S-w"             backward-delete-word*)
  ("C-p"               vim-cmd-paste-after-no-adjust)
  ("C-S-p"             browse-kill-ring)

  ("C-/"               nil)
  ("C-v"               set-mark-command)
  ("C-y"               copy-region-as-kill)
  ("C-d"               kill-region)
  ("C-f"               read-and-insert-filename)

  ("<delete>"          delete-char)
  ("<home>"            beginning-of-line)
  ("<end>"             end-of-line)

  ("C-SPC"             delete-minibuffer-contents)
  ;; ("SPC"               self-insert-command)

  ("S-<delete>"        delete-whitespace-forward)
  ("S-<backspace>"     delete-whitespace-backward)
  ("<backspace>"       delete-backward-char))

;; (def-keys-for-map minibuffer-inactive-mode-map
;;   ("?" self-insert-command))

(def-keys-for-map (minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-filename-completion-map)
  ("SPC" self-insert-command)
  ("?"   self-insert-command))

(def-keys-for-map minibuffer-local-map
  +vim-parens-keys+
  ;; In sexp-related situations the "''" is quite rare.
  ("'" self-insert-command))

(def-keys-for-map read--expression-map
  ("C-h" next-history-element)
  ("C-t" previous-history-element))

(provide 'minibuffer-setup)

;; Local Variables:
;; End:

;; minibuffer-setup.el ends here
