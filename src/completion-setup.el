;; completion-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago (since august inception)
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)

(setf read-buffer-completion-ignore-case t)

(defun completing-read-buffer (prompt &optional default require-match)
  (completing-read prompt
                   (map #'buffer-name (visible-buffers))
                   nil
                   require-match
                   nil
                   nil
                   default))

;; tweak vanilla emacs-23 completion to make
;; it more powerful
(setf completion-styles '(partial-completion)
      completion-category-overrides '()
      read-buffer-function #'completing-read-buffer)

;; convenient command completer

(setf smex-history-length 100
      smex-save-file (concat +prog-data-path+ "/smex-items")
      smex-auto-update (not (platform-use? 'work))
      smex-flex-matching t)

(autoload 'smex "smex" "" t)
(autoload 'smex-major-mode-commands "smex" "" t)

(setf ido-enable-flex-matching t
      ido-create-new-buffer 'always ;; don't prompt when opening nonexistent files
      ido-file-extensions-order '(".hs" ".lhs" ".y" ".l" ".cabal" ".h" ".c" t)
      ido-case-fold t ;; ignore case
      ido-everywhere t
      ido-ignore-extensions t ;; ignore extensions in `completion-ignored-extensions'
      ido-enable-last-directory-history nil
      ido-save-directory-list-file nil
      ido-show-dot-for-dired t
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window
      ido-enable-dot-prefix t
      ido-ignore-buffers (list (rx "*"
                                   (or "Ibuffer"
                                       "Compile-Log"
                                       "Ido Completions")
                                   "*")
                               "\\` ")
      ido-decorations '(" ("
                        ")"
                        " | "
                        " | ..."
                        "["
                        "]"
                        " [No match]"
                        " [Matched]"
                        " [Not readable]"
                        " [Too big]"
                        " [Confirm]"))

(ido-mode 1)

(defun ido-setup-custom-bindings ()
  (def-keys-for-map ido-common-completion-map
    ("C-SPC"      ido-edit-input)
    ("C-<return>" ido-select-text)
    ("<C-return>" ido-select-text)
    ("<up>"       previous-history-element)
    ("<down>"     next-history-element))
  (def-keys-for-map (ido-file-completion-map
                     ido-file-dir-completion-map)
    ("C-w"        ido-up-directory)))

(add-hook 'ido-setup-hook #'ido-setup-custom-bindings)

;;;

;; (defun ido-setup ()
;;   ;; bind keys to ido-completion-map here, etc
;;   (def-keys-for-map (;; ido-common-completion-map
;;                      ;;ido-completion-map
;;                      ido-file-completion-map
;;                      )
;;     ("C-w" ido-up-directory))
;;
;;   (def-keys-for-map ido-completion-map
;;     ("<f6>" ido-toggle-regexp)))

;; (add-hook 'ido-setup-hook #'ido-setup)

(provide 'completion-setup)

;; completion-setup.el ends here
