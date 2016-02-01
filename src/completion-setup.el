;; completion-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago (since august inception)
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)

;; Vanilla completion

(setf read-buffer-completion-ignore-case t)

(defun completing-read-buffer (prompt &optional default require-match)
  (completing-read prompt
                   (-map #'buffer-name (visible-buffers))
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

;; Smex - convenient command completer
(setf smex-history-length 100
      smex-save-file (concat +prog-data-path+ "/smex-items")
      smex-auto-update (not (platform-use? 'work))
      smex-flex-matching t)

(autoload 'smex "smex" "" t)
(autoload 'smex-major-mode-commands "smex" "" t)

;; pcomplete

(setf pcomplete-dir-ignore (rx bol (or "." "..") "/")
      ;; directory-files-no-dot-files-regexp
      pcomplete-ignore-case t
      pcomplete-autolist nil
      pcomplete-recexact nil
      pcomplete-cycle-completions t
      pcomplete-command-completion-function
      (lambda ()
        (pcomplete-here
         (pcomplete-entries nil
                            (lambda (filename)
                              (or (file-executable-p filename)
                                  (string-match-pure? (rx (or ".hs"
                                                              ".sh"
                                                              ".py"
                                                              ".exe"
                                                              ".bat"
                                                              ".cmd")
                                                          eol)
                                                      filename)))))))

(eval-after-load "pcomplete"
  '(progn
     (require 'pcmpl-gnu)
     (require 'pcmpl-linux)
     (require 'pcmpl-rpm)
     (require 'pcmpl-unix)))

;; Ido
(setf ido-enable-flex-matching t
      ido-create-new-buffer 'always ;; don't prompt when opening nonexistent files
      ido-file-extensions-order '(".hs" ".lhs" ".y" ".l" ".cabal" ".idr" ".agda" ".h" ".c" t)
      ido-case-fold t ;; ignore case
      ido-everywhere t
      ido-ignore-extensions t ;; ignore extensions in `completion-ignored-extensions'
      ido-enable-last-directory-history nil
      ido-save-directory-list-file nil
      ido-show-dot-for-dired nil
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
    ("SPC"        self-insert-command)
    ("C-SPC"      ido-edit-input)
    ("C-<return>" ido-select-text)
    ("<C-return>" ido-select-text)
    ("<up>"       previous-history-element)
    ("<down>"     next-history-element))
  (def-keys-for-map (ido-file-completion-map
                     ido-file-dir-completion-map)
    ("C-w"        ido-up-directory)))

(add-hook 'ido-setup-hook #'ido-setup-custom-bindings)

(provide 'completion-setup)

;; completion-setup.el ends here
