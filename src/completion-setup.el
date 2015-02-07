;; completion-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago (since august inception)
;; Description:


;; As of current time (Sat Aug 27 19:03:59 EEST 2011) I don't use
;; hippie-expand/pabbrev completion at all.
;; And I'm planning to try autocomplete/predictive completion (qbit)
;; when I'll be in appropriate mood, so this file is left mostly for
;; reference purposes

;; Huh, now (Thu Dec 15 18:26:37 FET 2011) icicles proved itself completely
;; inadequate as vehicle for carrying out completions for me -
;; I cannot withstand it's source code due to awful style,
;; 100 column width, etc.
;; and unfortunately modifications to source are neccessary in order to
;; make it work as desired, especially after hacking in swank completion

;; That's funny, today (Sat Dec 17 12:32:55 FET 2011) i completely returned
;; to Icicles and don't want anything else, especially this anything.el
;; which seems nice but misses my aim. Auto-completion (the one with nice
;; popup menu) plays bad with vim mode. Ido seems not bad for file and
;; buffer searches but it's totally inadequate for everything else.
;; Only smex is left since it's just convenient.
;;
;; Upgraded Icicles seems to work OK

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'icicles-autoload)

(setf read-buffer-completion-ignore-case t)

(defun completing-read-buffer (prompt &optional default require-match)
  (completing-read-vanilla prompt
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

(eval-after-load "smex"
  '(progn
     ;; note: ido is used my smex

     ;; create new buffers if buffer with prompted name does not exist
     (setf ido-enable-flex-matching t
           ;; ido-create-new-buffer 'always
           ;; ido-file-extensions-order '(".org" ".lisp" ".l" ".cl" ".clisp" ".el" t)
           ;; ido-ignore-extensions t
           ;; case ignorance
           ;; ido-case-fold t
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
                             " [Confirm]"))))

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
