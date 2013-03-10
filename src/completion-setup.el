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


(require 'common)
(require 'icicles-setup)
(require 'pcomplete)

(defun completing-read-buffer (prompt &optional default require-match)
  (completing-read-vanilla prompt
                           (buffer-list)
                           nil
                           require-match
                           nil
                           nil
                           default))

(setf completion-styles '(partial-completion)
      completion-category-overrides '()
      read-buffer-function #'completing-read-buffer)

(setf pcomplete-autolist nil
      pcomplete-recexact nil
      pcomplete-cycle-completions t)

;; convenient buffer switcher


(require 'smex)

(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;; (require 'auto-complete-config)
;; (ac-config-default)



;;;;

;; note: ido is used my smex
(require 'ido)

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
                        " [Confirm]"))

;; (defun ido-setup ()
;;   ;; bind keys to ido-completion-map here, etc
;;   (def-keys-for-map (;; ido-common-completion-map
;;                      ;;ido-completion-map
;;                      ido-file-completion-map
;;                      )
;;     ("C-w" ido-up-directory))
;;
;;   (def-keys-for-map ido-completion-map
;;     ("<f1>" ido-toggle-regexp)))

;; (add-hook 'ido-setup-hook #'ido-setup)


;;;;


;; (require 'hippie-exp)
;;
;; (setq hippie-expand-try-functions-list
;;       (append
;;        ;; (when (fboundp 'yas/hippie-try-expand)
;;        ;;   '(yas/hippie-try-expand))
;;        '(;; try-expand-all-abbrevs
;;          ;; try-pabbrev-expand
;;
;;          try-expand-dabbrev
;;          ;; try-expand-dabbrev-all-buffers
;;          ;; try-expand-dabbrev-from-kill
;;          try-complete-file-name
;;          try-complete-file-name-partially
;;          ;; try-expand-list
;;          ;; try-expand-line
;;          try-complete-lisp-symbol
;;          try-complete-lisp-symbol-partially
;;          )))
;;
;; (global-set-key (kbd "M-/") 'hippie-expand)

;; (require 'pabbrev)
;;
;; (setq pabbrev-minimal-expansion-p t
      ;; pabbrev-global-mode-not-buffer-names '("*Messages*" "*Compile-Log*" "*Help*")
      ;; pabbrev-idle-timer-verbose nil
      ;; pabbrev-global-mode-buffer-size-limit nil
      ;; pabbrev-marker-distance-before-scavenge 2000
      ;; pabbrev-read-only-error nil
      ;; pabbrev-scavenge-some-chunk-size 64
      ;; pabbrev-thing-at-point-constituent (quote symbol))
;;
;;
;; ;; (defun he-general-word-beg ()
  ;; ;; "Return last non-whitespace position to the left of point"
  ;; ;; (let ((bol (line-beginning-position))
        ;; ;; (p))
    ;; ;; (save-excursion
      ;; ;; (let (char (char-before))
        ;; ;; (while (and
                ;; ;; (< bol (point))
                ;; ;; (not (equal char " "))
                ;; ;; (not (equal char "\n"))
                ;; ;; (not (equal char "\t")))
          ;; ;; (backward-char)))
      ;; ;; (setq p (point)))
    ;; ;; p))
;;
;; (defun he-general-word-beg ()
  ;; "Return last non-whitespace position to the left of point"
  ;; (let ((bol (line-beginning-position)))
    ;; (save-excursion
      ;; (skip-syntax-backward "^-()\"\\/$'<>!|" bol)
      ;; (point))))
;;
;; (defun try-pabbrev-expand (old)
  ;; (unless old
    ;; (he-init-string (he-general-word-beg) (point))
;;
    ;; (setq he-expand-list (sort
                          ;; (mapcar #'car
                                  ;; (pabbrev-fetch-all-suggestions-for-prefix
                                   ;; he-search-string))
                          ;; 'string-lessp)))
  ;; (while (and he-expand-list
              ;; (he-string-member (car he-expand-list) he-tried-table))
    ;; (setq he-expand-list (cdr he-expand-list)))
  ;; (if (null he-expand-list)
      ;; (progn
        ;; (when old (he-reset-string))
        ;; ())
      ;; (he-substitute-string (car he-expand-list))
      ;; (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
      ;; (setq he-expand-list (cdr he-expand-list))
      ;; t))

;; Now you can bind keys to hippie-expand for vanilla completion
;; and to pabbrev-expand-maybe for completion with popup

(provide 'completion-setup)

;; completion-setup.el ends here
