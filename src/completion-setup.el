;; completion-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago (since august inception)
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'set-up-platform)
  (require 'macro-util))

(require 'common-constants)
(require 'common)
(require 'set-up-paths)

(require 'ivy)
;; (require 'ivy-posframe)
(require 'flx)

(eval-after-load "pcomplete"
  '(progn
     (require 'pcmpl-gnu)
     (require 'pcmpl-linux)
     (require 'pcmpl-rpm)
     (require 'pcmpl-unix)))

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
      read-buffer-function #'completing-read-buffer
      ;; Make tab completion in shell not offer ‘.’ and ‘..’ completions.
      completion-regexp-list (list
                              (rx bos (or ""
                                          (not ?.)
                                          (seq (char ?.)
                                               (not ?.))
                                          (seq (char ?.)
                                               (char ?.)
                                               anything)))))

;; Smex - convenient command completer
(setf smex-history-length 100
      smex-save-file (concat +prog-data-path+ "/smex-items")
      smex-auto-update (not (platform-use? 'work))
      smex-flex-matching t)

(autoload 'smex "smex" "" t)
(autoload 'smex-major-mode-commands "smex" "" t)

;; pcomplete

(when-emacs-version (and (<= 25 it)
                         (<= it 27))
  (setf pcomplete-ignore-case t))

(when-emacs-version (<= 28 it)
  (setf completion-ignore-case t))

(setf pcomplete-dir-ignore (rx bol (or "." "..") "/")
      ;; directory-files-no-dot-files-regexp
      pcomplete-autolist nil
      pcomplete-recexact nil
      pcomplete-cycle-completions t
      pcomplete-command-completion-function
      (lambda ()
        (pcomplete-here
         (pcomplete-entries nil
                            (lambda (filename)
                              (or (file-executable-p filename)
                                  (string-match-p (rx (or ".hs"
                                                          ".sh"
                                                          ".py"
                                                          ".exe"
                                                          ".bat"
                                                          ".cmd")
                                                      eol)
                                                  filename)))))))

(setf ivy-use-virtual-buffers t
      ivy-initial-inputs-alist nil
      ivy-wrap t
      ivy-case-fold-search-default t ;; ignore case
      ivy-flx-limit 1000
      ivy-extra-directories '("./")
      ;; Ivy uses strange popup for completing in region which may leave
      ;; Emacs unresponsive on some setups.
      ivy-do-completion-in-region nil
      counsel-find-file-ignore-regexp
      (eval-when-compile
        (let ((re-group
               (lambda (x) (concat "\\(?:" x "\\)")))
              (re-alt
               (lambda (x y) (concat x "\\|" y))))
          (concat
           "\\`"
           (funcall re-group
                    (funcall re-alt
                             (funcall re-group
                                      (concat
                                       ".*"
                                       (regexp-opt
                                        (append (list ".#" ".cask")
                                                +ignored-file-extensions+))))
                             (funcall re-group
                                      (regexp-opt +version-control-directories+))))
           "\\'")))

      ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center))
      ivy-posframe-border-width 4
      ivy-posframe-hide-minibuffer t)

(ivy-mode +1)
;; (ivy-posframe-mode +1)

(add-to-list 'ivy-ignore-buffers invisible-buffers-re)

(awhen (assq 'read-file-name-internal ivy-sort-functions-alist)
  (setcdr it #'ivy-sort-file-function-prioritise-visible-dirs))

(defun ivy-sort-file-function-prioritise-visible-dirs (x y)
  "Compare two files X and Y.
Prioritize directories unless they're invisible."
  (if (= 0 (length x))
      (not (= 0 (length y)))
    (let* ((x-dir? (file-directory-p x))
           (y-dir? (file-directory-p y))
           (x-visible? (and (< 0 (length x))
                            (not (char-equal ?. (aref x 0)))))
           (y-visible? (and (< 0 (length y))
                            (not (char-equal ?. (aref y 0)))))
           (cmp-dir
            (lambda ()
              (if x-dir?
                  (if y-dir?
                      (string< (directory-file-name x) (directory-file-name y))
                    t)
                (if y-dir?
                    nil
                  (string< x y))))))
      (if x-visible?
          (if y-visible?
              (funcall cmp-dir)
            t)
        (if y-visible?
            nil
          (funcall cmp-dir))))))

(def-keys-for-map ivy-minibuffer-map
  (("C-h" "<C-up>")   ivy-next-history-element)
  (("C-t" "<C-down>") ivy-previous-history-element)
  ("C-p"              yank)
  ("C-S-p"            browse-kill-ring)
  ("C-w"              ivy-backward-kill-word)
  ("C-S-w"            backward-delete-word*)

  ("C-v"              set-mark-command)
  ("C-y"              copy-region-as-kill)
  ("C-d"              kill-region)

  ("C-SPC"            delete-minibuffer-contents)
  ("<C-return>"       ivy-immediate-done))

(provide 'completion-setup)

;; completion-setup.el ends here
