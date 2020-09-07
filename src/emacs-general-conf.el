;; emacs-general-conf.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'set-up-paths)
(require 'common)

(require 'vim)
(vim-mode 1)

;;;; modes without much customization
(setq-default frame-background-mode nil)

(setf global-auto-revert-ignore-modes
      '(comint-mode
        git-rebase-mode
        magit-diff-mode
        magit-log-mode
        magit-popup-mode
        magit-process-mode
        magit-reflog-mode
        magit-refs-mode
        magit-revision-mode
        magit-stash-mode
        magit-status-mode

        dired-mode
        haskell-compilation-mode
        rust-compilation-mode
        inferior-scheme-mode
        dante-repl-mode
        lisp-interaction-mode
        prolog-inferior-mode
        python-repl-mode
        select-mode
        shell-mode
        special-mode
        undo-tree-visualizer-mode))

(global-auto-revert-mode 1)


;;;; saveplace - minor mode to remember positions in visited files
(setq save-place-file (path-concat +prog-data-path+ "saveplace"))
(setq-default save-place t)
(require 'saveplace)

;;;; bunch of standard customizations

(setq-default indent-tabs-mode nil) ;; never use tabs for indentation

(setf cursor-type 'hbar
      tab-width 2
      tab-always-indent t)
(setq-default major-mode 'text-mode
              cursor-type 'hbar)
(blink-cursor-mode -1)

(setq-default major-mode 'text-mode)

(remove-hook 'temp-buffer-setup-hook #'help-mode-setup)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(line-number-mode +1)
(column-number-mode +1)
(fringe-mode (cons 0 0))

;; Get rid of annoying message on emacs start, from
;; http://yann.hodique.info/blog/rant-obfuscation-in-emacs/
(put 'inhibit-startup-echo-area-message 'saved-value
     (setq inhibit-startup-echo-area-message (user-login-name)))

;; Final newline requirement is for C89 or earlier programs that
;; for some reason cannot figure out how to safely read lines
;; from a file. Perhaps it’s time to replace those programs
;; rather than perpetuate this lunacy.
(setq-default require-final-newline nil)

(setf x-select-enable-clipboard t
      interprogram-paste-function
      (fold-platform-os-type
       #'x-cut-buffer-or-selection-value
       #'w32-get-clipboard-data)
      query-replace-highlight t
      query-replace-interactive nil ;; do not use last search string as initial regexp
      search-highlight t
      undo-limit (* 100 1024 1024)
      undo-outer-limit (* 32 1024 1024)
      undo-strong-limit (* 256 1024 1024)


      inhibit-startup-message t
      inhibit-startup-screen t
      frame-title-format '("%f")
      scroll-step 1
      ;; Do not ignore case during search, except under special
      ;; conditions.
      case-fold-search nil
      ;; Make uppercase characters during search not disable
      ;; ignore-case mode.
      search-upper-case nil
      next-line-add-newlines nil
      read-file-name-completion-ignore-case t

      auto-save-list-file-name nil
      auto-save-default nil
      search-whitespace-regexp nil ;; intuitive behavior for interactive regexps
      system-time-locale "C"

      ;; Any kind of bell is annoying, but not when we're on windows
      ;; since it uses audible bell everywhere.
      visible-bell (fold-platform-os-type nil t)
      message-log-max 2048

      ;; Length from the beginning of buffer for magic mode detection.
      magic-mode-regexp-match-limit 1000000
      suggest-key-bindings nil

      ;; Always prefer *.elc to *.el.
      load-prefer-newer nil
      kill-ring-max 512
      save-interprogram-paste-before-kill nil
      enable-recursive-minibuffers t
      ad-default-compilation-action 'always
      echo-keystrokes 0.01
      warning-minimum-level :error
      use-empty-active-region t

      ;; Set only variables marked as safe, ignore all other.
      enable-local-variables :safe

      sentence-end-double-space nil

      vc-display-status nil)

(remove-hook 'find-file-hook #'vc-refresh-state)

;; Speed up display of long lines at the expense of correct
;; handling of bidirectional text.
(setq-default bidi-display-reordering nil)

(when (boundp 'ring-bell-function)
  (setf ring-bell-function #'ignore))

;; Remove completion buffer when done
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (awhen (get-buffer "*Completions*")
              (kill-buffer it))))

;;;; character enconding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility since default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp buffer-file-coding-system)
    (setq buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


;;;; disable mouse scrolling
;; (mouse-wheel-mode -1)
(def-keys-for-map ( ;; global-map
                   minibuffer-inactive-mode-map)
  ("<mouse-1>"      ignore)
  ("<down-mouse-1>" ignore)
  ("<up-mouse-1>"   ignore)
  ("<drag-mouse-1>" ignore)
  ("<mouse-2>"      ignore)
  ("<down-mouse-2>" ignore)
  ("<up-mouse-2>"   ignore)
  ("<drag-mouse-2>" ignore)
  ("<mouse-3>"      ignore)
  ("<down-mouse-3>" ignore)
  ("<up-mouse-3>"   ignore)
  ("<drag-mouse-3>" ignore)
  ("<mouse-4>"      ignore)
  ("<mouse-5>"      ignore))

;;;; native emacs backup
(setq make-backup-files   nil)
(setq backup-directory-alist nil)

;;;; desktops
(setq desktop-save nil)
(desktop-save-mode -1)

;; nuke trailing whitespaces when writing to a file
(add-to-list 'write-file-functions
             #'delete-trailing-whitespace+)

(setq auto-save-list-file-prefix (path-concat +prog-data-path+
                                              "auto-save-list/.save-"))

;;;; color themes, current line etc
(global-hl-line-mode -1)

;; primarily used by the theme-changer
;; (setf calendar-location-name "Nikolaev Ukraine"
;;       calendar-latitude 46.967
;;       calendar-longitude 32)
;; as reported by Google
(setf calendar-location-name "Kiev Ukraine"
      calendar-latitude 50.4500
      calendar-longitude 30.5233)

(setf font-lock-maximum-decoration
      ;; there are three decoration levels, 1 being the minimum
      ;; and 3 being the maximum. With t latex slows down on netbook.
      '((latex-mode . 2) (LaTeX-mode . 2) (t . t))
      ;; font-lock-global-modes
      ;; '(not cl-mode)
      )

(global-font-lock-mode 1)

;; Give user a chance to close file with very long lines without freezing Emacs.
(global-so-long-mode)
(setf so-long-variable-overrides
      (append so-long-variable-overrides
              '((show-trailing-whitespace . nil))))

;;;; key definitions

(require 'keys-def)

;;;; fill-column-indicator

(setf fci-handle-truncate-lines t
      fci-rule-column 80
      fill-colunm 80)

;;;; tab bar

;; hide tab bar if there's only one tab
(setf tab-bar-show 1
      ;; Open current buffer in new tab
      tab-bar-new-tab-choice t
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-tab-hints t
      tab-bar-tab-name-function #'tab-bar-tab-name-current-with-count
      tab-bar-new-tab-to 'right)

(tab-bar-mode 1)

;;;; customizations without dedicated setup file

(eval-after-load "term" ;; ansi-term et al
  '(progn
     (require 'term-setup)))

(defadvice scroll-up (around
                      scroll-up-preserve-column
                      compile
                      activate)
  (let ((col (current-column)))
    ad-do-it
    (move-to-column col)))

(defadvice scroll-down (around
                        scroll-down-preserve-column
                        compile
                        activate)
  (let ((col (current-column)))
    ad-do-it
    (move-to-column col)))

(def-keys-for-map read-passwd-map
  ("C-p" yank))

(awhen (get-buffer "*Messages*")
  (with-current-buffer it
    (read-only-mode -1)))

;; (unless noninteractive
;;
;;   (defun maximize-frame (&optional frame)
;;     "Maximize the selected FRAME."
;;     (interactive)
;;     (or frame
;;         (setq frame (selected-frame)))
;;     (let ((pixels-per-col (/ (float (frame-pixel-width))
;;                              (frame-width)))
;;           (pixels-per-row (/ (float
;;                               (frame-pixel-height)) (frame-height))))
;;       (set-frame-size frame
;;                       ;; truncate or round?
;;                       (truncate (/
;;                                  (x-display-pixel-width) pixels-per-col))
;;                       ;; reduce size to account for the toolbar
;;                       (- (truncate (/
;;                                     (x-display-pixel-height) pixels-per-row)) 7))
;;       (set-frame-position frame 0 0)))
;;
;;   (maximize-frame))


(defun egc--create-non-existent-directory ()
  "Ask to create parent dirs when opening a non-existent file."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-directory-p parent-directory))
               (y-or-n-p (format "Parent directory does not exist: %s. Create?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'egc--create-non-existent-directory)


;;;; aliases

(fset 'yes-or-no-p 'y-or-n-p)
(defalias 'mv 'rename-file-and-buffer)
(defalias 'cp 'copy-file-and-open)
(defalias 'mkdir 'make-directory)
(defalias 'rmdir 'delete-directory)
(defalias 'unnarrow 'widen)

;;;; Epilogue

(provide 'emacs-general-conf)

;; Local Variables:
;; End:

;; emacs-general-conf.el ends here
