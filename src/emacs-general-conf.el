;; emacs-general-conf.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'set-up-platform)
  (require 'macro-util)

  (defvar lv-wnd))

(require 'common)
(require 'current-column-fixed)
(require 'persistent-sessions-global-vars)
(require 'set-up-paths)

(require 'tabbar-setup)

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

        bkr-mode
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
        undo-tree-visualizer-mode
        xref--xref-buffer-mode
        debugger-mode))

(global-auto-revert-mode 1)


;;;; saveplace - minor mode to remember positions in visited files
(setq save-place-file (path-concat +prog-data-path+ "saveplace")
      save-place-limit nil)
(require 'saveplace)
(save-place-mode +1)

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

;; Ignore case during general searches.
(setq-default case-fold-search t)

(setf select-enable-clipboard t
      interprogram-paste-function
      (fold-platform-os-type
       #'gui-selection-value
       #'w32-get-clipboard-data)
      query-replace-highlight t
      search-highlight t
      undo-limit (* 100 1024 1024)
      undo-outer-limit (* 32 1024 1024)
      undo-strong-limit (* 256 1024 1024)

      inhibit-startup-message t
      inhibit-startup-screen t
      frame-title-format '("%f")
      scroll-step 1
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

      ;; Length from the beginning of buffer for magic mode detection.
      magic-mode-regexp-match-limit 1000000
      suggest-key-bindings nil

      ;; Always prefer *.elc to *.el.
      load-prefer-newer nil
      kill-ring-max 1024
      save-interprogram-paste-before-kill nil
      enable-recursive-minibuffers t
      ad-default-compilation-action 'always
      echo-keystrokes 0.01
      warning-minimum-level :error
      use-empty-active-region t

      ;; Set only variables marked as safe, ignore all other.
      enable-local-variables :safe

      sentence-end-double-space nil

      vc-display-status nil
      ;; Don’t want vc to do anything by default. Could enable
      ;; depending on circumstances and whether I need it. But not by
      ;; default!
      vc-handled-backends nil

      ;; LSP may produce pretty large outputs during its work.
      read-process-output-max 32768

      ;; I’ll compile everything I need ahead of time.
      native-comp-deferred-compilation nil

      ;; Whether to save existing clipboard value in kill-ring when doing a kill/yank operation.
      save-interprogram-paste-before-kill nil

      fill-colunm 80

      message-log-max (ash 1 19)

      ;; To be able to show tall messages
      max-mini-window-height 0.5)

(when-emacs-version (<= 28 it)
  (when (boundp 'native-comp-deferred-compilation-deny-list)
    (add-to-list 'native-comp-deferred-compilation-deny-list ".*")))

(remove-hook 'find-file-hook #'vc-refresh-state)

;; Speed up display of long lines at the expense of correct
;; handling of bidirectional text.
(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

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

(defun shoud-enable-so-long-mode? ()
  "Check whether current buffer should be put into ‘so-long-mode’."
  (and (if (eval-when-compile (fboundp #'buffer-line-statistics))
           (so-long-statistics-excessive-p)
         (so-long-detected-long-line-p))
       ;; cabal-install’s plan.json is formatted as a single line but is
       ;; typicaly never too long to cause significant issues for Emacs.
       ;; Any working with the file will involve pretty printing it anyway.
       (not (s-suffix-p "/cache/plan.json"
                        (buffer-file-name (current-buffer))))))

;; Give user a chance to close file with very long lines without freezing Emacs.
(global-so-long-mode)
(setf so-long-variable-overrides
      (append so-long-variable-overrides
              '((show-trailing-whitespace . nil)))
      so-long-predicate
      #'shoud-enable-so-long-mode?)

;;;; key definitions

(require 'keys-def)

;;;; customizations without dedicated setup file

(eval-after-load "term" ;; ansi-term et al
  '(progn
     (require 'term-setup)))

(defun scroll-up--preserve-column (old-scroll-up &rest args)
  (let ((col (current-column-fixed)))
    (apply old-scroll-up args)
    (unless (bobp)
      (move-to-column col))))

(advice-add 'scroll-up :around #'scroll-up--preserve-column)

(defun scroll-down--preserve-column (old-scroll-up &rest args)
  (let ((col (current-column-fixed)))
    (apply old-scroll-up args)
    (unless (eobp)
      (move-to-column col))))

(advice-add 'scroll-down :around #'scroll-down--preserve-column)

(when-emacs-version (>= it 30)
  (require 'auth-source))

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

;; This global mode is enabled by default and is superseded by hl-paren.
(show-paren-mode -1)

;; NB there’s also ‘help-mode-setup’ defined in vanilla Emacs, which we don’t want.
;;;###autoload
(defun proper-help-mode-setup ()
  (hl-line-mode +1))

;;;###autoload
(add-hook 'help-mode-hook #'proper-help-mode-setup)

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
