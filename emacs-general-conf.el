;; emacs-general-conf.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'set-up-paths)
(require 'common)
(require 'custom-predicates)
(require 'more-scheme)

(require 'vim)
(vim-mode 1)

; set up modes where vim-mode shold be (partially) disabled
(setf vim:initial-modes
      '((fundamental-mode . nil)
        (text-mode . normal)
        (slime-repl-mode . normal)
        (sldb-mode . nil)
        (slime-inspector-mode . nil)
        (slime-fuzzy-completions-mode . nil)
        (slime-xref-mode . nil)
        (Custom-mode . nil)
        (ibuffer-mode . nil)
        (ediff-mode . nil)
        (emms-playlist-mode . nil)
        (dired-mode . nil)
        (isearch-mode . nil)
        (debugger-mode . nil)
        (compilation-mode . nil)
        (grep-mode . nil)
        (occur-mode . nil)
        (completion-list-mode . nil)
        (doc-view-mode . nil)
        (browse-kill-ring-mode . nil)
        (magit-status-mode . nil)
        (magit-log-mode . nil)
        (magit-key-mode . nil)
        (magit-show-branches-mode . nil)
        (magit-branch-manager-mode . nil)
        (magit-commit-mode . nil)
        (magit-diff-mode . nil)
        (org-agenda-mode . nil)
        (image-mode . nil)
        (calendar-mode . nil)
        (select-mode . nil)
        (minimap-mode . nil)
        (tagged-buflist-mode . nil)
        (git-rebase-mode . nil)

        (clojure-compilation-mode . nil)
        (nrepl-popup-buffer-mode . nil)
        (nrepl-macroexpansion-minor-mode . nil)
        (nrepl-interaction-mode . nil)))

;;;; modeline
(setq-default mode-line-format
              '(" %[%b%] "
                ;; if buffer has assigned file and is modified
                (:eval (when (and (buffer-file-name)
                                  (buffer-modified-p))
                         "(+)"))
                (:eval (when buffer-read-only
                         "(RO)"))
                ("("
                 mode-name
                 mode-line-process
                 ")")
                (:eval
                 (case (coding-system-eol-type buffer-file-coding-system)
                   ;; here should be unix but it is most of the time so
                   ;; there's no reason to say obvious things
                   (0 "")
                   (1 "(dos)")
                   (2 "(mac)")))
                (:eval
                 (when (buffer-narrowed?)
                   "(Narrowed)"))
                " "
                (:eval
                 (when vc-mode
                   (concat vc-mode " ")))
                (line-number-mode
                 ("%l/"
                  (:eval (number-to-string
                          (count-lines (point-min)
                                       (point-max))))
                  "(%p)"))
                (column-number-mode
                 (2 " %c"))
                (which-func-mode (" (" which-func-format ")"))
                ;; this usually shows vim's current mode - hardly interesting
                ;; for me
                ;; global-mode-string
                ))

;;;; modes without much customization
(setq-default frame-background-mode 'light)

(global-auto-revert-mode t)

;;;; saveplace - minor mode to remember positions in visited files
(setq save-place-file (path-concat +prog-data-path+ "saveplace"))
(setq-default save-place t)
(require 'saveplace)

;;;; bunch of standard customizations

(setq-default indent-tabs-mode nil) ;; never use tabs for indentation
(setq-default cursor-type 'box)     ;; 'bar)
(setf cursor-type 'box
      ;; default-major-mode 'text-mode ;;'lisp-interaction-mode
      tab-width 4
      tab-always-indent t)

(setq-default major-mode 'text-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(line-number-mode +1)
(column-number-mode +1)

(setf x-select-enable-clipboard t
      interprogram-paste-function
      (if (platform-os-type? 'linux)
        #'x-cut-buffer-or-selection-value
        #'x-get-selection)
      query-replace-highlight t
      query-replace-interactive nil ;; do not use last search string as initial regexp
      search-highlight t
      undo-limit 1000000
      undo-outer-limit 32000000
      undo-strong-limit (* 10 undo-limit)

      require-final-newline t
      inhibit-startup-message t
      frame-title-format '("%f")
      scroll-step 1
      case-fold-search nil ;; do not ignore case during search
      next-line-add-newlines nil
      read-file-name-completion-ignore-case t

      auto-save-list-file-name  nil
      auto-save-default         nil
      linum-format "%d"
      search-whitespace-regexp nil ;; intuitive behavior for interactive regexps
      system-time-locale "C"

      visible-bell nil ;; any kind of bell is annoying
      message-log-max t

      ;; length from the beginning of buffer for magic mode detection
      magic-mode-regexp-match-limit 1000000)

;; Remove completion buffer when done
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (aif (get-buffer "*Completions*")
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
(mouse-wheel-mode -1)
(def-keys-for-map global-map
  ("<mouse-1>"      nil)
  ("<down-mouse-1>" nil)
  ("<up-mouse-1>"   nil)
  ("<drag-mouse-1>" nil)
  ("<mouse-2>"      nil)
  ("<down-mouse-2>" nil)
  ("<up-mouse-2>"   nil)
  ("<drag-mouse-2>" nil)
  ("<mouse-3>"      nil)
  ("<down-mouse-3>" nil)
  ("<up-mouse-3>"   nil)
  ("<drag-mouse-3>" nil))


;;;; native emacs backup
(setq make-backup-files   nil
      ;; kept-new-versions   100
      ;; kept-old-versions   100
      ;; delete-old-versions t
      ;; version-control     'never
      )
(setq backup-directory-alist nil)

;;;; desktops
(setq desktop-save 'if-exists
      desktop-dirname +prog-data-path+
      desktop-base-file-name "emacs.session"
      desktop-path (list "." +prog-data-path+))
(desktop-save-mode -1)

;; nuke trailing whitespaces when writing to a file
(add-hook 'write-file-hooks #'delete-trailing-whitespace+)

(setq auto-save-list-file-prefix (path-concat +prog-data-path+
                                              "auto-save-list/.save-"))

;;;; color - themes, current line etc
(global-hl-line-mode t)

;; primarily used by the theme-changer
(setf calendar-location-name "Nikolaev Ukraine"
      calendar-latitude 46.967
      calendar-longitude 32)

(setq color-theme-libraries
      (directory-files +color-themes-path+ t "^color-theme")
      color-theme-directory (concat +emacs-config-path+
                                    "/src/color-theme-6.6.0/themes/")
      color-theme-libraries (list (concat +emacs-config-path+
                                          "/src/color-theme-6.6.0/themes/color-theme-library.el"))
      color-theme-load-all-themes nil)
(require 'solarized+)


(setf font-lock-maximum-decoration
      ;; there are three decoration levels, 1 being the minimum
      ;; and 3 being the maximum. With t latex slows down on netbook.
      '((latex-mode . 2) (LaTeX-mode . 2) (t . t))
      ;; font-lock-global-modes
      ;; '(not cl-mode)
      )

(global-font-lock-mode 1)

;;;; key definitions

(require 'keys-def)

;;;; customizations without dedicated setup file

(eval-after-load "term" ;; ansi-term et al
  '(progn
     (setf ansi-term-color-vector
           ["#fdf6e3" "#586475" "#dc322f" "#859900" "#b58900"
            "#268bd2" "#d33682" "#2aa198" "#839496"]
           term-buffer-maximum-size 0 ;; don't truncate anything
           )))

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


(eval-after-load "ring" '(require 'ring+))

;;;; aliases

(fset 'yes-or-no-p 'y-or-n-p)
(defalias 'rm 'delete-file)
(defalias 'mv 'rename-file-and-buffer)
(defalias 'cp 'copy-file-and-open)
(defalias 'mv* 'rename-file)
(defalias 'cp* 'copy-file)
(defalias 'mkdir 'make-directory)
(defalias 'rmdir 'delete-directory)
(defalias 'qrr 'query-replace-regexp)
(defalias 'open 'find-file)
(defalias 'openo 'find-file-other-window)
(defalias 'unnarrow 'widen)
(defalias 'align 'align-regexp)
(defalias 'toggle-wrap-lines 'toggle-truncate-lines)

;;;; Epilogue

(provide 'emacs-general-conf)

;; Local Variables:
;; End:

;; emacs-general-conf.el ends here
