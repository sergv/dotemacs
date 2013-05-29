;; dotemacs.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

;; (setf debug-on-error t)

(load-library "set-up-platform")
(load-library "set-up-environment-variables")
(load-library "set-up-paths")
(load-library "set-up-font")

(load-library "base-emacs-fixes")
(load-library "more-scheme")
(load-library "more-clojure")
(load-library "custom")

;; ******************************************************************

;; parts of custom
(load-library "win-buf-utils")
(load-library "cycle-on-lines")

(load-library "common")
(load-library "search")
(load-library "persistent-store")
(persistent-store-init)

(load-library "backups")
(load-library "emacs-general-conf")

;; ******************************************************************

(setq compilation-auto-jump-to-first-error nil
      whitespace-line-column 81
      whitespace-style '(face lines-tail tabs)
      whitespace-global-modes nil)

(defconst +do-not-track-long-lines-modes+
  '(lisp-interaction-mode
    inferior-scheme-mode
    prolog-inferior-mode
    comint-mode
    org-mode
    inferior-octave-mode
    python-repl-mode
    inferior-haskell-mode

    makefile-automake-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    makefile-mode
    makefile-makepp-mode))

(defun* init-common (&key (use-yasnippet t)
                          (use-nxhtml-menu nil)
                          (use-comment t)
                          (use-whitespace t)
                          (use-render-formula nil))
  (linum-mode 1)
  (when use-comment
    (comment-util-mode 1))

  ;; (autopair-mode)
  ;; (set-buffer-file-coding-system 'utf-8-unix)

  (when use-yasnippet
    (yas-minor-mode-on))

  ;; it's usually already enabled by nxhtml autoloads
  ;; so action should be taken to turn it off
  (nxhtml-menu-mode (if use-nxhtml-menu 1 -1))

  (setq undo-tree-visualizer-timestamps    t
        undo-tree-visualizer-parent-buffer t)

  (when use-whitespace
    (whitespace-mode
     (if (memq major-mode
               +do-not-track-long-lines-modes+)
       -1
       +1)))

  (when use-render-formula
    (render-formula-mode 1)))

(defun* init-repl (&key (show-directory nil))
  (use-repl-modeline)
  (setf *vim:do-not-adjust-point* t)
  (emacs-forget-buffer-process))


(load-library "all-lisp-setup")
(load-library "org-mode-autoload")
;; (load-library "clojure-setup") ;; handled by all-lisp-setup
(load-library "persistent-sessions")

(load-library "c-like-setup")
(load-library "haskell-autoload")
(load-library "ocaml-setup")
(load-library "awk-setup")
(load-library "shell-autoloads")
(load-library "snippet-autoloads")
(load-library "eshell-setup")
(load-library "gnuplot-setup")
(load-library "html-autoload")
(load-library "latex-autoloads")
(load-library "debsources-setup")
(load-library "markdown-setup")
(load-library "prolog-setup")
(load-library "octave-setup")
(load-library "sql-setup")
(load-library "python-setup")
(load-library "cython-setup")
(load-library "graphviz-setup")
(load-library "csv-setup")
(load-library "rst-setup")
(load-library "texinfo-setup")
(load-library "maxima-setup")
(load-library "d-mode-setup")
(load-library "yaml-mode-setup")
(load-library "doc-view-setup")
(load-library "lua-setup")
;; (load-library "glsl-setup") ;; handled by c-like-setup
(load-library "other-setup")
(load-library "cmake-setup")
(load-library "asm-setup")
(load-library "llvm-setup")
;; (load-library "java-setup") ;; handled by c-like-setup

(load-library "compilation-setup")
(load-library "completion-setup")
(load-library "auto-insert-setup")
(load-library "emms-setup")
(load-library "tagged-buflist-setup")
(load-library "hl-paren")
(load-library "spell-setup")
(load-library "abbrev+")
(load-library "grep+")
(load-library "comment-util")
(load-library "comint-setup")
(load-library "dired-setup")
(load-library "remember-win-config")
(load-library "yasnippet-autoload")
;; (load-library "cedet-setup")
(load-library "git-setup")
(load-library "hideshow-setup")
(load-library "render-formula")
(load-library "image-mode-setup")
(load-library "tabbar-setup")
(load-library "calendar-mode-setup")
(load-library "telnet-mode-setup")
(load-library "ediff-setup")
(load-library "minimap-setup")
(load-library "select-mode")
(load-library "revive-setup")
(load-library "paredit-autoload")
(load-library "undo-tree-setup")
(load-library "recentf-setup")
(load-library "misc-autoloads")
(load-library "eproj-setup")

;; load keys after everything to ensure that nothing will be rebond
;; after it finishes
(load-library "keys")
(load-library "vim-init")

;; this is quick-and-dirty autoloading mechanism
(eval-after-load "ibuffer"
  '(progn
     (load-library "ibuffer-setup")))

(require 'fortunes)
(random t)
(setq initial-scratch-message nil)
(fortune-init-scratch-buf)


;; test faces for readability
;; (progn
;;   (load-file "~/emacs/color-lab.elc")
;;   (load-file "~/emacs/shr-color.elc")
;;   (mapc #'(lambda (entry)
;;             (let* ((colors (list
;;                             (cadr entry)
;;                             (caddr entry)))
;;                    (result (shr-color-visible (car colors) (cadr colors) t)))
;;               (when (not (equal colors result))
;;                 (message "face %S transform from %S to %S" (car entry) colors result))))
;;         '((face-name "bg" "fg")))
;;   nil)

(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)


(ex-commands-re-cache-update)


(require 'package)
(let ((package-load-list '((melpa t))))
  (package-initialize))

(provide 'dotemacs)

(unless (featurep 'custom-variables-defined)
  (load-library ".emacs"))

;; Local Variables:
;; End:

;; dotemacs.el ends here
