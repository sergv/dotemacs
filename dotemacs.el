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
(load-library "cycle-on-lines")

(load-library "common")
(load-library "persistent-store")
(persistent-store-init)

(load-library "backups")
(load-library "emacs-general-conf")
(load-library "smartparens-setup")

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
    inferior-octave-mode
    python-repl-mode
    inferior-haskell-mode

    makefile-automake-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    makefile-mode
    makefile-makepp-mode))

(defun vim:bind-local-keymaps ()
  (setf vim:normal-mode-local-keymap              (make-keymap)
        vim:visual-mode-local-keymap              (make-sparse-keymap)
        vim:insert-mode-local-keymap              (make-keymap)
        vim:operator-pending-mode-local-keymap    (make-sparse-keymap)
        vim:motion-mode-local-keymap              (make-sparse-keymap)
        vim:complex-command-override-local-keymap (make-sparse-keymap)))

(defvar disable-fci-mode? nil
  "Variable to control, whether to enable `fci-mode' in new buffers
or not.")

(defmacro with-disabled-fci (&rest body)
  (let ((old-val-var (gensym "Old-value")))
    `(let ((,old-val-var disable-fci-mode?))
       (unwind-protect
           (progn
             (setf disable-fci-mode? t)
             ,@body)
         (setf disable-fci-mode? ,old-val-var)))))

(defun* init-common (&key (use-yasnippet t)
                          (use-comment t)
                          (use-fci t)
                          (use-whitespace nil) ;; can be t, nil, 'tabs-only
                          (use-render-formula nil)
                          (use-hl-line t)
                          (sp-slurp-sexp-insert-space t))
  (linum-mode 1)
  (hl-line-mode (if use-hl-line +1 -1))
  (when use-comment
    (comment-util-mode 1))

  ;; (set-buffer-file-coding-system 'utf-8-unix)

  (when use-yasnippet
    (yas-minor-mode-on))

  (smartparens-buffer-local-setup)

  (setf undo-tree-visualizer-timestamps    t
        undo-tree-visualizer-parent-buffer t)

  (when use-whitespace
    (when (memq major-mode
                +do-not-track-long-lines-modes+)
      (error "Shouldn't have enabled whitespace-mode in %s" major-mode))
    (when (eq? use-whitespace 'tabs-only)
      (setq-local whitespace-style '(face tabs)))
    (whitespace-mode 1))

  (when use-render-formula
    (render-formula-mode 1))

  (vim:bind-local-keymaps)

  ;; I should figure what's going on here someday.
  (eval-after-load "smartparens"
    `(progn
       (when (not (eq ,sp-slurp-sexp-insert-space
                      sp-forward-slurp-sexp-insert-space))
         (setq-local sp-forward-slurp-sexp-insert-space ,sp-slurp-sexp-insert-space))))

  ;; bind in vim:normal-mode-local-keymap since
  ;; it will not be bound in vim:normal-mode-keymap because
  ;; everyone needs different versions, e.g. repls, shells
  (def-keys-for-map vim:normal-mode-local-keymap
    ("<return>"  sp-newline))
  (def-keys-for-map vim:insert-mode-local-keymap
    ("C-("       sp-backward-slurp-sexp)
    ("C-)"       sp-forward-slurp-sexp)
    ("M-("       sp-forward-barf-sexp)
    ("M-)"       sp-backward-barf-sexp)

    ("<return>"  sp-newline)

    ("M-?"       sp-convolute-sexp)
    ("C-<left>"  sp-backward-slurp-sexp)
    ("C-<right>" sp-forward-slurp-sexp)
    ("M-<left>"  sp-forward-barf-sexp)
    ("M-<right>" sp-backward-barf-sexp)
    ("M-<up>"    sp-splice-sexp-killing-backward)
    ("M-<down>"  sp-splice-sexp-killing-forward))

  (when (and use-fci
             (not disable-fci-mode?))
    (fci-mode (if (memq major-mode
                        +do-not-track-long-lines-modes+)
                -1
                +1))))

(defun* init-repl (&key (show-directory nil)
                        (bind-return t)
                        (create-keymaps nil)
                        (bind-vim:motion-current-line t))
  (use-repl-modeline :show-directory show-directory)
  (setq-local *vim:do-not-adjust-point* t)
  (setq-local vim:insert-mode-exit-move-point 'dont-move-at-line-end)
  (emacs-forget-buffer-process)

  (when create-keymaps
    (vim:bind-local-keymaps))
  (when bind-vim:motion-current-line
    (if (not (null? vim:operator-pending-mode-local-keymap))
      (def-keys-for-map vim:operator-pending-mode-local-keymap
        ("c" vim:motion-current-line))
      (message "init-repl warning: vim:operator-pending-mode-local-keymap is nil, \"c\" not bound in buffer %s"
               (current-buffer))))

  (cond ((keymapp bind-return)
         (def-keys-for-map bind-return
           ("<return>"   comint-send-input)
           ("C-<return>" sp-newline)))
        ((and (not (null? bind-return))
              (cons? bind-return)
              (keymapp (car bind-return)))
         (dolist (keymap bind-return)
           (def-keys-for-map keymap
             ("<return>"   comint-send-input)
             ("C-<return>" sp-newline))))
        ((not (null? bind-return))
         (def-keys-for-map (vim:normal-mode-local-keymap
                            vim:insert-mode-local-keymap)
           ("<return>"   comint-send-input)
           ("C-<return>" sp-newline)))))

(defun* bind-tab-keys (tab-binding
                       backtab-binding
                       &key (enable-yasnippet nil))
  (if enable-yasnippet
    (progn
      (setq-local yas-expand-fallback tab-binding)
      (def-keys-for-map (vim:normal-mode-local-keymap
                         vim:insert-mode-local-keymap)
        ("<tab>" yas-expand-or-fallback)))
    (dolist (kmap (list vim:normal-mode-local-keymap
                        vim:insert-mode-local-keymap))
      (define-key kmap (kbd "<tab>") tab-binding)))
  (dolist (kmap (list vim:normal-mode-local-keymap
                      vim:insert-mode-local-keymap))
    (dolist (binding (list (kbd "<backtab>")
                           (kbd "S-<tab>")
                           (kbd "S-<iso-lefttab>")))
      (define-key kmap binding backtab-binding))))

(load-library "all-lisp-setup")
(load-library "org-mode-autoload")
(load-library "persistent-sessions-autoloads")

(load-library "c-like-setup")
(load-library "haskell-autoload")
(load-library "ocaml-setup")
(load-library "awk-setup")
(load-library "shell-autoloads")
(load-library "snippet-autoloads")
(load-library "gnuplot-setup")
(load-library "html-autoload")
(load-library "js-setup")
(load-library "css-setup")
(load-library "latex-autoloads")
(load-library "debsources-setup")
(load-library "markdown-setup")
(load-library "prolog-setup")
(load-library "octave-setup")
(load-library "sql-setup")
(load-library "python-autoloads")
(load-library "graphviz-setup")
(load-library "csv-setup")
(load-library "rst-setup")
(load-library "texinfo-setup")
(load-library "yaml-mode-setup")
(load-library "doc-view-setup")
(load-library "lua-setup")
;; (load-library "glsl-setup") ;; handled by c-like-setup
(load-library "other-setup")
(load-library "cmake-setup")
(load-library "asm-setup")
(load-library "llvm-setup")
;; (load-library "java-setup") ;; handled by c-like-setup
(load-library "antlr-setup")
(load-library "bison-setup")
(load-library "verilog-setup")
(load-library "vhdl-setup")
(load-library "tcl-setup")
(load-library "cool-setup")
(load-library "agda-autoload")
(load-library "idris-setup")
(load-library "dos-setup")

(load-library "search-autoload")
(load-library "compilation-setup")
(load-library "completion-setup")
(load-library "auto-insert-setup")
(load-library "hl-paren")
(load-library "spell-setup")
(load-library "abbrev+")
(load-library "grep-autoload")
(load-library "comment-util")
(load-library "comint-autoload")
(load-library "dired-autoload")
(load-library "yasnippet-autoload")
(load-library "git-autoload")
(load-library "hideshow-autoload")
(load-library "render-formula-autoload")
(load-library "image-mode-setup")
(load-library "calendar-mode-setup")
(load-library "telnet-mode-setup")
(load-library "ediff-autoload")
(load-library "select-mode-setup")
(load-library "paredit-autoload")
(load-library "undo-tree-setup")
(load-library "recentf-setup")
(load-library "misc-autoloads")
(load-library "eproj-setup")
(load-library "xilinx-setup")
(load-library "diff-mode-setup")
(load-library "fci-setup")

;; load keys after everything to ensure that nothing will be rebond
;; after it finishes
(load-library "keys")
(load-library "vim-setup")

;; this is quick-and-dirty autoloading mechanism
(eval-after-load "ibuffer"
  '(progn
     (load-library "ibuffer-setup")))

(require 'fortunes)
(random t)
(setq initial-scratch-message nil)
(fortune-init-scratch-buf)


(require 'theme-changer)
(change-theme #'color-theme-solarized+-light
              #'color-theme-solarized+-dark)

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

(require 'package)
(let ((package-load-list '((melpa t))))
  (package-initialize))

(provide 'dotemacs)

(unless (featurep 'custom-variables-defined)
  (load-library ".emacs"))



(let ((user-info-file
       (find-if #'file-exists?
                (list (concat +emacs-config-path+ "/../user-info.el")
                      (concat +emacs-config-path+ "/user-info.el")))))
  (aif user-info-file
    (load-file it)
    (error "user-info.el not found")))

(let ((machine-specific-setup-file
       (find-if #'file-exists?
                (list (concat +emacs-config-path+ "/../machine-specific-setup.el")
                      (concat +emacs-config-path+ "/machine-specific-setup.el")))))
  (aif machine-specific-setup-file
    (load-file it)
    (error "machine-specific-setup.el not found")))

(when (platform-os-type? 'windows)
  (load-library "windows-setup"))

;; Local Variables:
;; End:

;; dotemacs.el ends here
