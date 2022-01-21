;; dump.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created:  1 September 2020
;; Description:

(require 'cl)

(defvar dumping t)

(defun dump-main (emacs-dir)
  (let ((init-file
         (cl-find-if #'file-exists-p
                     (mapcan (lambda (x) (list (concat emacs-dir "/src/" x)
                                          (concat "~/" x)))
                             '(".emacs"))))
        (dump-target "~/.emacs.d/emacs.dmp"))

    (let* ((emacs-root (getenv "EMACS_ROOT"))
           (default-emacs-dir (expand-file-name "~/.emacs.d"))
           (default-src-dir (expand-file-name "src" default-emacs-dir)))
      (cond
        (emacs-root
         (progn
           (cl-assert (file-directory-p emacs-root))
           (let ((src-dir (concat emacs-root "/src")))
             (cl-assert (file-directory-p src-dir))
             (add-to-list 'load-path src-dir))))
        ((file-directory-p default-src-dir)
         (add-to-list 'load-path default-src-dir))
        (t
         (error "EMACS_ROOT not defined"))))

    ;; (message "Loading start.el...")
    ;; (load-library "start")
    ;; (message "Loading start.el... OK")

    (require 'set-up-platform)
    (when-windows
     (require 'windows-setup))
    (load-library "set-up-environment-variables")
    (require 'set-up-paths)
    (load-library "set-up-tmp-paths")

    (require 'base-emacs-fixes)

    (require 'foreign-setup)

    (require 'cycle-on-lines)
    (require 'common)
    (require 'common-heavy)
    (require 'datastructures)

    (require 'backups)
    (require 'emacs-general-conf)
    (require 'hl-paren)
    (require 'mode-line-setup)

    (require 'dash)
    (require 'f)
    (require 's)
    (require 'v)

    (require 'prev-buffer-tracking)

    (require 'browse-kill-ring)
    (require 'company)
    (require 'company-mode-setup)
    (require 'el-patch)
    (require 'em-alias)
    (require 'em-banner)
    (require 'em-basic)
    (require 'em-dirs)
    (require 'em-glob)
    (require 'em-hist)
    (require 'em-ls)
    (require 'em-script)
    (require 'em-unix)
    (require 'em-xtra)
    (require 'eshell)
    (require 'flycheck)
    (require 'flycheck-rust)
    (require 'grep)
    (require 'ibuffer)
    (require 'ivy)
    ;; (require 'ivy-posframe)
    (require 'ivy-smex)
    (require 'lsp-mode)
    (require 'magit)
    (require 'markdown-mode)
    (require 'org)
    (require 'org-faces)
    (require 'org-indent)
    ;; (require 'posframe)
    (require 'ptx-mode)
    (require 'rainbow-delimiters)
    (require 'rust-mode)
    (require 'shell)
    (require 'smex)
    (require 'toml-mode)
    (require 'yasnippet)

    (require 'pcomplete)
    (require 'pcmpl-gnu)
    (require 'pcmpl-linux)
    (require 'pcmpl-rpm)
    (require 'pcmpl-unix)

    (require 'haskell-autoload)
    (require 'org-mode-autoload)
    (require 'rust-autoloads)

    (require 'c++-setup)
    (require 'comint-setup)
    (require 'completion-setup)
    (require 'compilation-setup)
    (require 'egrep)
    (require 'eproj)
    (require 'emacs-lisp-setup)
    (require 'eshell-setup)
    (require 'folding-setup)
    (require 'fortunes)
    (require 'git-setup)
    (require 'grep-setup)
    (require 'ibuffer-setup)
    (require 'ptx-mode-setup)
    (require 'org-mode-setup)
    (require 'render-formula)
    (require 'rust-setup)
    (require 'select-mode)
    (require 'solarized)
    (require 'spell)
    (require 'toml-mode-setup)
    (require 'typography-setup)
    ;; (require 'undo-tree-setup)

    (require 'shell-setup)
    (require 'shell-completion)

    (require 'register)
    (require 'vim-macs)
    (require 'vim-defs)
    (require 'vim-modes)
    (require 'vim-keymap)
    (require 'vim-compat)
    (require 'vim-undo)

    (require 'keys)
    (require 'vim-setup)

    (when-windows
     (require 'fakecygpty-setup))


    (provide 'already-dumped)

    (makunbound 'dumping)
    (fmakunbound 'dump-main)
    (delete-file dump-target)
    (dump-emacs-portable dump-target)

    (message "Success")

    (mapc (lambda (func)
            (remove-hook 'kill-emacs-hook func)
            (remove-hook 'kill-emacs-hook func t))
          '(icicle-command-abbrev-save
            emms-cache-save
            smex-save-to-file
            doc-view-save-pages-on-kill
            save-place-kill-emacs-hook
            backup-all-buffers
            persistent-store-flush-database
            company-statistics--maybe-save))))

;; Local Variables:
;; no-byte-compile: t
;; End:

;; dump.el ends here
