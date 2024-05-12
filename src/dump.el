;; dump.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created:  1 September 2020
;; Description:

(require 'cl)

(require 'dump-init)

(defvar trie-opt--global-cache)

(defun dump-main (emacs-dir dump-target)
  (setf dumping t)

  (unless emacs-dir
    (setf emacs-dir
          (let ((emacs-root (expand-file-name (getenv "EMACS_ROOT")))
                (default-emacs-dir (expand-file-name "~/.emacs.d")))
            (cond
              (emacs-root
               (progn
                 (cl-assert (file-directory-p emacs-root))
                 emacs-root))
              ((file-directory-p default-emacs-dir)
               default-emacs-dir)
              (t
               (error "EMACS_ROOT not defined and default emacs directory does not exist: %s" default-emacs-dir))))))
  (dolist (dir '("compiled" "src"))
    (let ((dir2 (concat emacs-dir "/" dir)))
      (cl-assert (file-directory-p dir2))
      (add-to-list 'load-path dir2)))
  (when (boundp 'native-comp-eln-load-path)
    (startup-redirect-eln-cache (concat emacs-dir "/compiled")))

  (let ((init-file
         (cl-find-if #'file-exists-p
                     (mapcan (lambda (x) (list (concat emacs-dir "/src/" x)
                                          (expand-file-name (concat "~/" x))))
                             '(".emacs")))))
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
    (load-library "persistent-store")
    (persistent-store-init)

    (require 'append-list)
    (require 'bimap)
    (require 'bisect)
    (require 'sorted-set)
    (require 'trie)

    (require 'backups)
    (require 'emacs-general-conf)
    (require 'hl-paren)
    (require 'mode-line-setup)

    (require 'local-autoloads)

    (require 'dash)
    (require 'f)
    (require 's)
    (require 'v)

    (require 'prev-buffer-tracking)

    (require 'messages-buffer-setup)

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
    (require 'isar-mode)
    (require 'ivy)
    ;; (require 'ivy-posframe)
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

    (require 'ebuf)

    (require 'pcomplete)
    (require 'pcmpl-gnu)
    (require 'pcmpl-linux)
    (require 'pcmpl-rpm)
    (require 'pcmpl-unix)

    (require 'haskell-autoload)
    ;; Emacs 29: unsupported object type in dump: weird pseudovector
    ;; (require 'org-mode-autoload)
    (require 'rust-autoloads)

    (require 'configurable-compilation)

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
    (require 'haskell-setup)
    (require 'ptx-mode-setup)
    (require 'org-mode-setup)
    (require 'render-formula)
    (require 'rust-setup)
    (require 'select-mode)
    (require 'solarized)
    (require 'spell)
    (require 'tar-mode-setup)
    (require 'toml-mode-setup)
    (require 'typography-setup)
    ;; (require 'undo-tree-setup)

    (require 'bkr)

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

    ;; So that all tries will be shared among themselves during dumping.
    (let ((trie-opt--global-cache (make-hash-table :test #'equal)))
      (require 'agda-abbrev+)
      (require 'c++-abbrev+)
      (require 'c-abbrev+)
      (require 'clojure-abbrev+)
      (require 'emacs-lisp-abbrev+)
      (require 'haskell-abbrev+)
      (require 'java-abbrev+)
      (require 'latex-abbrev+)
      (require 'octave-abbrev+)
      (require 'python-abbrev+)
      (require 'shell-script-abbrev+)

      (agda-abbrev+-make-abbrevs)
      (c++-abbrev+-make-abbrevs)
      (c-abbrev+-make-abbrevs)
      (clojure-abbrev+-make-abbrevs)
      (emacs-lisp-abbrev+-make-abbrevs)
      (haskell-abbrev+-make-abbrevs)
      (java-abbrev+-make-abbrevs)
      (latex-abbrev+-make-abbrevs)
      (octave-abbrev+-make-abbrevs)
      (python-abbrev+-make-abbrevs)
      (shell-script-abbrev+-make-abbrevs))

    (provide 'already-dumped)

    (makunbound 'dumping)
    (fmakunbound 'dump-main)
    (delete-file dump-target)

    (dump-emacs-portable dump-target)

    (mapc (lambda (func)
            (remove-hook 'kill-emacs-hook func)
            (remove-hook 'kill-emacs-hook func t))
          '(icicle-command-abbrev-save
            emms-cache-save
            smex-save-to-file
            doc-view-save-pages-on-kill
            save-place-kill-emacs-hook
            backup-all-buffers
            persistent-store-flush-database))))

;; Local Variables:
;; no-byte-compile: t
;; End:

;; dump.el ends here
