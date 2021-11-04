;; recompile.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 11 September 2012
;; Description:

(eval-when-compile (require 'cl))

(defconst +ignored-files-re+
  (rx bol
      (or "third-party/yafolding.el/features/support/env.el"
          (seq (* any) "tests" (* any))
          (seq "src/"
               (or "dump.el"
                   "huffman.el"
                   "rb-tree.el"
                   "recompile.el"))
          )
      eol))

(defun recompile-disable-hooks ()
  (message "[recompile.el] disabling unsafe hooks")
  (mapc (lambda (func)
          (remove-hook 'kill-emacs-hook func)
          (remove-hook 'kill-emacs-hook func t))
        '(icicle-command-abbrev-save
          emms-cache-save
          smex-save-to-file
          doc-view-save-pages-on-kill
          save-place-kill-emacs-hook
          backup-all-buffers
          persistent-store-flush-database)))

(defun recompile-set-up-env (emacs-dir)
  (cl-assert emacs-dir)
  (cl-proclaim '(optimize (speed 3) (safety 0)))
  (setf comp-speed 2
        comp-debug 0
        comp-verbose 0)
  (message "cl--optimize-speed = %s"
           (pp-to-string cl--optimize-speed))
  (message "cl--optimize-safety = %s"
           (pp-to-string cl--optimize-safety))
  (message "comp-speed = %s, comp-debug = %s, comp-verbose = %s"
           (pp-to-string comp-speed)
           (pp-to-string comp-debug)
           (pp-to-string comp-verbose))
  (setf emacs-dir (expand-file-name (directory-file-name emacs-dir))
        gc-cons-threshold (* 50 1024 1024)
        gc-cons-percentage 0.1)

  (let ((init-file
         (find-if #'file-exists-p
                  (mapcan (lambda (x) (list (concat emacs-dir "/src/" x)
                                       (concat "~/" x)))
                          '(".emacs")))))

    ;; load init file to get path detection from set-up-paths.el
    (load-library init-file)
    (recompile-disable-hooks)

    (cons emacs-dir init-file)))

(defun recompile-main (emacs-dir)
  (destructuring-bind
      (emacs-dir . init-file)
      (recompile-set-up-env emacs-dir)
    (message "[recompile.el] collecting *.el files")
    (let* ((local-dirs
            (find-elisp-dirs (concat emacs-dir "/src")))
           (third-party-dirs
            (find-elisp-dirs (concat emacs-dir "/third-party")
                             set-up-paths--ignored-third-party-el-dirs-re))
           (extra-files (list init-file))
           (dir-el-files
            (lambda (dir)
              (directory-files dir
                               t ;; produce full names
                               "^.*\\.el\\'"
                               nil ;; do sort
                               )))
           (should-not-recompile-p
            (lambda (x)
              (let ((fname (file-name-nondirectory x))
                    (rel-name (file-relative-name x emacs-dir)))
                (or (string-match-p +ignored-files-re+ rel-name)
                    ;; (string-match-p "^ob-.*\\.el$" fname)
                    (string-match-p "^\\..*el$" fname)))))
           (local-files
            (cl-remove-if should-not-recompile-p
                          (mapcan dir-el-files local-dirs)))
           (third-party-files
            (cl-remove-if should-not-recompile-p
                          (append extra-files
                                  (mapcan dir-el-files third-party-dirs))))
           ;; (byte-compile-warning-types
           ;;  '(redefine callargs free-vars unresolved obsolete noruntime
           ;;             interactive-only make-local mapcar
           ;;             constants suspicious lexical))
           )

      (message "[recompile.el] loading local *.el files")
      (dolist (file local-files)
        (require (intern (file-name-sans-extension (file-name-nondirectory file))))
        ;; (load-library file)
        )

      (message "[recompile.el] recompiling files")
      (dolist (file (append local-files third-party-files))
        (message "[recompile.el] byte-compiling %s" file)
        (byte-compile-file file))))

  (message "[recompile.el] done")

  (recompile-disable-hooks))

(defun recompile-native (emacs-dir)

  (require 'comp)

  (setf emacs-dir (car (recompile-set-up-env emacs-dir)))

  (let ((files-to-recompile
         (list "src/vim-mode/vim-compat.el"
               "src/vim-mode/vim-core.el"
               "src/vim-mode/vim-defs.el"
               "src/vim-mode/vim-ex.el"
               "src/vim-mode/vim-keymap.el"
               "src/vim-mode/vim-macs.el"
               "src/vim-mode/vim-modes.el"
               "src/vim-setup.el"

               "third-party/attrap/attrap.el"
               "third-party/browse-kill-ring/browse-kill-ring.el"

               "third-party/company-mode/company-abbrev.el"
               "third-party/company-mode/company-bbdb.el"
               "third-party/company-mode/company-capf.el"
               "third-party/company-mode/company-clang.el"
               "third-party/company-mode/company-cmake.el"
               "third-party/company-mode/company-css.el"
               "third-party/company-mode/company-dabbrev-code.el"
               "third-party/company-mode/company-dabbrev.el"
               "third-party/company-mode/company-eclim.el"
               "third-party/company-mode/company.el"
               "third-party/company-mode/company-elisp.el"
               "third-party/company-mode/company-etags.el"
               "third-party/company-mode/company-files.el"
               "third-party/company-mode/company-gtags.el"
               "third-party/company-mode/company-ispell.el"
               "third-party/company-mode/company-keywords.el"
               "third-party/company-mode/company-nxml.el"
               "third-party/company-mode/company-oddmuse.el"
               "third-party/company-mode/company-semantic.el"
               "third-party/company-mode/company-template.el"
               "third-party/company-mode/company-tempo.el"
               "third-party/company-mode/company-tests.el"
               "third-party/company-mode/company-tng.el"
               "third-party/company-mode/company-xcode.el"
               "third-party/company-mode/company-yasnippet.el"

               "third-party/dash.el/dash.el"
               "third-party/dash.el/dash-functional.el"
               "third-party/epl/epl.el"
               "third-party/flx/flx.el"
               "third-party/f.el/f.el"
               "third-party/flycheck/flycheck.el"
               "third-party/glsl-mode/glsl-mode.el"
               "third-party/haskell-mode/haskell-compile.el"
               "third-party/haskell-mode/haskell-indentation.el"
               "third-party/haskell-mode/haskell-mode.el"
               "third-party/haskell-mode/haskell-sort-imports.el"
               "third-party/ivy/ivy.el"
               "third-party/ivy/colir.el"
               "third-party/ivy/counsel.el"
               "third-party/ivy/ivy-overlay.el"
               "third-party/ivy-smex/ivy-smex.el"
               "third-party/js2-mode/js2-mode.el"

               "third-party/lcr/lcr.el"
               "third-party/lean-mode/company-lean.el"
               "third-party/lean-mode/helm-lean.el"
               "third-party/lean-mode/lean-debug.el"
               "third-party/lean-mode/lean-dev.el"
               "third-party/lean-mode/lean-eri.el"
               "third-party/lean-mode/lean-flycheck.el"
               "third-party/lean-mode/lean-hole.el"
               "third-party/lean-mode/lean-info.el"
               "third-party/lean-mode/lean-input.el"
               "third-party/lean-mode/lean-leanpkg.el"
               "third-party/lean-mode/lean-message-boxes.el"
               "third-party/lean-mode/lean-mode.el"
               "third-party/lean-mode/lean-right-click.el"
               "third-party/lean-mode/lean-server.el"
               "third-party/lean-mode/lean-settings.el"
               "third-party/lean-mode/lean-syntax.el"
               "third-party/lean-mode/lean-type.el"
               "third-party/lean-mode/lean-util.el"

               "third-party/lua-mode/lua-mode.el"

               "third-party/magit/lisp/magit-apply.el"
               "third-party/magit/lisp/magit-autorevert.el"
               "third-party/magit/lisp/magit-bisect.el"
               "third-party/magit/lisp/magit-blame.el"
               "third-party/magit/lisp/magit-bookmark.el"
               "third-party/magit/lisp/magit-branch.el"
               "third-party/magit/lisp/magit-collab.el"
               "third-party/magit/lisp/magit-commit.el"
               "third-party/magit/lisp/magit-core.el"
               "third-party/magit/lisp/magit-diff.el"
               "third-party/magit/lisp/magit-ediff.el"
               "third-party/magit/lisp/magit.el"
               "third-party/magit/lisp/magit-extras.el"
               "third-party/magit/lisp/magit-files.el"
               "third-party/magit/lisp/magit-git.el"
               "third-party/magit/lisp/magit-imenu.el"
               "third-party/magit/lisp/magit-log.el"
               "third-party/magit/lisp/magit-margin.el"
               "third-party/magit/lisp/magit-merge.el"
               "third-party/magit/lisp/magit-mode.el"
               "third-party/magit/lisp/magit-notes.el"
               "third-party/magit/lisp/magit-obsolete.el"
               "third-party/magit/lisp/magit-pkg.el"
               "third-party/magit/lisp/magit-process.el"
               "third-party/magit/lisp/magit-refs.el"
               "third-party/magit/lisp/magit-remote.el"
               "third-party/magit/lisp/magit-repos.el"
               "third-party/magit/lisp/magit-reset.el"
               "third-party/magit/lisp/magit-section.el"
               "third-party/magit/lisp/magit-sequence.el"
               "third-party/magit/lisp/magit-stash.el"
               "third-party/magit/lisp/magit-status.el"
               "third-party/magit/lisp/magit-submodule.el"
               "third-party/magit/lisp/magit-subtree.el"
               "third-party/magit/lisp/magit-tag.el"
               "third-party/magit/lisp/magit-utils.el"
               "third-party/magit/lisp/magit-wip.el"
               "third-party/magit/lisp/magit-worktree.el"
               "third-party/magit-popup/magit-popup.el"

               "third-party/markdown-mode/markdown-mode.el"
               "third-party/misc-modes/clang-format.el"
               "third-party/misc-modes/llvm-mode.el"
               "third-party/misc-modes/typopunct.el"

               "third-party/org-mode/lisp/org.el"
               "third-party/org-mode/lisp/org-footnote.el"
               "third-party/org-mode/lisp/org-macro.el"
               "third-party/org-mode/lisp/org-list.el"
               "third-party/org-mode/lisp/org-lint.el"
               "third-party/org-mode/lisp/org-protocol.el"
               "third-party/org-mode/lisp/org-src.el"
               "third-party/paredit/paredit.el"
               "third-party/popup-el/popup.el"
               "third-party/prop-menu-el/prop-menu.el"
               "third-party/ptx-mode/ptx-mode.el"
               "third-party/rainbow-delimiters/rainbow-delimiters.el"
               "third-party/rust-mode/rust-mode.el"
               "third-party/s.el/s.el"
               "third-party/smartparens/smartparens.el"
               "third-party/smex/smex.el"
               "third-party/sml-mode/sml-mode.el"
               "third-party/toml-mode/toml-mode.el"
               "third-party/tuareg/tuareg.el"
               "third-party/undo-tree/undo-tree.el"
               "third-party/yafolding.el/yafolding.el"
               "third-party/yasnippet/yasnippet.el")
         ;; (list "src/abbrev+.el"
         ;;       "src/buffer-switching.el"
         ;;       "src/cc-setup.el"
         ;;       "src/c-like-setup.el"
         ;;       "src/cmake-setup.el"
         ;;       "src/comint-setup.el"
         ;;       "src/comment-util.el"
         ;;       "src/common.el"
         ;;       "src/common-heavy.el"
         ;;       "src/company-mode-setup.el"
         ;;       "src/compilation-navigation.el"
         ;;       "src/custom/cycle-on-lines.el"
         ;;       "src/completion-setup.el"
         ;;       "src/configurable-compilation.el"
         ;;       "src/c++-setup.el"
         ;;       "src/c-setup.el"
         ;;       "src/cuda-setup.el"
         ;;       "src/datastructures.el"
         ;;       "src/ediff-setup.el"
         ;;       "src/egrep.el"
         ;;       "src/emacs-general-conf.el"
         ;;       "src/eproj/company-eproj.el"
         ;;       "src/eproj/eproj-ctags.el"
         ;;       "src/eproj/eproj-customization.el"
         ;;       "src/eproj/eproj-haskell.el"
         ;;       "src/eproj/eproj-query.el"
         ;;       "src/eproj/eproj-symbnav.el"
         ;;       "src/eproj/eproj-tag-index.el"
         ;;       "src/eproj/eproj.el"
         ;;       "src/find-files.el"
         ;;       "src/fortunes.el"
         ;;       "src/gitconfig-mode.el"
         ;;       "src/gitignore-mode.el"
         ;;       "src/git-setup.el"
         ;;       "src/glsl-setup.el"
         ;;       "src/haskell/dante-repl.el"
         ;;       "src/haskell/haskell-block-indent.el"
         ;;       "src/haskell/haskell-compilation-commands.el"
         ;;       "src/haskell/haskell-misc.el"
         ;;       "src/haskell/haskell-outline.el"
         ;;       "src/haskell/haskell-setup.el"
         ;;       "src/haskell/haskell-smart-operators-mode.el"
         ;;       "src/haskell/uuag-mode.el"
         ;;       "src/hideshow-setup.el"
         ;;       "src/hl-paren.el"
         ;;       "src/hl-tags-mode.el"
         ;;       "src/indentation.el"
         ;;       "src/java-setup.el"
         ;;       "src/lisp/align-let.el"
         ;;       "src/lisp/emacs-lisp-highlight.el"
         ;;       "src/lisp/emacs-lisp-setup.el"
         ;;       "src/lisp/general-lisp-setup.el"
         ;;       "src/llvm-setup.el"
         ;;       "src/mode-line-setup.el"
         ;;       "src/org-mode-setup.el"
         ;;       "src/paredit-setup.el"
         ;;       "src/render-formula.el"
         ;;       "src/rust/rust-compilation-commands.el"
         ;;       "src/rust/rust-setup.el"
         ;;       "src/rust/rust-smart-operators.el"
         ;;       "src/select-mode.el"
         ;;       "src/select-mode-setup.el"
         ;;       "src/shell/shell-completion.el"
         ;;       "src/shell/shell-setup.el"
         ;;       "src/smart-operators-utils.el"
         ;;       "src/smartparens-setup.el"
         ;;       "src/sml-setup.el"
         ;;       "src/solarized.el"
         ;;       "src/toml-setup.el"
         ;;       "src/undo-tree-setup.el"
         ;;       "src/vim-mode/vim-compat.el"
         ;;       "src/vim-mode/vim-core.el"
         ;;       "src/vim-mode/vim-defs.el"
         ;;       "src/vim-mode/vim-ex.el"
         ;;       "src/vim-mode/vim-keymap.el"
         ;;       "src/vim-mode/vim-macs.el"
         ;;       "src/vim-mode/vim-modes.el"
         ;;       "src/vim-setup.el"
         ;;
         ;;       "third-party/attrap/attrap.el"
         ;;       "third-party/browse-kill-ring/browse-kill-ring.el"
         ;;
         ;;       "third-party/company-mode/company-abbrev.el"
         ;;       "third-party/company-mode/company-bbdb.el"
         ;;       "third-party/company-mode/company-capf.el"
         ;;       "third-party/company-mode/company-clang.el"
         ;;       "third-party/company-mode/company-cmake.el"
         ;;       "third-party/company-mode/company-css.el"
         ;;       "third-party/company-mode/company-dabbrev-code.el"
         ;;       "third-party/company-mode/company-dabbrev.el"
         ;;       "third-party/company-mode/company-eclim.el"
         ;;       "third-party/company-mode/company.el"
         ;;       "third-party/company-mode/company-elisp.el"
         ;;       "third-party/company-mode/company-etags.el"
         ;;       "third-party/company-mode/company-files.el"
         ;;       "third-party/company-mode/company-gtags.el"
         ;;       "third-party/company-mode/company-ispell.el"
         ;;       "third-party/company-mode/company-keywords.el"
         ;;       "third-party/company-mode/company-nxml.el"
         ;;       "third-party/company-mode/company-oddmuse.el"
         ;;       "third-party/company-mode/company-semantic.el"
         ;;       "third-party/company-mode/company-template.el"
         ;;       "third-party/company-mode/company-tempo.el"
         ;;       "third-party/company-mode/company-tests.el"
         ;;       "third-party/company-mode/company-tng.el"
         ;;       "third-party/company-mode/company-xcode.el"
         ;;       "third-party/company-mode/company-yasnippet.el"
         ;;
         ;;       "third-party/dash.el/dash.el"
         ;;       "third-party/dash.el/dash-functional.el"
         ;;       "third-party/epl/epl.el"
         ;;       "third-party/flx/flx.el"
         ;;       "third-party/f.el/f.el"
         ;;       "third-party/flycheck/flycheck.el"
         ;;       "third-party/glsl-mode/glsl-mode.el"
         ;;       "third-party/haskell-mode/haskell-compile.el"
         ;;       "third-party/haskell-mode/haskell-indentation.el"
         ;;       "third-party/haskell-mode/haskell-mode.el"
         ;;       "third-party/haskell-mode/haskell-sort-imports.el"
         ;;       "third-party/ivy/ivy.el"
         ;;       "third-party/ivy/colir.el"
         ;;       "third-party/ivy/counsel.el"
         ;;       "third-party/ivy/ivy-overlay.el"
         ;;       "third-party/ivy-smex/ivy-smex.el"
         ;;       "third-party/js2-mode/js2-mode.el"
         ;;
         ;;       "third-party/lcr/lcr.el"
         ;;       "third-party/lean-mode/company-lean.el"
         ;;       "third-party/lean-mode/helm-lean.el"
         ;;       "third-party/lean-mode/lean-debug.el"
         ;;       "third-party/lean-mode/lean-dev.el"
         ;;       "third-party/lean-mode/lean-eri.el"
         ;;       "third-party/lean-mode/lean-flycheck.el"
         ;;       "third-party/lean-mode/lean-hole.el"
         ;;       "third-party/lean-mode/lean-info.el"
         ;;       "third-party/lean-mode/lean-input.el"
         ;;       "third-party/lean-mode/lean-leanpkg.el"
         ;;       "third-party/lean-mode/lean-message-boxes.el"
         ;;       "third-party/lean-mode/lean-mode.el"
         ;;       "third-party/lean-mode/lean-right-click.el"
         ;;       "third-party/lean-mode/lean-server.el"
         ;;       "third-party/lean-mode/lean-settings.el"
         ;;       "third-party/lean-mode/lean-syntax.el"
         ;;       "third-party/lean-mode/lean-type.el"
         ;;       "third-party/lean-mode/lean-util.el"
         ;;
         ;;       "third-party/lua-mode/lua-mode.el"
         ;;
         ;;       "third-party/magit/lisp/magit-apply.el"
         ;;       "third-party/magit/lisp/magit-autorevert.el"
         ;;       "third-party/magit/lisp/magit-bisect.el"
         ;;       "third-party/magit/lisp/magit-blame.el"
         ;;       "third-party/magit/lisp/magit-bookmark.el"
         ;;       "third-party/magit/lisp/magit-branch.el"
         ;;       "third-party/magit/lisp/magit-collab.el"
         ;;       "third-party/magit/lisp/magit-commit.el"
         ;;       "third-party/magit/lisp/magit-core.el"
         ;;       "third-party/magit/lisp/magit-diff.el"
         ;;       "third-party/magit/lisp/magit-ediff.el"
         ;;       "third-party/magit/lisp/magit.el"
         ;;       "third-party/magit/lisp/magit-extras.el"
         ;;       "third-party/magit/lisp/magit-files.el"
         ;;       "third-party/magit/lisp/magit-git.el"
         ;;       "third-party/magit/lisp/magit-imenu.el"
         ;;       "third-party/magit/lisp/magit-log.el"
         ;;       "third-party/magit/lisp/magit-margin.el"
         ;;       "third-party/magit/lisp/magit-merge.el"
         ;;       "third-party/magit/lisp/magit-mode.el"
         ;;       "third-party/magit/lisp/magit-notes.el"
         ;;       "third-party/magit/lisp/magit-obsolete.el"
         ;;       "third-party/magit/lisp/magit-pkg.el"
         ;;       "third-party/magit/lisp/magit-process.el"
         ;;       "third-party/magit/lisp/magit-refs.el"
         ;;       "third-party/magit/lisp/magit-remote.el"
         ;;       "third-party/magit/lisp/magit-repos.el"
         ;;       "third-party/magit/lisp/magit-reset.el"
         ;;       "third-party/magit/lisp/magit-section.el"
         ;;       "third-party/magit/lisp/magit-sequence.el"
         ;;       "third-party/magit/lisp/magit-stash.el"
         ;;       "third-party/magit/lisp/magit-status.el"
         ;;       "third-party/magit/lisp/magit-submodule.el"
         ;;       "third-party/magit/lisp/magit-subtree.el"
         ;;       "third-party/magit/lisp/magit-tag.el"
         ;;       "third-party/magit/lisp/magit-utils.el"
         ;;       "third-party/magit/lisp/magit-wip.el"
         ;;       "third-party/magit/lisp/magit-worktree.el"
         ;;       "third-party/magit-popup/magit-popup.el"
         ;;
         ;;       "third-party/markdown-mode/markdown-mode.el"
         ;;       "third-party/misc-modes/clang-format.el"
         ;;       "third-party/misc-modes/llvm-mode.el"
         ;;       "third-party/misc-modes/typopunct.el"
         ;;
         ;;       "third-party/org-mode/lisp/org.el"
         ;;       "third-party/org-mode/lisp/org-footnote.el"
         ;;       "third-party/org-mode/lisp/org-macro.el"
         ;;       "third-party/org-mode/lisp/org-list.el"
         ;;       "third-party/org-mode/lisp/org-lint.el"
         ;;       "third-party/org-mode/lisp/org-protocol.el"
         ;;       "third-party/org-mode/lisp/org-src.el"
         ;;       "third-party/paredit/paredit.el"
         ;;       "third-party/popup-el/popup.el"
         ;;       "third-party/prop-menu-el/prop-menu.el"
         ;;       "third-party/ptx-mode/ptx-mode.el"
         ;;       "third-party/rainbow-delimiters/rainbow-delimiters.el"
         ;;       "third-party/rust-mode/rust-mode.el"
         ;;       "third-party/s.el/s.el"
         ;;       "third-party/smartparens/smartparens.el"
         ;;       "third-party/smex/smex.el"
         ;;       "third-party/sml-mode/sml-mode.el"
         ;;       "third-party/toml-mode/toml-mode.el"
         ;;       "third-party/tuareg/tuareg.el"
         ;;       "third-party/undo-tree/undo-tree.el"
         ;;       "third-party/yafolding.el/yafolding.el"
         ;;       "third-party/yasnippet/yasnippet.el")

         )

        ;; (byte-compile-warning-types
        ;;  '(redefine callargs free-vars unresolved obsolete noruntime
        ;;             interactive-only make-local mapcar
        ;;             constants suspicious lexical))
        )
    (message "[recompile.el] running native compiler")
    (setf comp-src-pool (--filter (not (file-exists-p (concat it "n"))) files-to-recompile))
    ;; (message "comp-src-pool = %s"
    ;;          (pp-to-string comp-src-pool))
    (cl-loop repeat 5
             do (comp-start-async-worker))
    (message "[recompile.el] native compilation started")

    (while (not (null comp-prc-pool))
      (sleep-for 0.1)))

  (message "[recompile.el] done")

  (recompile-disable-hooks))


;; Local Variables:
;; no-byte-compile: t
;; End:

;; recompile.el ends here
