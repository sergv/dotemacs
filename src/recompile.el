;; recompile.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 11 September 2012
;; Description:

(eval-when-compile
  (require 'cl))

(require 'comp nil t)

(let ((load-path (cons (file-name-directory load-file-name) load-path)))
  (require 'recompile-init))

(defconst +ignored-files-re+
  (rx (or "yafolding.el/features/support/env.el"
          "yafolding.el/features/step-definitions/yafolding-steps.el"
          "dtrt-indent/dtrt-indent-test.el"
          "hydra/hydra-test.el"
          "ivy/ivy-test.el"
          "rainbow-delimiters/rainbow-delimiters-test.el"
          "paredit/test.el"
          "company-mode/company-tests.el"
          "csv-mode/csv-mode-tests.el"
          "flx/misc/flx-test-list.el"
          "prop-menu-el/prop-menu-tests.el"
          "rust-mode/rust-mode-tests.el"
          "tuareg/tuareg-tests.el"
          "v.el/v-tests.el"
          "yasnippet/yasnippet-tests.el"

          (seq "ivy/targets/" (* any))
          (seq "org-mode/testing/" (* any))

          (seq (* any) "/tests/" (* any))
          (seq "third-party/"
               (or "clojure-mode"
                   "epl"
                   "flx"
                   "haskell-mode"
                   "ivy"
                   "lsp-mode"
                   "lsp-ui"
                   "pkg-info"
                   "poly-mode"
                   "treepy.el")
               (or "/test/"
                   "/tests/")
               (* any))

          (seq "src/"
               (or "dump.el"
                   "huffman.el"
                   "rb-tree.el"
                   "recompile.el")))
      eos))

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

(defun fix-comp-delete-or-replace-file! ()
  (require 'el-patch)

  (el-patch-defun comp-delete-or-replace-file (oldfile &optional newfile)
    "Replace OLDFILE with NEWFILE.
When NEWFILE is nil just delete OLDFILE.
Takes the necessary steps when dealing with OLDFILE being a
shared library that might be currently loaded into a running Emacs
session."
    (cond ((eq 'windows-nt system-type)
           (ignore-errors (delete-file oldfile))
           (while
               (condition-case _
                   (progn
                     ;; oldfile maybe recreated by another Emacs in
                     ;; between the following two rename-file calls
                     (if (file-exists-p oldfile)
                         (rename-file oldfile (make-temp-file-internal
                                               (file-name-sans-extension oldfile)
                                               nil ".eln.old" nil)
                                      t))
                     (when newfile
                       (rename-file newfile oldfile nil))
                     ;; Keep on trying.
                     nil)
                 (file-already-exists
                  ;; Done
                  t))))
          ;; Remove the old eln instead of copying the new one into it
          ;; to get a new inode and prevent crashes in case the old one
          ;; is currently loaded.
          (t (delete-file oldfile)
             (when newfile
               (el-patch-swap
                 (rename-file newfile oldfile)
                 ;; Concurrent compilation process may have written
                 ;; the file after we just deleted it.
                 (rename-file newfile oldfile t)))))))

(defun recompile-set-up-env (emacs-dir)
  (cl-assert emacs-dir)
  (cl-proclaim '(optimize (speed 3) (safety 0)))
  (message "cl--optimize-speed = %s"
           (pp-to-string cl--optimize-speed))
  (message "cl--optimize-safety = %s"
           (pp-to-string cl--optimize-safety))
  (setf emacs-dir (expand-file-name (directory-file-name emacs-dir))
        gc-cons-threshold (* 50 1024 1024)
        gc-cons-percentage 0.1)

  (let ((init-file
         (find-if #'file-exists-p
                  (mapcan (lambda (x) (list (concat emacs-dir "/" x)
                                       (concat "~/.emacs.d/" x)))
                          '("init.el")))))

    ;; load init file to get path detection from set-up-paths.el
    (load-library init-file)
    (recompile-disable-hooks)

    (fix-comp-delete-or-replace-file!)

    (cons emacs-dir init-file)))

(defun recompile-main (emacs-dir compilation-elc-dest-dir k n compile-native? preload? zipped-el-dest)
  (cl-assert (numberp k))
  (cl-assert (numberp n))
  (unwind-protect
      (cl-destructuring-bind
          (emacs-dir . init-file)
          (recompile-set-up-env emacs-dir)
        (message "[recompile.el] collecting *.el files")
        (let* ((local-dirs
                (find-elisp-dirs (concat emacs-dir "/src")))
               (third-party-dirs
                (append
                 (when-windows
                  (list
                   (concat emacs-dir "/native/fakecygpty")))
                 (find-elisp-dirs (concat emacs-dir "/third-party")
                                  ;; Keep in sync with ‘set-up-tmp-paths.el’
                                  (rx
                                   (or (seq bow (or "test" "tests" "doc" "examples" ".cask" ".stack-work.*") eol)
                                       "auctex/tests"
                                       "auctex/style"
                                       "clojure-mode/test"
                                       "company-mode/test"
                                       "dash.el/dev"
                                       (seq "f.el/" (or "bin" "test"))
                                       "flycheck-haskell/test"
                                       (seq "flycheck/" (or ".cask" "test"))
                                       "ivy/targets"
                                       "groovy-mode/test"
                                       (seq "haskell-mode/" (or "doc/gifcasts" "tests" "tests/compat"))
                                       "ht/test"
                                       "js2-mode/tests"
                                       "kotlin-ts-mode/test"
                                       "lua-mode/test"
                                       "magit/t"
                                       "markdown-mode/tests"
                                       "markdown-mode/scripts"
                                       "nix-ts-mode/test"
                                       (seq "org-mode/" (or "mk" "testing"))
                                       "pkg-info/test"
                                       "s.el/dev"
                                       "treepy.el/test"
                                       "transient/test")))))
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
                        (string-match-p "^\\..*el$" fname)))))
               (local-files
                (cl-remove-if should-not-recompile-p
                              (mapcan dir-el-files local-dirs)))
               (third-party-files
                (cl-remove-if should-not-recompile-p
                              (append extra-files
                                      (mapcan dir-el-files third-party-dirs))))
               (byte-compile-docstring-max-column
                200)
               ;; (byte-compile-warnings
               ;;  (cl-remove 'docstrings byte-compile-warning-types))

               (native-comp-available? (and (fboundp #'native-comp-available-p)
                                            (native-comp-available-p)))
               (proceed? (or (not compile-native?)
                             native-comp-available?)))

          (cond
            (preload?
             ;; Preloading finished, exit now.
             )

            (proceed?
             (message "[recompile.el] loading local *.el files")
             (dolist (file local-files)
               (let ((compile--in-progress t))
                 (require (intern (file-name-sans-extension (file-name-nondirectory file)))))
               ;; (load-library file)
               )

             (let ((i 0)
                   (byte-compile-dest-file-function
                    (lambda (path)
                      (elisp-compile-get-elc-destination path compilation-elc-dest-dir)))
                   (emacs-dir-with-trailing-slash
                    (concat emacs-dir "/")))
               (message "[recompile.el] %s %s files" k (if compile-native? "native-compiling" "byte-compiling"))
               (dolist (file (append local-files third-party-files))
                 (when (= k (mod i n))
                   (if compile-native?
                       (progn
                         (message "[recompile.el] %s native-compiling %s" k file)

                         (condition-case err
                             (let ((no-native-compile nil)
                                   (byte-native-compiling t)
                                   (byte-native-qualities nil)
                                   ;; Batch compilation has memory leak thanks to libgccjit.
                                   (comp-running-batch-compilation nil)
                                   (native-comp-debug 0)
                                   ;; (native-comp-compiler-options '("-O2"))
                                   ;; (native-comp-driver-options '("-march=native"))
                                   )
                               (native-compile file
                                               (comp-el-to-eln-filename file)))
                           (error
                            (message "[recompile.el] %s failed to native-compile %s: %s" k file (cdr err)))))

                     (progn
                       (message "[recompile.el] %s byte-compiling %s" k file)
                       (let ((target (concat file "c")))
                         (if (file-exists-p target)
                             (message "[recompile.el] %s skipping %s - already compiled" k file)
                           (byte-compile-file file))
                         (when zipped-el-dest
                           (let* ((dest-base
                                   (concat zipped-el-dest "/" (strip-string-prefix emacs-dir-with-trailing-slash file)))
                                  (dest (concat dest-base ".gz")))
                             (with-temp-buffer
                               (if (zerop
                                    (call-process "gzip"
                                                  file
                                                  (current-buffer)
                                                  nil
                                                  "--best"
                                                  "--stdout"))
                                   (progn
                                     (make-directory (file-name-directory dest) t)
                                     (unless (string= "init.el" (file-name-nondirectory file))
                                       (jka-compr-run-real-handler
                                        'write-region
                                        (list (point-min) (point-max) dest))))
                                 (error "Failed to compress source ‘%s’ to ‘%s’:\n%s" file dest (buffer-substring-no-properties (point-min) (point-max)))))))))))
                 (cl-incf i))

               (message "[recompile.el] %s done %s" k (if compile-native? "native-compiling" "byte-compiling"))))

            (t
             (message "[recompile.el] %s nothing to do" k)))))

    (recompile-disable-hooks)))


;; Local Variables:
;; no-byte-compile: t
;; End:

;; recompile.el ends here
