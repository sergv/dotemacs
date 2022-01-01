;; recompile.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 11 September 2012
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'cl-lib))

(require 'comp)

(defconst +ignored-files-re+
  (rx bol
      (or "third-party/yafolding.el/features/support/env.el"
          (seq (* any) "tests" (* any))
          (seq "third-party/"
               (or "lsp-mode"
                   "pkg-info")
               "/test/"
               (* any))

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
  (message "cl--optimize-speed = %s"
           (pp-to-string cl--optimize-speed))
  (message "cl--optimize-safety = %s"
           (pp-to-string cl--optimize-safety))
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

(defun recompile-main (emacs-dir k n)
  (cl-destructuring-bind
      (emacs-dir . init-file)
      (recompile-set-up-env emacs-dir)
    (message "[recompile.el] collecting *.el files")
    (let* ((local-dirs
            (find-elisp-dirs (concat emacs-dir "/src")))
           (third-party-dirs
            (append
             (list
              (concat emacs-dir "/native/fakecygpty"))
             (find-elisp-dirs (concat emacs-dir "/third-party")
                              set-up-paths--ignored-third-party-el-dirs-re)))
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
           (byte-compile-docstring-max-column
            200)
           ;; (byte-compile-warnings
           ;;  (cl-remove 'docstrings byte-compile-warning-types))
           )

      (message "[recompile.el] loading local *.el files")
      (dolist (file local-files)
        (require (intern (file-name-sans-extension (file-name-nondirectory file))))
        ;; (load-library file)
        )

      (if (and k n)
          (if (and (fboundp #'native-comp-available-p)
                   (native-comp-available-p))

              (let ((i 0))
                (message "[recompile.el] %s native-compiling files" k)
                (dolist (file (append local-files third-party-files))

                  (when (= k (mod i n))
                    (message "[recompile.el] %s native-compiling %s" k file)

                    (condition-case err
                        (let ((no-native-compile nil)
                              (byte-native-compiling t)
                              (byte-native-qualities nil)
                              ;; Batch compilation has memory leak thanks to libgccjit.
                              (comp-running-batch-compilation nil)
                              (native-comp-compiler-options '("-O2"))
                              (native-comp-driver-options '("-march=native")))
                          (native-compile file
                                          (comp-el-to-eln-filename file)))
                      (error
                       (message "[recompile.el] %s failed to native-compile %s: %s" k file (cdr err)))))

                  (cl-incf i))
                (message "[recompile.el] done native-compiling"))
            (message "[recompile.el] native compilation not available, skipping"))

        (progn
          (message "[recompile.el] byte-compiling files")
          (dolist (file (append local-files third-party-files))
            (message "[recompile.el] byte-compiling %s" file)
            (let ((target (concat file "c")))
              (if (file-exists-p target)
                  (message "[recompile.el] skipping %s - already compiled" file)
                (byte-compile-file file))))
          (message "[recompile.el] done byte-compiling")))))

  (recompile-disable-hooks))


;; Local Variables:
;; no-byte-compile: t
;; End:

;; recompile.el ends here
