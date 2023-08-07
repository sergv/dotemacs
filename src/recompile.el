;; recompile.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 11 September 2012
;; Description:

(eval-when-compile
  (require 'cl))

(require 'comp nil t)

(defconst +ignored-files-re+
  (rx (or "yafolding.el/features/support/env.el"
          "yafolding.el/features/step-definitions/yafolding-steps.el"
          "dtrt-indent/dtrt-indent-test.el"
          "hydra/hydra-test.el"
          "ivy/ivy-test.el"
          "rainbow-delimiters/rainbow-delimiters-test.el"
          "paredit/test.el"
          "company-statistics/company-statistics-tests.el"
          "company-mode/company-tests.el"
          "csv-mode/csv-mode-tests.el"
          "flx/misc/flx-test-list.el"
          "flycheck-elm/flycheck-elm-tests.el"
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

(defun recompile-main (emacs-dir k n compile-native? config)
  (cl-assert (numberp k))
  (cl-assert (numberp n))
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

      (when config
        (with-temp-buffer
          (dolist (entry '((no-native-compile nil)
                           (byte-native-compiling t)
                           (byte-native-qualities nil)
                           ;; Batch compilation has memory leak thanks to libgccjit.
                           (comp-running-batch-compilation nil)
                           (native-comp-debug 0)
                           (native-comp-compiler-options '("-O2"))
                           (native-comp-driver-options '("-march=native"))))
            (insert (format "(setf %s %S)\n" (car entry) (cadr entry))))
          (insert (format "(setf load-path '%S)" load-path))

          (write-file config)
          (message "WRITTEN CONFIG TO %S" config)))

      (if proceed?
          (progn
            (message "[recompile.el] loading local *.el files")
            (dolist (file local-files)
              (require (intern (file-name-sans-extension (file-name-nondirectory file))))
              ;; (load-library file)
              )

            (let ((i 0))
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
                                  (native-comp-compiler-options '("-O2"))
                                  (native-comp-driver-options '("-march=native")))
                              (native-compile file
                                              (comp-el-to-eln-filename file)))
                          (error
                           (message "[recompile.el] %s failed to native-compile %s: %s" k file (cdr err)))))

                    (progn
                      (message "[recompile.el] %s byte-compiling %s" k file)
                      (let ((target (concat file "c")))
                        (if (file-exists-p target)
                            (message "[recompile.el] %s skipping %s - already compiled" k file)
                          (byte-compile-file file))))))
                (cl-incf i))

              (message "[recompile.el] %s done %s" k (if compile-native? "native-compiling" "byte-compiling"))))
        (message "[recompile.el] %s nothing to do" k))))

  (recompile-disable-hooks))


;; Local Variables:
;; no-byte-compile: t
;; End:

;; recompile.el ends here
