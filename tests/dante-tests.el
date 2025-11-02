;; dante-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  8 October 2025
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'set-up-platform))

(require 'common)
(require 'ert)
(require 'tests-utils)

(defconst dante-tests-tests/simple-check-test-project
  (concat +emacs-config-path+ "/tests/test-data/dante/simple-check-project"))

(defconst dante-tests-tests/simple-test-project-arcive
  (concat +emacs-config-path+ "/tests/test-data/dante/simple-check-project.zip"))

(defconst dante-tests-tests/simple-repl-test-project
  (concat +emacs-config-path+ "/tests/test-data/dante/simple-repl-project"))

(defconst dante-tests-tests/simple-check-test-project-with-hsc-archive
  (concat +emacs-config-path+ "/tests/test-data/dante/simple-check-project-with-hsc.zip"))

(defconst dante-tests-tests/simple-repl-test-project-with-hsc
  (concat +emacs-config-path+ "/tests/test-data/dante/simple-repl-project-with-hsc"))

(defmacro dante-tests/with-file (path &rest body)
  (declare (indent 1))
  (let ((buf-var '#:buf))
    `(let ((noninteractive nil))
       (with-fresh-buffer-no-switch
           ,buf-var
           (find-file-noselect ,path)
         (unwind-protect
             (with-current-buffer ,buf-var
               ,@body)
           (when (buffer-live-p ,buf-var)
             (awhen (get-buffer (dante-buffer-name (dante-get-config ,buf-var)))
               (when (buffer-live-p it)
                 (kill-buffer it)))
             (awhen (get-buffer (dante-repl-buffer-name ,buf-var))
               (when (buffer-live-p it)
                 (kill-buffer it)))
             (kill-buffer ,buf-var)))))))

(defmacro dante-tests/check-buffer-and-assert-when-done (&rest body)
  `(let* ((checking-done nil)
          (check-func
           (lambda ()
             (setf checking-done t))))

     (add-hook 'flycheck-after-syntax-check-hook check-func nil t)

     (haskell-flycheck-force-run)
     ;; (flycheck-buffer)

     (while (not checking-done)
       (sit-for 0.05))

     (remove-hook 'flycheck-after-syntax-check-hook check-func t)

     (progn
       ,@body)))

(defun dante-repl/wait-for-prompt (proc)
  "Spin in a loop until prompt dante-repl prompt shows up befor epoint."
  (cl-assert (processp proc))
  (accept-process-output proc nil nil t)
  (let ((p (point)))
    (while (not (and (eq (char-before p) ?\s)
                     (when-let ((p2 (char-before (- p 1))))
                       (or (eq p2 ?\4)
                           (eq p2 ?\5)))))
      (accept-process-output proc nil nil t)
      (setf p (point)))))

(defun dante-repl/wait-for-modules-loaded (proc)
  "Spin in a loop until prompt dante-repl prompt shows up befor epoint."
  (cl-assert (processp proc))
  (accept-process-output proc nil nil t)
  (while (save-excursion
           (goto-char (line-beginning-position 0))
           (not (or (looking-at-p "Ok, modules loaded:.*\\.$")
                    (looking-at-p "Failed, modules loaded: none\\.$"))))
    (accept-process-output proc nil nil t)))

(ert-deftest dante-tests/simple-check-project-1 ()
  (unless (executable-find "cabal")
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))

  (dante-tests/with-file
      (concat dante-tests-tests/simple-check-test-project "/src/Foo.hs")
    ;; (find-file (concat dante-tests-tests/simple-check-test-project "/src/Foo.hs"))
    (should (derived-mode-p 'haskell-ts-base-mode))
    (should flycheck-mode)
    (should dante-mode)

    (should (string= (dante-config/cabal-target (dante-get-config))
                     "emacs-dante-simple-check-test-project:lib:emacs-dante-simple-check-test-project"))

    (delete-directory (dante-config/build-dir (dante-get-config)) t)

    (dante-tests/check-buffer-and-assert-when-done
     (should (not (null flycheck-current-errors)))

     (let ((err (--find (string-suffix-p "Baz.hs" (flycheck-error-filename it) t)
                        flycheck-current-errors)))
       (should (not (null err)))
       (should (string= (flycheck-error-filename err)
                        (concat dante-tests-tests/simple-check-test-project "/src/Bar/Baz.hs")))
       (should (= (flycheck-error-line err) 10))
       (should (string-search "GHC-25897" (flycheck-error-message err)))
       (should (string-search "Couldn't match expected type ‘b’ with actual type ‘a’"
                              (flycheck-error-message err)))))

    (progn
      (flycheck-enhancements-next-error-with-wraparound)

      (should (string= (concat dante-tests-tests/simple-check-test-project "/src/Bar/Baz.hs")
                       (buffer-file-name))))

    (progn
      (goto-line-dumb 10)
      (move-to-column 4)

      (dante-type-at--with-type-at-point
       (lambda (ty)
         (should (string= ty "x :: a")))))))

(ert-deftest dante-tests/simple-check-project-2 ()
  (unless (executable-find "cabal")
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))

  (test-utils--with-unzipped-project
      dante-tests-tests/simple-test-project-arcive
      tmp-dir

    (dante-tests/with-file
        (concat tmp-dir "/simple-check-project/src/Foo.hs")
      (should (derived-mode-p 'haskell-ts-base-mode))
      (should flycheck-mode)
      (should dante-mode)

      (should (string= (dante-config/cabal-target (dante-get-config))
                       "emacs-dante-simple-check-test-project:lib:emacs-dante-simple-check-test-project"))

      (delete-directory (dante-config/build-dir (dante-get-config)) t)

      (dante-tests/check-buffer-and-assert-when-done
       (should (not (null flycheck-current-errors)))

       (let ((err (--find (string-suffix-p "Baz.hs" (flycheck-error-filename it) t)
                          flycheck-current-errors)))
         (should (not (null err)))
         (should (string= (flycheck-error-filename err)
                          (concat tmp-dir "/simple-check-project/src/Bar/Baz.hs")))
         (should (= (flycheck-error-line err) 10))
         (should (string-search "GHC-25897" (flycheck-error-message err)))
         (should (string-search "Couldn't match expected type ‘b’ with actual type ‘a’"
                                (flycheck-error-message err)))))

      (progn
        (flycheck-enhancements-next-error-with-wraparound)

        (should (string= (concat tmp-dir "/simple-check-project/src/Bar/Baz.hs")
                         (buffer-file-name))))

      (progn
        (goto-line-dumb 10)
        (move-to-column 4)

        (dante-type-at--with-type-at-point
         (lambda (ty)
           (should (string= ty "x :: a")))))

      (progn
        (goto-char (point-min))

        (save-match-data
          (search-forward "baz :: a -> b"))

        (delete-word -1)
        (insert "a")
        (save-buffer)

        (dante-tests/check-buffer-and-assert-when-done
         (should (null flycheck-current-errors))))

      (dante-tests/with-file
          (concat tmp-dir "/simple-check-project/src/Foo.hs")

        (dante-tests/check-buffer-and-assert-when-done
         (should (null flycheck-current-errors)))))))

(ert-deftest dante-tests/simple-repl-project-1 ()
  (unless (executable-find "cabal")
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))

  (dante-tests/with-file
      (concat dante-tests-tests/simple-repl-test-project "/src/Foo.hs")

    (should (derived-mode-p 'haskell-ts-base-mode))
    (should flycheck-mode)
    (should dante-mode)

    (should (string= (dante-config/cabal-target (dante-get-config))
                     "emacs-dante-simple-repl-test-project:lib:emacs-dante-simple-repl-test-project"))

    (delete-directory (dante-config/repl-dir (dante-get-config)) t)

    (vim:haskell-dante-load-file-into-repl:wrapper)

    (let ((repl-proc (get-buffer-process (current-buffer))))
      (dante-repl/wait-for-modules-loaded repl-proc)
      (accept-process-output repl-proc 1 nil t)

      (insert ":t 1 + 2")
      (comint-send-input)
      (dante-repl/wait-for-prompt repl-proc)
      (should (string= (dante-repl-get-last-output) "1 + 2 :: Num a => a\n"))

      (insert ":i Num")
      (comint-send-input)
      (dante-repl/wait-for-prompt repl-proc)
      (let ((msg (dante-repl-get-last-output)))
        (should (string-match-p "class Num a where" msg))
        (should (string-match-p "instance Num Double" msg))
        (should (string-match-p "instance Num Int" msg)))

      (insert ":t foo")
      (comint-send-input)
      (dante-repl/wait-for-prompt repl-proc)
      (should (string= (dante-repl-get-last-output) "foo :: a -> a\n"))

      (insert ":t baz")
      (comint-send-input)
      (dante-repl/wait-for-prompt repl-proc)
      (should (string= (dante-repl-get-last-output) "baz :: a -> a\n"))

      (insert ":t bar")
      (comint-send-input)
      (dante-repl/wait-for-prompt repl-proc)
      (let ((msg (dante-repl-get-last-output)))
        (should (string-match-p "GHC-88464" msg))
        (should (string-match-p "Variable not in scope: bar" msg)))

      (insert "foo baz 36")
      (comint-send-input)
      (dante-repl/wait-for-prompt repl-proc)
      (should (string= (dante-repl-get-last-output) "36\n")))))

(ert-deftest dante-tests/check-hsc-project-1 ()
  (unless (executable-find "cabal")
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))

  (test-utils--with-unzipped-project
      dante-tests-tests/simple-check-test-project-with-hsc-archive
      tmp-dir

    (dante-tests/with-file
        (concat tmp-dir "/simple-check-project-with-hsc/src/Foo.hs")
      (should (derived-mode-p 'haskell-ts-base-mode))
      (should flycheck-mode)
      (should dante-mode)

      (should (string= (dante-config/cabal-target (dante-get-config))
                       "emacs-dante-simple-check-test-project-with-hsc:lib:emacs-dante-simple-check-test-project-with-hsc"))

      (should (eq 'build (dante-method/name (dante-config/method (dante-get-config)))))

      (delete-directory (dante-config/build-dir (dante-get-config)) t)

      (dante-tests/check-buffer-and-assert-when-done
       (should (not (null flycheck-current-errors)))

       (let ((err (--find (string-suffix-p "Baz.hsc" (flycheck-error-filename it) t)
                          flycheck-current-errors)))
         (should (not (null err)))
         (should (string= (flycheck-error-filename err)
                          (concat tmp-dir "/simple-check-project-with-hsc/src/Bar/Baz.hsc")))
         (should (= (flycheck-error-line err) 18))
         (should (string-search "GHC-25897" (flycheck-error-message err)))
         (should (string-search "Couldn't match expected type ‘a’ with actual type ‘CDoubleTyp’"
                                (flycheck-error-message err)))))

      (progn
        (flycheck-enhancements-next-error-with-wraparound)

        (should (string= (concat tmp-dir "/simple-check-project-with-hsc/src/Bar/Baz.hsc")
                         (buffer-file-name)))

        (dante-tests/check-buffer-and-assert-when-done
         (should (not (null flycheck-current-errors)))

         (let ((err (--find (string-suffix-p "Baz.hsc" (flycheck-error-filename it) t)
                            flycheck-current-errors)))
           (should (not (null err)))
           (should (string= (flycheck-error-filename err)
                            (concat tmp-dir "/simple-check-project-with-hsc/src/Bar/Baz.hsc")))
           (should (= (flycheck-error-line err) 18))
           (should (string-search "GHC-25897" (flycheck-error-message err)))
           (should (string-search "Couldn't match expected type ‘a’ with actual type ‘CDoubleTyp’"
                                  (flycheck-error-message err)))))        )

      (progn
        (goto-char (point-min))

        (save-match-data
          (search-forward "baz :: Double -> a"))

        (delete-word -1)
        (insert "CDoubleTyp")
        (save-buffer)

        (dante-tests/check-buffer-and-assert-when-done
         (should (null flycheck-current-errors))))

      (dante-tests/with-file
          (concat tmp-dir "/simple-check-project-with-hsc/src/Foo.hs")

        (dante-tests/check-buffer-and-assert-when-done
         (should (null flycheck-current-errors)))))))

(ert-deftest dante-tests/check-hsc-project-2 ()
  (unless (executable-find "cabal")
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))
  (unless (executable-find "sed")
    (ert-skip "sed not available"))

  (test-utils--with-unzipped-project
      dante-tests-tests/simple-check-test-project-with-hsc-archive
      tmp-dir

    (dante-tests/with-file
        (concat tmp-dir "/simple-check-project-with-hsc/src/Foo.hs")
      (should (derived-mode-p 'haskell-ts-base-mode))
      (should flycheck-mode)
      (should dante-mode)

      (should (string= (dante-config/cabal-target (dante-get-config))
                       "emacs-dante-simple-check-test-project-with-hsc:lib:emacs-dante-simple-check-test-project-with-hsc"))

      (should (eq 'build (dante-method/name (dante-config/method (dante-get-config)))))

      (delete-directory (dante-config/build-dir (dante-get-config)) t)

      (dante-tests/check-buffer-and-assert-when-done
       (should (not (null flycheck-current-errors)))

       (let ((err (--find (string-suffix-p "Baz.hsc" (flycheck-error-filename it) t)
                          flycheck-current-errors)))
         (should (not (null err)))
         (should (string= (flycheck-error-filename err)
                          (concat tmp-dir "/simple-check-project-with-hsc/src/Bar/Baz.hsc")))
         (should (= (flycheck-error-line err) 18))
         (should (string-search "GHC-25897" (flycheck-error-message err)))
         (should (string-search "Couldn't match expected type ‘a’ with actual type ‘CDoubleTyp’"
                                (flycheck-error-message err)))))

      ;; Check that changes outside Emacs trigger preprocessing.
      (call-process "sed" nil nil nil
                    "-re"
                    "s/baz :: Double -> a/baz :: Double -> CDoubleTyp/"
                    "-i"
                    (concat tmp-dir "/simple-check-project-with-hsc/src/Bar/Baz.hsc"))
      (dante-tests/check-buffer-and-assert-when-done
       (should (string= (buffer-file-name)
                        (concat tmp-dir "/simple-check-project-with-hsc/src/Foo.hs")))
       (should (null flycheck-current-errors)))

      ;; And back again
      (call-process "sed" nil nil nil
                    "-re"
                    "s/baz :: Double -> CDoubleTyp/baz :: Double -> a/"
                    "-i"
                    (concat tmp-dir "/simple-check-project-with-hsc/src/Bar/Baz.hsc"))
      (dante-tests/check-buffer-and-assert-when-done
       (should (string= (buffer-file-name)
                        (concat tmp-dir "/simple-check-project-with-hsc/src/Foo.hs")))
       (should (not (null flycheck-current-errors)))))))

(ert-deftest dante-tests/repl-hsc-project-1 ()
  (unless (executable-find "cabal")
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))

  (dante-tests/with-file
      (concat dante-tests-tests/simple-repl-test-project-with-hsc "/src/Foo.hs")
    (should (derived-mode-p 'haskell-ts-base-mode))
    (should flycheck-mode)
    (should dante-mode)

    (should (string= (dante-config/cabal-target (dante-get-config))
                     "emacs-dante-simple-repl-test-project-with-hsc:lib:emacs-dante-simple-repl-test-project-with-hsc"))

    (should (eq 'build (dante-method/name (dante-config/method (dante-get-config)))))

    (delete-directory (dante-config/repl-dir (dante-get-config)) t)

    (vim:haskell-dante-load-file-into-repl:wrapper)

    (let ((repl-proc (get-buffer-process (current-buffer))))
      (dante-repl/wait-for-modules-loaded repl-proc)
      (accept-process-output repl-proc 1 nil t)

      (insert ":t 1 + 2")
      (comint-send-input)
      (dante-repl/wait-for-prompt repl-proc)
      (should (string= (dante-repl-get-last-output) "1 + 2 :: Num a => a\n"))

      (insert ":i Num")
      (comint-send-input)
      (dante-repl/wait-for-prompt repl-proc)
      (let ((msg (dante-repl-get-last-output)))
        (should (string-match-p "class Num a where" msg))
        (should (string-match-p "instance Num Double" msg))
        (should (string-match-p "instance Num Int" msg)))

      (insert ":t baz")
      (comint-send-input)
      (dante-repl/wait-for-prompt repl-proc)
      (should (string= (dante-repl-get-last-output) "baz :: Double -> CDoubleTyp\n"))

      (insert "foo 0")
      (comint-send-input)
      (dante-repl/wait-for-prompt repl-proc)
      (should (string= (dante-repl-get-last-output) "0.0\n")))))

(ert-deftest dante-tests/repl-hsc-project-2 ()
  (unless (executable-find "cabal")
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))

  (dante-tests/with-file
   (concat dante-tests-tests/simple-repl-test-project-with-hsc "/src/Bar/Baz.hsc")
   (should (derived-mode-p 'haskell-ts-base-mode))
   (should flycheck-mode)
   (should dante-mode)

   (should (string= (dante-config/cabal-target (dante-get-config))
                    "emacs-dante-simple-repl-test-project-with-hsc:lib:emacs-dante-simple-repl-test-project-with-hsc"))

   (should (eq 'build (dante-method/name (dante-config/method (dante-get-config)))))

   (delete-directory (dante-config/repl-dir (dante-get-config)) t)

   (vim:haskell-dante-load-file-into-repl:wrapper)

   (let ((repl-proc (get-buffer-process (current-buffer))))
     (dante-repl/wait-for-modules-loaded repl-proc)
     (accept-process-output repl-proc 1 nil t)

     (insert ":t 1 + 2")
     (comint-send-input)
     (dante-repl/wait-for-prompt repl-proc)
     (should (string= (dante-repl-get-last-output) "1 + 2 :: Num a => a\n"))

     (insert ":i Num")
     (comint-send-input)
     (dante-repl/wait-for-prompt repl-proc)
     (let ((msg (dante-repl-get-last-output)))
       (should (string-match-p "class Num a where" msg))
       (should (string-match-p "instance Num Double" msg))
       (should (string-match-p "instance Num Int" msg)))

     (insert ":t baz")
     (comint-send-input)
     (dante-repl/wait-for-prompt repl-proc)
     (should (string= (dante-repl-get-last-output) "baz :: Double -> CDoubleTyp\n"))

     (insert "baz 0")
     (comint-send-input)
     (dante-repl/wait-for-prompt repl-proc)
     (should (string= (dante-repl-get-last-output) "0.0\n")))))

(ert-deftest dante-tests/repl-hsc-project-3 ()
  ;; todo: unpack simple-check-project-with-hsc.zip and try fixing and reloading to see
  ;; that changes in hsc file are picked up by dante-repl ghci session
  (unless (executable-find "cabal")
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))

  (test-utils--with-unzipped-project
      dante-tests-tests/simple-check-test-project-with-hsc-archive
      tmp-dir
    (dante-tests/with-file
        (concat tmp-dir "/simple-check-project-with-hsc/src/Bar/Baz.hsc")
      (should (derived-mode-p 'haskell-ts-base-mode))
      (should flycheck-mode)
      (should dante-mode)

      (should (string= (dante-config/cabal-target (dante-get-config))
                       "emacs-dante-simple-check-test-project-with-hsc:lib:emacs-dante-simple-check-test-project-with-hsc"))

      (should (eq 'build (dante-method/name (dante-config/method (dante-get-config)))))

      (delete-directory (dante-config/repl-dir (dante-get-config)) t)

      (let ((hsc-buf (current-buffer)))

        (vim:haskell-dante-load-file-into-repl:wrapper)

        (let ((repl-proc (get-buffer-process (current-buffer))))
          (dante-repl/wait-for-modules-loaded repl-proc)
          (accept-process-output repl-proc 1 nil t)

          (let ((initial-output (dante-repl-get-last-output)))
            (should (string-search "GHC-25897" initial-output))
            (should (string-search "Couldn't match expected type ‘a’ with actual type ‘CDoubleTyp’" initial-output)))

          (insert ":t 1 + 2")
          (comint-send-input)
          (dante-repl/wait-for-prompt repl-proc)
          (should (string= (dante-repl-get-last-output) "1 + 2 :: Num a => a\n"))

          (insert ":i Num")
          (comint-send-input)
          (dante-repl/wait-for-prompt repl-proc)
          (let ((msg (dante-repl-get-last-output)))
            (should (string-match-p "class Num a where" msg))
            (should (string-match-p "instance Num Double" msg))
            (should (string-match-p "instance Num Int" msg)))

          (switch-to-buffer hsc-buf)

          (progn
            (goto-char (point-min))

            (save-match-data
              (search-forward "baz :: Double -> a"))

            (delete-word -1)
            (insert "CDoubleTyp")
            (save-buffer))

          (vim:haskell-dante-load-file-into-repl:wrapper)
          (dante-repl/wait-for-prompt repl-proc)

          (insert ":t 1 + 2")
          (comint-send-input)
          (dante-repl/wait-for-prompt repl-proc)
          (should (string= (dante-repl-get-last-output) "1 + 2 :: Num a => a\n"))

          (insert ":t baz")
          (comint-send-input)
          (dante-repl/wait-for-prompt repl-proc)
          (should (string= (dante-repl-get-last-output) "baz :: Double -> CDoubleTyp\n"))

          (insert "baz 0")
          (comint-send-input)
          (dante-repl/wait-for-prompt repl-proc)
          (should (string= (dante-repl-get-last-output) "0.0\n")))))))

(provide 'dante-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; dante-tests.el ends here
