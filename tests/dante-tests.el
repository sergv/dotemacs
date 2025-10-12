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
             (with-current-buffer ,buf-var
               (awhen (dante-buffer-p)
                 (kill-buffer it)))))))))

(defmacro dante-tests/check-buffer-and-assert-when-done (&rest body)
  `(progn
     (flycheck-buffer)

     (let* ((checking-done nil)
            (check-func
             (lambda ()
               (unwind-protect
                   (progn
                     ,@body)
                 (setf checking-done t)))))

       (add-hook 'flycheck-after-syntax-check-hook check-func nil t)

       (while (not checking-done)
         (sleep-for 0.05))

       (remove-hook 'flycheck-after-syntax-check-hook check-func t))))

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
           (not (looking-at-p "Ok, modules loaded:.*\\.$")))
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

    (dante-initialize-method)
    (should (string= dante-target "emacs-dante-simple-check-test-project:lib:emacs-dante-simple-check-test-project"))

    (delete-directory (dante-check-get-component-build-dir (current-buffer)) t)

    (dante-tests/check-buffer-and-assert-when-done
     (should (not (null flycheck-current-errors)))

     (let ((err (--find (string-suffix-p "Baz.hs" (flycheck-error-filename it) t)
                        flycheck-current-errors)))
       (should (not (null err)))
       (should (string= (flycheck-error-filename err)
                        (concat dante-tests-tests/simple-check-test-project "/src/Bar/Baz.hs")))
       (should (= (flycheck-error-line err) 10))
       (should (string-search "GHC-25897" (flycheck-error-message err)))

       (setf checking-done t)))

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

      (dante-initialize-method)
      (should (string= dante-target "emacs-dante-simple-check-test-project:lib:emacs-dante-simple-check-test-project"))

      (delete-directory (dante-check-get-component-build-dir (current-buffer)) t)

      (dante-tests/check-buffer-and-assert-when-done
       (should (not (null flycheck-current-errors)))

       (let ((err (--find (string-suffix-p "Baz.hs" (flycheck-error-filename it) t)
                          flycheck-current-errors)))
         (should (not (null err)))
         (should (string= (flycheck-error-filename err)
                          (concat tmp-dir "/simple-check-project/src/Bar/Baz.hs")))
         (should (= (flycheck-error-line err) 10))
         (should (string-search "GHC-25897" (flycheck-error-message err)))

         (setf checking-done t)))

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

    (dante-initialize-method)
    (should (string= dante-target
                     "emacs-dante-simple-repl-test-project:lib:emacs-dante-simple-repl-test-project"))

    (delete-directory (dante-repl-get-component-build-dir (current-buffer)) t)


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

(provide 'dante-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; dante-tests.el ends here
