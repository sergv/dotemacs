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

(defconst dante-tests-tests/simple-test-project
  (concat +emacs-config-path+ "/tests/test-data/dante/simple-project"))

(ert-deftest dante-tests-simple-project-1 ()

  (unless (executable-find "cabal")
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))

  (let ((noninteractive nil))
    (find-file (concat dante-tests-tests/simple-test-project "/src/Foo.hs"))
    (should (derived-mode-p 'haskell-ts-base-mode))
    (should flycheck-mode)
    (should dante-mode)

    (dante-initialize-method)
    (should (string= dante-target "emacs-dante-simple-test-project:lib:emacs-dante-simple-test-project"))

    (delete-directory (dante-check-get-component-build-dir (current-buffer)) t)

    (flycheck-buffer)

    (let ((checking-done nil))

      (add-hook 'flycheck-after-syntax-check-hook
                (lambda ()
                  (should (not (null flycheck-current-errors)))

                  (let ((err (--find (string-suffix-p "Baz.hs" (flycheck-error-filename it) t)
                                     flycheck-current-errors)))
                    (should (not (null err)))
                    (should (string= (flycheck-error-filename err)
                                     (concat dante-tests-tests/simple-test-project "/src/Bar/Baz.hs")))
                    (should (= (flycheck-error-line err) 10))
                    (should (string-search "GHC-25897" (flycheck-error-message err)))

                    (setf checking-done t)))
                nil
                t)

      (while (not checking-done)
        (sleep-for 0.1)))

    (flycheck-enhancements-next-error-with-wraparound)

    (should (string= (concat dante-tests-tests/simple-test-project "/src/Bar/Baz.hs")
                     (buffer-file-name)))

    (goto-line-dumb 10)
    (move-to-column 4)

    (dante-type-at--with-type-at-point
     (lambda (ty)
       (should (string= ty "x :: a"))))))

(provide 'dante-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; dante-tests.el ends here
