;; dante-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  8 October 2025
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'set-up-platform))

(defvar flycheck-current-errors)
(defvar flycheck-setup--force-enable-flycheck-for-tests)

(require 'common)
(require 'dante)

(require 'dante-tests-utils)
(require 'ert)
(require 'tests-utils)

(defconst dante-test-data/simple-check-test-project
  (concat dante-test-data/resources-root "/dante/simple-check-project"))

(defconst dante-test-data/simple-test-project-archive
  (concat dante-test-data/resources-root "/dante/simple-check-project.zip"))

(defconst dante-test-data/simple-test-project-name-shadowing-error
  (concat dante-test-data/resources-root "/dante/simple-check-project-name-shadowing-error"))

(defconst dante-test-data/simple-check-project-error-with-relative-path-from-subproject-archive
  (concat dante-test-data/resources-root
          "/dante/simple-check-project-error-with-relative-path-from-subproject.zip"))

(defconst dante-test-data/native-flake
  (concat +emacs-config-path+
          "/native/rure-ffi/flake.nix"))

(defconst dante-test-data/native-flake-lock
  (concat +emacs-config-path+
          "/native/rure-ffi/flake.lock"))

(defconst dante-test-data/simple-repl-test-project
  (concat dante-test-data/resources-root "/dante/simple-repl-project"))

(defconst dante-test-data/simple-check-test-project-with-dotghci
  (concat dante-test-data/resources-root "/dante/simple-check-project-with-dotghci"))

(ert-deftest dante-tests/dante--extract-current-working-directory-from-show-paths ()
  (should
   (string=
    "/foo/bar"
    (dante--extract-current-working-directory-from-show-paths
     (tests-utils--multiline
      "current working directory: "
      "  /foo/bar"
      "module import search paths:"
      "  .")))))

(ert-deftest z-dante-tests/simple-check-project-1 ()
  (unless (executable-find dante-cabal-executable)
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))

  (dante-tests/with-file
      (concat dante-test-data/simple-check-test-project "/src/Foo.hs")
    ;; (find-file (concat dante-test-data/simple-check-test-project "/src/Foo.hs"))
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
       (should (dante-tests--paths= (flycheck-error-filename err)
                                    (concat dante-test-data/simple-check-test-project "/src/Bar/Baz.hs")))
       (should (= (flycheck-error-line err) 10))
       (should (string-search "GHC-25897" (flycheck-error-message err)))
       (should (string-search "Couldn't match expected type ‘b’ with actual type ‘a’"
                              (flycheck-error-message err)))))

    (progn
      (flycheck-enhancements-next-error-with-wraparound)

      (should (string= (concat dante-test-data/simple-check-test-project "/src/Bar/Baz.hs")
                       (buffer-file-name))))

    (progn
      (goto-line-dumb 10)
      (move-to-column 4)

      (dante-tests/type-at-point-and-assert-when-done
          ty
        (should (string= ty "x :: a"))))))

(ert-deftest z-dante-tests/simple-check-project-2 ()
  (unless (executable-find dante-cabal-executable)
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))

  (test-utils--with-unzipped-project
      dante-test-data/simple-test-project-archive
      tmp-dir

    (let ((proj-dir (concat tmp-dir "/simple-check-project")))
      (dante-tests/with-file
          (concat proj-dir "/src/Foo.hs")
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
           (should (dante-tests--paths= (flycheck-error-filename err)
                                        (concat proj-dir "/src/Bar/Baz.hs")))
           (should (= (flycheck-error-line err) 10))
           (should (string-search "GHC-25897" (flycheck-error-message err)))
           (should (string-search "Couldn't match expected type ‘b’ with actual type ‘a’"
                                  (flycheck-error-message err)))))

        (progn
          (flycheck-enhancements-next-error-with-wraparound)

          (should (dante-tests--paths= (concat proj-dir "/src/Bar/Baz.hs")
                                       (buffer-file-name))))

        (progn
          (goto-line-dumb 10)
          (move-to-column 4)

          (dante-tests/type-at-point-and-assert-when-done
              ty
            (should (string= ty "x :: a"))))

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
            (concat proj-dir "/src/Foo.hs")

          (dante-tests/check-buffer-and-assert-when-done
           (should (null flycheck-current-errors))))))))

(ert-deftest z-dante-tests/simple-check-project-3-name-shadowing-error ()
  (unless (executable-find dante-cabal-executable)
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))

  (dante-tests/with-file
      (concat dante-test-data/simple-test-project-name-shadowing-error "/src/Foo.hs")
    (should (derived-mode-p 'haskell-ts-base-mode))
    (should flycheck-mode)
    (should dante-mode)

    (should (string= (dante-config/cabal-target (dante-get-config))
                     "emacs-dante-simple-check-test-project-name-shadowing-error:lib:emacs-dante-simple-check-test-project-name-shadowing-error"))

    (delete-directory (dante-config/build-dir (dante-get-config)) t)

    (dante-tests/check-buffer-and-assert-when-done
     (should (not (null flycheck-current-errors)))

     (let ((err (--find (and (string-suffix-p "Foo.hs" (flycheck-error-filename it) t)
                             (string-search "GHC-63397" (flycheck-error-message it)))
                        flycheck-current-errors)))
       (should (not (null err)))
       (should (dante-tests--paths= (flycheck-error-filename err)
                                    (concat dante-test-data/simple-test-project-name-shadowing-error "/src/Foo.hs")))
       (should (string-search "This binding for ‘x’ shadows the existing binding"
                              (flycheck-error-message err)))))

    (progn
      (flycheck-enhancements-next-error-with-wraparound)

      (should (string= (concat dante-test-data/simple-test-project-name-shadowing-error "/src/Foo.hs")
                       (buffer-file-name))))

    (progn
      (goto-line-dumb 10)
      (move-to-column 14)

      ;; Test that :type-at still works even when project doesn’t build because of -Werror.
      ;; Dante must override -Werror with -Wwarn to make this work.
      (dante-tests/type-at-point-and-assert-when-done
          ty
        (should (string= ty "myreplicate :: Int -> [a] -> [[a]]"))))))

(defun dante-tests--simple-check-project--error-with-relative-path-from-subproject-impl (enable-flakes?)
  (unless (executable-find dante-cabal-executable)
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))

  (test-utils--with-unzipped-project
      dante-test-data/simple-check-project-error-with-relative-path-from-subproject-archive
      tmp-dir

    (let ((proj-dir (concat tmp-dir "/simple-check-project-error-with-relative-path-from-subproject")))

      (when enable-flakes?
        (dolist (x (list dante-test-data/native-flake
                         dante-test-data/native-flake-lock))
          (unless (file-exists-p x)
            (ert-skip "flakes not available"))
          (copy-file x (concat proj-dir "/" (file-name-nondirectory x)))))

      (dante-tests/with-file-no-clean
          (concat proj-dir "/main/src/Foo/Bar.hs")
        (should (derived-mode-p 'haskell-ts-base-mode))
        (should flycheck-mode)
        (should dante-mode)

        (should (string= (dante-config/cabal-target (dante-get-config))
                         "emacs-dante-simple-check-test-project-error-with-rel-path-main:lib:emacs-dante-simple-check-test-project-error-with-rel-path-main"))

        (delete-directory (dante-config/build-dir (dante-get-config)) t)

        (dante-tests/check-buffer-and-assert-when-done
         (should (null flycheck-current-errors)))

        (goto-line-dumb 13)
        (move-to-column-fixed 12)

        ;; Sanity check
        (should (looking-at-p (rx symbol-start "bar" symbol-end)))

        (dante-tests/haskell-symbnav-go-to-symbol-home-and-assert-when-done
         (should (dante-tests--paths= (concat proj-dir "/main/src/Baz/Quux.hs")
                                      (buffer-file-name)))
         (should (string= (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                          "bar x = x"))))

      (dante-tests/with-file-no-clean
          (concat proj-dir "/main/src/Baz/Quux.hs")
        (goto-line-dumb 10)
        (delete-region (line-beginning-position) (line-end-position))
        (insert "bar bar")
        (save-buffer))

      (dante-tests/with-file-no-clean
          (concat proj-dir "/main/src/Foo/Bar.hs")
        (dante-tests/check-buffer-and-assert-when-done
         (should (not (null flycheck-current-errors)))

         (let ((err (--find (and (string-suffix-p "Baz/Quux.hs" (flycheck-error-filename it) t)
                                 (string-search "GHC-25277" (flycheck-error-message it)))
                            flycheck-current-errors)))
           (should (not (null err)))
           (should (dante-tests--paths= (flycheck-error-filename err)
                                        (concat proj-dir "/main/src/Baz/Quux.hs")))))

        (progn
          (flycheck-enhancements-next-error-with-wraparound)

          (should (dante-tests--paths= (concat proj-dir "/main/src/Baz/Quux.hs")
                                       (buffer-file-name))))))))

(ert-deftest z-dante-tests/simple-check-project-4-error-with-relative-path-from-subproject ()
  (unless (executable-find dante-cabal-executable)
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))

  (dante-tests--simple-check-project--error-with-relative-path-from-subproject-impl nil))

(ert-deftest z-dante-tests/simple-check-project-5-error-with-relative-path-from-subproject-run-under-nix ()
  (unless (executable-find dante-cabal-executable)
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))
  (unless (or (executable-find "trix") (executable-find "nix"))
    (ert-skip "neither trix nor nix are not available"))

  (dante-tests--simple-check-project--error-with-relative-path-from-subproject-impl t))

(ert-deftest z-dante-tests/simple-check-project-6-with-dotghci-leading-to-errors ()
  (unless (executable-find dante-cabal-executable)
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))

  (dante-tests/with-file
   (concat dante-test-data/simple-check-test-project-with-dotghci "/Foo.hs")

   (should (derived-mode-p 'haskell-ts-base-mode))
   (should flycheck-mode)
   (should dante-mode)

   (should (string= (dante-config/cabal-target (dante-get-config))
                    "test-project-with-ifdef:lib:test-project-with-ifdef"))

   (delete-directory (dante-config/build-dir (dante-get-config)) t)

   (dante-tests/check-buffer-and-assert-when-done
    (should (null flycheck-current-errors)))))

(ert-deftest z-dante-tests/simple-repl-project-1 ()
  (unless (executable-find dante-cabal-executable)
    (ert-skip "cabal not available"))
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))

  (dante-tests/with-file
      (concat dante-test-data/simple-repl-test-project "/src/Foo.hs")

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


(ert-deftest z-dante-tests/simple-check-project-ghci-method-1 ()
  (unless (executable-find "ghc")
    (ert-skip "ghc not available"))
  (unless (treesit-language-available-p 'haskell)
    (ert-skip "haskell-ts-mode not available"))

  (let ((flycheck-setup--force-enable-flycheck-for-tests t))
    (test-utils--with-temp-file
        tmp-file
        "Main"
        ".hs"
        "\
foo :: Int
foo = 1

bar :: String
bar = foo
"

      (should (derived-mode-p 'haskell-ts-base-mode))

      (let ((cfg (dante-get-config)))
        (should (not (null cfg)))
        (should (eq 'bare-ghci (dante-method/name (dante-config/method cfg)))))

      (should flycheck-mode)
      (should dante-mode)

      (dante-tests/check-buffer-and-assert-when-done
       (should (not (null flycheck-current-errors)))

       (should (not (null flycheck-current-errors)))
       (should (= 1 (length flycheck-current-errors)))
       (let ((err (car flycheck-current-errors)))
         (should (dante-tests--paths= (flycheck-error-filename err)
                                      tmp-file))
         (should (= (flycheck-error-line err) 5))
         (should (string-search "GHC-83865" (flycheck-error-message err)))
         (let ((msg (flycheck-error-message err)))
           (should (string-match-p
                    (rx-let ((apostrophe (any ?\' ?\’))
                             (quotes (x) (seq (any ?\` ?\‘) x (any ?\' ?\’) ))
                             (ws (+ (any ?\s ?\t))))
                      (rx "Couldn" apostrophe "t" ws
                          "match" ws
                          "type" ws (quotes "Int") ws "with" ws (quotes (or "[Char]" "String"))))
                    msg)))))
      (progn
        (goto-line-dumb 1)
        (move-to-column 1)

        (dante-tests/type-at-point-and-assert-when-done
            ty
          (should (string= ty "foo :: Int")))))))

(provide 'dante-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; dante-tests.el ends here
