;;; flycheck-haskell-test.el --- Flycheck Haskell: Test suite  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Sergey Vinokurov <serg.foo@gmail.com>
;; Copyright (C) 2014, 2015  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The test suite for Flycheck Haskell.

;;; Code:

(require 'flycheck-haskell)

(require 'let-alist)
(require 'cl-lib)
(require 'ert)


;;; Directories

(defun flycheck-haskell--concat-dirs (&rest dirs)
  "Combine multiple relative directory names into one.

Combine directory names in DIRS into one long directory name
using directory separator."
  (cl-reduce
   #'concat
   (seq-map #'file-name-as-directory dirs)
   :initial-value nil))

(defconst flycheck-haskell-test-dir
  (file-name-directory (if load-in-progress load-file-name (buffer-file-name)))
  "Directory of the test suite.")

(defconst flycheck-haskell-cabal-test-dir
  (flycheck-haskell--concat-dirs flycheck-haskell-test-dir
                                 "test-data"
                                 "project-with-cabal-file")
  "Directory where tests are run by default.")

(defconst flycheck-haskell-hpack-test-dir
  (flycheck-haskell--concat-dirs flycheck-haskell-test-dir
                                 "test-data"
                                 "project-with-package-yaml")
  "Directory where tests are run by default.")

(defconst flycheck-haskell-test-cabal-file
  (expand-file-name "flycheck-haskell-test.cabal"
                    flycheck-haskell-cabal-test-dir)
  "Cabal file for our test suite.")

(defconst flycheck-haskell-test-hpack-file
  (expand-file-name "package.yaml"
                    flycheck-haskell-hpack-test-dir)
  "package.yaml file for our test suite.")


;;; Helpers

(defun flycheck-haskell-read-test-cabal-config ()
  "Read the Cabal configuration from the test file."
  (flycheck-haskell-read-cabal-configuration flycheck-haskell-test-cabal-file))

(defun flycheck-haskell-read-test-hpack-config ()
  "Read the Cabal configuration from the test file."
  (flycheck-haskell-read-hpack-configuration flycheck-haskell-test-hpack-file))

(defmacro flycheck-haskell-test-with-cache (&rest body)
  "Run BODY and clear the config cache afterwards."
  (declare (indent 0))
  `(unwind-protect (progn ,@body)
     (flycheck-haskell-clear-config-cache)))

(defmacro flycheck-haskell-test-with-fake-file (&rest body)
  "Run BODY with a fake file buffer."
  (declare (indent 0))
  `(with-temp-buffer
     (let* ((default-directory flycheck-haskell-cabal-test-dir)
            (buffer-file-name (expand-file-name "test.hs")))
       ,@body)))

(defun flycheck-haskell--sort-strs (xs)
  (seq-sort #'string< xs))


;;; Cabal and hpack support
(ert-deftest flycheck-haskell-read-cabal-configuration/has-all-extensions ()
  (let-alist (flycheck-haskell-read-test-cabal-config)
    (should (equal (flycheck-haskell--sort-strs .extensions)
                   (flycheck-haskell--sort-strs
                    '("OverloadedStrings"
                      "YouDontKnowThisOne"
                      "GeneralizedNewtypeDeriving"))))))

(ert-deftest flycheck-haskell-read-hpack-configuration/has-all-extensions ()
  (skip-unless flycheck-haskell-hpack-executable)
  (let-alist (flycheck-haskell-read-test-hpack-config)
    (should (equal (flycheck-haskell--sort-strs .extensions)
                   (flycheck-haskell--sort-strs
                    '("OverloadedStrings"
                      "YouDontKnowThisOne"
                      "GeneralizedNewtypeDeriving"))))))

(ert-deftest flycheck-haskell-read-cabal-configuration/has-all-languages ()
  (let-alist (flycheck-haskell-read-test-cabal-config)
    (should-not (seq-difference .languages '("Haskell98"
                                             "SpamLanguage"
                                             "Haskell2010")))))

(ert-deftest flycheck-haskell-read-hpack-configuration/has-all-languages ()
  (skip-unless flycheck-haskell-hpack-executable)
  (let-alist (flycheck-haskell-read-test-hpack-config)
    (should-not (seq-difference .languages '("Haskell98"
                                             "SpamLanguage"
                                             "Haskell2010")))))

(ert-deftest flycheck-haskell-read-cabal-configuration/source-dirs ()
  (let-alist (flycheck-haskell-read-test-cabal-config)
    (should-not (seq-difference
                 (flycheck-haskell--sort-strs .source-directories)
                 (flycheck-haskell--sort-strs
                  (seq-map (lambda (fn)
                             (file-name-as-directory
                              (expand-file-name fn flycheck-haskell-cabal-test-dir)))
                           '("lib/" "." "src/")))))))

(ert-deftest flycheck-haskell-read-hpack-configuration/source-dirs ()
  (skip-unless flycheck-haskell-hpack-executable)
  (let-alist (flycheck-haskell-read-test-hpack-config)
    (should-not (seq-difference
                 (flycheck-haskell--sort-strs .source-directories)
                 (flycheck-haskell--sort-strs
                  (seq-map (lambda (fn)
                             (file-name-as-directory
                              (expand-file-name fn flycheck-haskell-hpack-test-dir)))
                           '("lib/" "." "src/")))))))

(ert-deftest flycheck-haskell-read-cabal-configuration/build-dirs ()
  (let* ((builddirs '("build" "build/autogen"
                      "build/flycheck-haskell-unknown-stuff/flycheck-haskell-unknown-stuff-tmp"
                      "build/flycheck-haskell-test/flycheck-haskell-test-tmp")))
    (let-alist (flycheck-haskell-read-test-cabal-config)
      (dolist (dir builddirs)
        (let ((stack-re (format "\\.stack-work/.*/%s\\'" (regexp-quote dir)))
              (cabal-re (format "dist/%s\\'" (regexp-quote dir))))
          (should (seq-find (apply-partially #'string-match-p stack-re)
                            .build-directories))
          (should (seq-find (apply-partially #'string-match-p cabal-re)
                            .build-directories)))))))

(ert-deftest flycheck-haskell-read-hpack-configuration/build-dirs ()
  (skip-unless flycheck-haskell-hpack-executable)
  (let* ((builddirs '("build" "build/autogen"
                      "build/flycheck-haskell-unknown-stuff/flycheck-haskell-unknown-stuff-tmp"
                      "build/flycheck-haskell-test/flycheck-haskell-test-tmp")))
    (let-alist (flycheck-haskell-read-test-hpack-config)
      (dolist (dir builddirs)
        (let ((stack-re (format "\\.stack-work/.*/%s\\'" (regexp-quote dir)))
              (cabal-re (format "dist/%s\\'" (regexp-quote dir))))
          (should (seq-find (apply-partially #'string-match-p stack-re)
                            .build-directories))
          (should (seq-find (apply-partially #'string-match-p cabal-re)
                            .build-directories)))))))

(ert-deftest flycheck-haskell-read-cabal-configuration/cpp-options ()
  (let-alist (flycheck-haskell-read-test-cabal-config)
    (should (member "-DDEBUG=1" .other-options))))

(ert-deftest flycheck-haskell-read-hpack-configuration/cpp-options ()
  (skip-unless flycheck-haskell-hpack-executable)
  (let-alist (flycheck-haskell-read-test-hpack-config)
    (should (member "-DDEBUG=1" .other-options))))

(ert-deftest flycheck-haskell-read-cabal-configuration/ghc-options ()
  (let-alist (flycheck-haskell-read-test-cabal-config)
    (should (member "-Wall" .other-options))))

(ert-deftest flycheck-haskell-read-hpack-configuration/ghc-options ()
  (skip-unless flycheck-haskell-hpack-executable)
  (let-alist (flycheck-haskell-read-test-hpack-config)
    (should (member "-Wall" .other-options))))

(ert-deftest flycheck-haskell-get-configuration/no-cache-entry ()
  (let* ((cabal-file flycheck-haskell-test-cabal-file))
    (cl-letf (((symbol-function 'flycheck-haskell-read-cabal-configuration)
               (lambda (_) 'dummy)))
      (flycheck-haskell-test-with-cache
        (should-not (flycheck-haskell-get-cached-configuration cabal-file))
        (should (eq (flycheck-haskell-get-configuration cabal-file) 'dummy))
        (should (eq (flycheck-haskell-get-cached-configuration cabal-file)
                    'dummy))))))

(ert-deftest flycheck-haskell-read-cabal-configuration/read-from-dir-that-has-prelude-module ()
  (let* ((test-dir (flycheck-haskell--concat-dirs flycheck-haskell-test-dir
                                                  "test-data"
                                                  "project-with-prelude-module"))
         (default-directory test-dir))
    (cl-assert (file-regular-p (expand-file-name "foo.cabal" test-dir)))
    (flycheck-haskell-read-cabal-configuration "foo.cabal")
    (let ((conf (flycheck-haskell-read-cabal-configuration "foo.cabal")))
      (let-alist conf
        (should (equal .dependencies '("base")))
        (should (equal .extensions '("OverloadedStrings")))
        (should (equal .languages '("Haskell2010")))
        (should (member "-Wall" .other-options))
        (should (member "-fwarn-haha-no-such-option" .other-options))
        (should (equal .source-directories (list (expand-file-name "lib/" test-dir))))))))


;;; Package environment support
(ert-deftest flycheck-haskell-read-cabal-configuration/read-from-dir-that-has-package-env ()
  (let* ((test-dir (flycheck-haskell--concat-dirs flycheck-haskell-test-dir
                                                  "test-data"
                                                  "project-with-package-env"))
         (default-directory test-dir))
    (cl-assert (file-regular-p (expand-file-name "foo.cabal" test-dir)))
    (flycheck-haskell-read-cabal-configuration "foo.cabal")
    (let ((conf (flycheck-haskell-read-cabal-configuration "foo.cabal")))
      (let-alist conf
        (should (equal .dependencies '("base")))
        (should (equal .extensions '("OverloadedStrings")))
        (should (equal .languages '("Haskell2010")))
        (should (member "-Wall" .other-options))
        (should (member "-fwarn-haha-no-such-option" .other-options))
        (should (equal .source-directories (list (expand-file-name "lib/" test-dir))))))))


;;; Configuration caching
(ert-deftest flycheck-haskell-clear-config-cache ()
  (unwind-protect
      (progn
        (puthash "foo" "bar" flycheck-haskell-config-cache)
        (should (= (hash-table-count flycheck-haskell-config-cache) 1))
        (flycheck-haskell-clear-config-cache)
        (should (= (hash-table-count flycheck-haskell-config-cache) 0)))
    (clrhash flycheck-haskell-config-cache)))

(ert-deftest flycheck-haskell-get-cached-configuration/no-cache-entry ()
  (should-not (flycheck-haskell-get-cached-configuration
               flycheck-haskell-test-cabal-file)))

(ert-deftest flycheck-haskell-get-cached-configuration/cached-cabal-config ()
  (flycheck-haskell-test-with-cache
    (flycheck-haskell-read-and-cache-configuration
     flycheck-haskell-test-cabal-file)
    (should (= (hash-table-count flycheck-haskell-config-cache) 1))
    (let ((config (flycheck-haskell-get-cached-configuration
                   flycheck-haskell-test-cabal-file)))
      (should (equal config
                     (flycheck-haskell-read-cabal-configuration
                      flycheck-haskell-test-cabal-file))))))

(ert-deftest flycheck-haskell-get-cached-configuration/cached-hpack-config ()
  (skip-unless flycheck-haskell-hpack-executable)
  (flycheck-haskell-test-with-cache
    (flycheck-haskell-read-and-cache-configuration
     flycheck-haskell-test-hpack-file)
    (should (= (hash-table-count flycheck-haskell-config-cache) 1))
    (let ((config (flycheck-haskell-get-cached-configuration
                   flycheck-haskell-test-hpack-file)))
      (should (equal config
                     (flycheck-haskell-read-hpack-configuration
                      flycheck-haskell-test-hpack-file))))))

(ert-deftest flycheck-haskell-get-cached-configuration/file-is-modified ()
  (flycheck-haskell-test-with-cache
    (flycheck-haskell-read-and-cache-configuration
     flycheck-haskell-test-cabal-file)
    (should (flycheck-haskell-get-cached-configuration
             flycheck-haskell-test-cabal-file))
    ;; Wait a second, to ensure that the current time advances
    (sleep-for 1)
    (set-file-times flycheck-haskell-test-cabal-file)
    (should-not (flycheck-haskell-get-cached-configuration
                 flycheck-haskell-test-cabal-file))
    (should (= (hash-table-count flycheck-haskell-config-cache) 0))))

(ert-deftest flycheck-haskell-get-configuration/has-cache-entry ()
  (let* ((cabal-file flycheck-haskell-test-cabal-file)
         (mtime (nth 6 (file-attributes cabal-file))))
    (cl-letf (((symbol-function 'flycheck-haskell-read-cabal-configuration)
               (lambda (_) 'dummy)))
      (flycheck-haskell-test-with-cache
        ;; Create a fake hash entry, which is guaranteed to be newer than the
        ;; actual file
        (puthash cabal-file (cons (time-add mtime (seconds-to-time 1))
                                  'cached-dummy)
                 flycheck-haskell-config-cache)
        (should (eq (flycheck-haskell-get-configuration cabal-file)
                    'cached-dummy))
        (flycheck-haskell-clear-config-cache)
        (should (eq (flycheck-haskell-get-configuration cabal-file)
                    'dummy))))))

(provide 'flycheck-haskell-test)

;;; flycheck-haskell-test.el ends here
