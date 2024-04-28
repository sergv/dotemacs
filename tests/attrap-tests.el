;; attrap-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 13 April 2024
;; Description:

(require 'attrap)

(require 'common)
(require 'eproj)
(require 'eproj-tag-index)
(require 'ert)
(require 'flycheck)
(require 'haskell-autoload)
(require 'haskell-setup)
(require 's)
(require 'tests-utils)

(defvar attrap-select-predefined-option)

(defun attrap-tests-make-ephemeral-haskell-eproj-project (tags)
  (let ((proj (eproj-make-project default-directory
                                  '((languages haskell-mode)
                                    (no-default-proj haskell-mode))))
        (tags-index (empty-eproj-tag-index)))
    (cl-assert (listp tags))
    (dolist (x tags)
      (cl-destructuring-bind (sym file line type props) x
        (eproj-tag-index-add! sym file line type props tags-index)))
    (cl-assert (= (eproj-tag-index-size tags-index)
                  (length tags)))
    (setf (eproj-project/tags proj)
          (list (cons 'haskell-mode
                      (eproj--make-thunk
                       tags-index))))
    proj))

(cl-defmacro attrap-tests--test-buffer-contents-many
    (&key
     name
     flycheck-errors
     action
     contents
     expected-value
     (modes '(haskell-mode haskell-ts-mode))
     ;; If supplied then disable caching and make buffer visit fresh
     ;; temporary directory. This is to facilitate testing with
     ;; ephemeral eproj projects.
     (eproj-project nil))
  `(progn
     ,@(cl-loop
        for mode in modes
        append
        (cl-loop
         for content-entry in contents
         collect
         (cl-destructuring-bind (subname content) content-entry
           `(ert-deftest ,(string->symbol (concat (format "%s/%s" name mode)
                                                  (when subname
                                                    (format "/%s" subname))))
                ()
              (tests-utils--test-buffer-contents
               :action ,action
               :contents ,content
               :expected-value ,expected-value
               :initialisation
               (progn
                 ,(when eproj-project
                    `(progn
                       (cd (make-temp-file ,(s-replace "/" "_" (format "attrap-test-%s" name)) t))
                       (let ((proj ,eproj-project))
                         (puthash default-directory proj *eproj-projects*)
                         (should (eq (eproj-get-project-for-buf-lax (current-buffer))
                                     proj)))))
                 (,mode))
               :post-content-initialisation
               (flycheck-report-current-errors ,flycheck-errors)
               :buffer-id
               ,(unless eproj-project
                  (string->symbol (format "attrap-tests-%s" mode))))))))))

(cl-defmacro attrap-tests--test-buffer-contents-one
    (&key
     name
     flycheck-errors
     action
     contents
     expected-value
     (modes '(haskell-mode haskell-ts-mode))
     (eproj-project nil))
  `(attrap-tests--test-buffer-contents-many
     :name ,name
     :flycheck-errors ,flycheck-errors
     :action ,action
     :contents ,(list (list nil contents))
     :expected-value ,expected-value
     :modes ,modes
     :eproj-project ,eproj-project))

(defun attrap-tests--run-attrap ()
  (cl-letf
      ;; Set up artificial checker because flycheck is disabled and
      ;; it’s best to not enable it for temporary buffers if we can
      ;; help it.
      (((symbol-function 'flycheck-get-checker-for-buffer)
        (lambda ()
          'haskell-dante)))
    (attrap-flycheck (point))))

(attrap-tests--test-buffer-contents-many
 :name attrap/haskell-dante/delete-module-import-1
 :flycheck-errors
 (list
  (let ((linecol (save-excursion
                   (re-search-forward "_|_")
                   (flycheck-line-column-at-pos (point)))))
    (flycheck-error-new
     :line (car linecol)
     :column (cdr linecol)
     :buffer (current-buffer)
     :checker 'haskell-dante
     :message
     (tests-utils--multiline
      "warning: [GHC-66111] [-Wunused-imports]"
      "    The import of ‘Foo.Bar.Baz’ is redundant"
      "      except perhaps to import instances from ‘Foo.Bar.Baz’"
      "    To import instances alone, use: import Foo.Bar.Baz()")
     :level 'warning
     :id nil
     :group nil)))
 :action
 (attrap-tests--run-attrap)
 :contents
 ((a
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Foo.Bar.Baz"
    ""
    ""))
  (b
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Foo.Bar.Baz (foo, bar)"
    ""
    ""))
  (c
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Foo.Bar.Baz"
    "  ( foo"
    "  , bar"
    "  )"
    ""
    "")))
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/delete-module-import-2
 :flycheck-errors
 (list
  (let ((linecol (save-excursion
                   (re-search-forward "_|_")
                   (flycheck-line-column-at-pos (point)))))
    (flycheck-error-new
     :line (car linecol)
     :column (cdr linecol)
     :buffer (current-buffer)
     :checker 'haskell-dante
     :message
     (tests-utils--multiline
      "warning: [GHC-66111] [-Wunused-imports]"
      "    The qualified import of ‘Distribution.ModuleName’ is redundant"
      "      except perhaps to import instances from ‘Distribution.ModuleName’"
      "    To import instances alone, use: import Distribution.ModuleName()")
     :level 'warning
     :id nil
     :group nil)))
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Distribution.ModuleName qualified as ModuleName"
  ""
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_"
  ""))

(attrap-tests--test-buffer-contents-many
 :name attrap/haskell-dante/delete-import-1
 :flycheck-errors
 (list
  (let ((linecol (save-excursion
                   (re-search-forward "_|_")
                   (flycheck-line-column-at-pos (point)))))
    (flycheck-error-new
     :line (car linecol)
     :column (cdr linecol)
     :buffer (current-buffer)
     :checker 'haskell-dante
     :message
     (tests-utils--multiline
      "warning: [GHC-38856] [-Wunused-imports]"
      "    The import of ‘depPkgName, unPackageName’"
      "    from module ‘Distribution.Package’ is redundant")
     :level 'warning
     :id nil
     :group nil)))
 :action
 (attrap-tests--run-attrap)
 :contents
 ((a
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Distribution.Package (unPackageName, depPkgName, PackageName)"
    ""
    ""))
  (b
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Distribution.Package (unPackageName, PackageName, depPkgName)"
    ""
    ""))
  (c
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Distribution.Package (PackageName, depPkgName,unPackageName)"
    ""
    "")))
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Distribution.Package (PackageName)"
  ""
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/delete-import-2
 :flycheck-errors
 (list
  (let ((linecol (save-excursion
                   (re-search-forward "_|_")
                   (flycheck-line-column-at-pos (point)))))
    (flycheck-error-new
     :line (car linecol)
     :column (cdr linecol)
     :buffer (current-buffer)
     :checker 'haskell-dante
     :message
     (tests-utils--multiline
      "warning: [GHC-38856] [-Wunused-imports]"
      "    The import of ‘Executable(buildInfo), Library(exposedModules),"
      "                   hcOptions, Library(libBuildInfo), Executable(modulePath),"
      "                   usedExtensions’"
      "    from module ‘Distribution.PackageDescription’ is redundant")
     :level 'warning
     :id nil
     :group nil)))
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Distribution.PackageDescription (GenericPackageDescription, PackageDescription(..), usedExtensions, hcOptions, exeName, buildInfo, modulePath, libBuildInfo, exposedModules)"
  ""
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Distribution.PackageDescription (GenericPackageDescription, PackageDescription(..), exeName)"
  ""
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/replace-1
 :flycheck-errors
 (list
  (let ((linecol (save-excursion
                   (re-search-forward "_|_")
                   (flycheck-line-column-at-pos (point)))))
    (flycheck-error-new
     :line (car linecol)
     :column (cdr linecol)
     :buffer (current-buffer)
     :checker 'haskell-dante
     :message
     (tests-utils--multiline
      "error: [GHC-76037]"
      "    Not in scope: type constructor or class ‘MonadMask’"
      "    Suggested fix:"
      "      Perhaps use ‘MC.MonadMask’ (imported from Control.Monad.Catch)")
     :level 'error
     :id nil
     :group nil)))
 :action
 (let ((attrap-select-predefined-option
        "replace MonadMask by MC.MonadMask from Control.Monad.Catch"))
   (attrap-tests--run-attrap))
 :contents
 (tests-utils--multiline
  ""
  "import Control.Monad.Catch qualified as MC"
  ""
  "foo :: _|_MonadMask m => a -> m a"
  "foo = undefined"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Control.Monad.Catch qualified as MC"
  ""
  "foo :: _|_MC.MonadMask m => a -> m a"
  "foo = undefined"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-binding-1
 :flycheck-errors
 (list
  (let ((linecol (save-excursion
                   (re-search-forward "_|_")
                   (flycheck-line-column-at-pos (point)))))
    (flycheck-error-new
     :line (car linecol)
     :column (cdr linecol)
     :buffer (current-buffer)
     :checker 'haskell-dante
     :message
     (tests-utils--multiline
      "error: [GHC-44432]"
      "    The type signature for ‘foo’"
      "      lacks an accompanying binding")
     :level 'error
     :id nil
     :group nil)))
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "_|_foo :: MonadMask m => a -> m a"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "_|_foo :: MonadMask m => a -> m a"
  "foo = _"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-binding-2
 :flycheck-errors
 (list
  (let ((linecol (save-excursion
                   (re-search-forward "_|_")
                   (flycheck-line-column-at-pos (point)))))
    (flycheck-error-new
     :line (car linecol)
     :column (cdr linecol)
     :buffer (current-buffer)
     :checker 'haskell-dante
     :message
     (tests-utils--multiline
      "error: [GHC-44432]"
      "    The type signature for ‘foo’"
      "      lacks an accompanying binding")
     :level 'error
     :id nil
     :group nil)))
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "_|_foo"
  "  :: MonadMask m"
  "  => a"
  "  -> m a"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "_|_foo"
  "  :: MonadMask m"
  "  => a"
  "  -> m a"
  "foo = _"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/export-1
 :flycheck-errors
 (list
  (let ((linecol (save-excursion
                   (re-search-forward "_|_")
                   (flycheck-line-column-at-pos (point)))))
    (flycheck-error-new
     :line (car linecol)
     :column (cdr linecol)
     :buffer (current-buffer)
     :checker 'haskell-dante
     :message
     (tests-utils--multiline
      "warning: [GHC-40910] [-Wunused-top-binds]"
      "    Defined but not used: ‘foo’")
     :level 'error
     :id nil
     :group nil)))
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "module Foo () where"
  ""
  "foo"
  "  :: MonadMask m"
  "  => a"
  "  -> m a"
  "_|_foo = _"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "module Foo (foo) where"
  ""
  "foo"
  "  :: MonadMask m"
  "  => a"
  "  -> m a"
  "_|_foo = _"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-import-1
 :flycheck-errors
 (list
  (let ((linecol (save-excursion
                   (re-search-forward "_|_")
                   (flycheck-line-column-at-pos (point)))))
    (flycheck-error-new
     :line (car linecol)
     :column (cdr linecol)
     :buffer (current-buffer)
     :checker 'haskell-dante
     :message
     (tests-utils--multiline
      "error: [GHC-76037]"
      "    • Not in scope: ‘osstr’"
      "    • In the quasi-quotation: [osstr|test|]")
     :level 'error
     :id nil
     :group nil)))
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "module Foo () where"
  ""
  "main = putStrLn _|_[osstr|test|]"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "module Foo () where"
  ""
  "import Test.Foo.Bar (osstr)"
  ""
  "main = putStrLn _|_[osstr|test|]"
  "")
 :eproj-project
 (attrap-tests-make-ephemeral-haskell-eproj-project
  '(("osstr" "/tmp/Test/Foo/Bar.hs" 100 ?f nil))))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-import-2
 :flycheck-errors
 (list
  (let ((linecol (save-excursion
                   (re-search-forward "_|_")
                   (flycheck-line-column-at-pos (point)))))
    (flycheck-error-new
     :line (car linecol)
     :column (cdr linecol)
     :buffer (current-buffer)
     :checker 'haskell-dante
     :message
     (tests-utils--multiline
      "error: [GHC-76037]"
      "    Not in scope: type constructor or class ‘WithCallStack’")
     :level 'error
     :id nil
     :group nil)))
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "module Foo () where"
  ""
  "import Control.Monad.State"
  "-- import MyProject.List.Ext (mySort)"
  ""
  "foo :: (_|_WithCallStack, MonadState Int m) => Int -> m Int"
  "foo = undefined"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "module Foo () where"
  ""
  "import Control.Monad.State"
  "import Test.Foo.Bar (WithCallStack)"
  "-- import MyProject.List.Ext (mySort)"
  ""
  "foo :: (_|_WithCallStack, MonadState Int m) => Int -> m Int"
  "foo = undefined"
  "")
 :eproj-project
 (attrap-tests-make-ephemeral-haskell-eproj-project
  '(("WithCallStack" "/tmp/Test/Foo/Bar.hs" 100 ?t nil))))

(provide 'attrap-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; attrap-tests.el ends here
