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
        (eproj-tag-index-add! sym file line type t props tags-index)))
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
     error-message
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
               (flycheck-report-current-errors
                (list
                 (let ((linecol (save-excursion
                                  (unless (re-search-forward "_|_" nil t)
                                    (error "Cannot find ‘_|_’ marker in test"))
                                  (flycheck-line-column-at-pos (point)))))
                   (flycheck-error-new
                    :line (car linecol)
                    :column (cdr linecol)
                    :buffer (current-buffer)
                    :checker 'haskell-dante
                    :message ,error-message
                    :level 'error
                    :id nil
                    :group nil))))
               :buffer-id
               ,(unless eproj-project
                  (string->symbol (format "attrap-tests-%s" mode))))))))))

(cl-defmacro attrap-tests--test-buffer-contents-one
    (&key
     name
     error-message
     action
     contents
     expected-value
     (modes '(haskell-mode haskell-ts-mode))
     (eproj-project nil))
  `(attrap-tests--test-buffer-contents-many
     :name ,name
     :error-message ,error-message
     :action ,action
     :contents ,(list (list nil contents))
     :expected-value ,expected-value
     :modes ,modes
     :eproj-project ,eproj-project))

(defmacro attrap-tests--wrap-run-attrap (&rest body)
  (declare (indent 0))
  `(cl-letf
       ;; Set up artificial checker because flycheck is disabled and
       ;; it’s best to not enable it for temporary buffers if we can
       ;; help it.
       (((symbol-function 'flycheck-get-checker-for-buffer)
         (lambda ()

           'haskell-dante)))
     ,@body))

(defun attrap-tests--run-attrap ()
  (attrap-tests--wrap-run-attrap
   (attrap-flycheck (point))))

(defun attrap-tests--run-attrap-check-fixes (expected-fixes)
  (attrap-tests--wrap-run-attrap
    (attrap-flycheck--with-messages-and-checker (point) checker messages
      (let ((fixes (attrap-flycheck--collect-fixes checker messages)))
        (should (equal (-map #'car fixes)
                       expected-fixes))
        (attrap-select-and-apply-option fixes)))))

(attrap-tests--test-buffer-contents-many
 :name attrap/haskell-dante/delete-import-1
 :error-message
 (tests-utils--multiline
  "warning: [GHC-38856] [-Wunused-imports]"
  "    The import of ‘depPkgName, unPackageName’"
  "    from module ‘Distribution.Package’ is redundant")
 :action
 (attrap-tests--run-attrap)
 :contents
 ((a
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Distribution.Package (unPackageName, depPkgName, PackageName)"
    ""))
  (b
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Distribution.Package (unPackageName, PackageName, depPkgName)"
    ""))
  (c
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Distribution.Package (PackageName, depPkgName,unPackageName)"
    ""))
  (d
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Distribution.Package (PackageName, unPackageName, depPkgName)"
    ""))
  (e
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Distribution.Package (PackageName, depPkgName, unPackageName)"
    "")))
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Distribution.Package (PackageName)"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/delete-import-2
 :error-message
 (tests-utils--multiline
  "warning: [GHC-38856] [-Wunused-imports]"
  "    The import of ‘Executable(buildInfo), Library(exposedModules),"
  "                   hcOptions, Library(libBuildInfo), Executable(modulePath),"
  "                   usedExtensions’"
  "    from module ‘Distribution.PackageDescription’ is redundant")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Distribution.PackageDescription (GenericPackageDescription, PackageDescription(..), usedExtensions, hcOptions, exeName, buildInfo, modulePath, libBuildInfo, exposedModules)"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Distribution.PackageDescription (GenericPackageDescription, PackageDescription(..), exeName)"
  ""))

(attrap-tests--test-buffer-contents-many
 :name attrap/haskell-dante/delete-import-3
 :error-message
 (tests-utils--multiline
  "warning: [GHC-66111] [-Wunused-imports]"
  "    The import of ‘Foo.Bar.Baz’ is redundant"
  "      except perhaps to import instances from ‘Foo.Bar.Baz’"
  "    To import instances alone, use: import Foo.Bar.Baz()")
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

(attrap-tests--test-buffer-contents-many
 :name attrap/haskell-dante/delete-import-3a
 :error-message
 (tests-utils--multiline
  "error: [GHC-66111] [-Wunused-imports, Werror=unused-imports]"
  "    The import of ‘Foo.Bar.Baz’ is redundant"
  "      except perhaps to import instances from ‘Foo.Bar.Baz’"
  "    To import instances alone, use: import Foo.Bar.Baz()")
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
 :name attrap/haskell-dante/delete-import-4
 :error-message
 (tests-utils--multiline
  "warning: [GHC-66111] [-Wunused-imports]"
  "    The qualified import of ‘Distribution.ModuleName’ is redundant"
  "      except perhaps to import instances from ‘Distribution.ModuleName’"
  "    To import instances alone, use: import Distribution.ModuleName()")
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

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/delete-import-5a
 :error-message
 (tests-utils--multiline
  "error: [GHC-38856] [-Wunused-imports, Werror=unused-imports]"
  "    The import of ‘PPTokens, PPTokens’"
  "    from module ‘Haskell.Language.Lexer.Types’ is redundant")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Haskell.Language.Lexer.Types (Pos, ServerToken, processTokens, PPTokens(PPTokens))"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Haskell.Language.Lexer.Types (Pos, ServerToken, processTokens)"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/delete-import-5b
 :error-message
 (tests-utils--multiline
  "error: [GHC-38856] [-Wunused-imports, Werror=unused-imports]"
  "    The import of ‘PPTokens, PPTokens2’"
  "    from module ‘Haskell.Language.Lexer.Types’ is redundant")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Haskell.Language.Lexer.Types (Pos, ServerToken, processTokens, PPTokens(PPTokens2))"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Haskell.Language.Lexer.Types (Pos, ServerToken, processTokens)"
  ""))

(attrap-tests--test-buffer-contents-one
 :modes (haskell-ts-mode)
 :name attrap/haskell-dante/delete-import-5c
 :error-message
 (tests-utils--multiline
  "error: [GHC-38856] [-Wunused-imports, Werror=unused-imports]"
  "    The import of ‘PPTokens, PPTokens2’"
  "    from module ‘Haskell.Language.Lexer.Types’ is redundant")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Haskell.Language.Lexer.Types (Pos, ServerToken, processTokens, PPTokens (PPTokens2))"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Haskell.Language.Lexer.Types (Pos, ServerToken, processTokens)"
  ""))

(attrap-tests--test-buffer-contents-one
 :modes (haskell-ts-mode)
 :name attrap/haskell-dante/delete-import-5d
 :error-message
 (tests-utils--multiline
  "error: [GHC-38856] [-Wunused-imports, Werror=unused-imports]"
  "    The import of ‘PPTokens’"
  "    from module ‘Haskell.Language.Lexer.Types’ is redundant")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Haskell.Language.Lexer.Types (Pos, ServerToken, processTokens, PPTokens(PPTokens, Foo))"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Haskell.Language.Lexer.Types (Pos, ServerToken, processTokens, PPTokens(Foo))"
  ""))

(attrap-tests--test-buffer-contents-many
 :name attrap/haskell-dante/delete-import-6
 :error-message
 (tests-utils--multiline
  "warning: [GHC-38856] [-Wunused-imports]"
  "    The import of ‘foo, bar’"
  "    from module ‘Distribution.Package’ is redundant")
 :action
 (attrap-tests--run-attrap)
 :contents
 ((a
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Distribution.Package (foo, quux, bar)"
    ""
    "")))
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
    "_|_import Distribution.Package (quux)"
  ""
  ""))

(attrap-tests--test-buffer-contents-many
 :name attrap/haskell-dante/delete-import-6a
 :error-message
 (tests-utils--multiline
  "warning: [GHC-38856] [-Wunused-imports]"
  "    The import of ‘foo, bar’"
  "    from module ‘Distribution.Package’ is redundant")
 :action
 (attrap-tests--run-attrap)
 :contents
 ((a
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Distribution.Package (foo, quux, bar, bar)"
    ""
    "")))
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
    "_|_import Distribution.Package (quux, bar)"
  ""
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/delete-import-7a
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "error: [GHC-38856] [-Wunused-imports, Werror=unused-imports]"
  "    The import of ‘foo’"
  "    from module ‘Data.Foo’ is redundant")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Quux"
  "import Data.Foo"
  "  ( _|_foo"
  "  , bar"
  "  , quux"
  "  )"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
  "import Data.Foo"
  "  ( _|_bar"
  "  , quux"
  "  )"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/delete-import-7b
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "error: [GHC-38856] [-Wunused-imports, Werror=unused-imports]"
  "    The import of ‘foo’"
  "    from module ‘Data.Foo’ is redundant")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Quux"
  "import Data.Foo"
  "  ( bar"
  "  , quux"
  "  , _|_foo"
  "  )"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
  "import Data.Foo"
  "  ( bar"
  "  , quux_|_"
  "  )"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/delete-import-7c
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "error: [GHC-38856] [-Wunused-imports, Werror=unused-imports]"
  "    The import of ‘foo’"
  "    from module ‘Data.Foo’ is redundant")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Quux"
  "import Data.Foo"
  "  ( bar"
  "  , _|_foo"
  "  , quux"
  "  )"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
  "import Data.Foo"
  "  ( bar_|_"
  "  , quux"
  "  )"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/replace-1
 :error-message
 (tests-utils--multiline
  "error: [GHC-76037]"
  "    Not in scope: type constructor or class ‘MonadMask’"
  "    Suggested fix:"
  "      Perhaps use ‘MC.MonadMask’ (imported from Control.Monad.Catch)")
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
 :name attrap/haskell-dante/replace-2
 :error-message
 (tests-utils--multiline
  "warning: [GHC-88464] [-Wdeferred-out-of-scope-variables]"
  "    Variable not in scope:"
  "      withWindow"
  "        :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO a0"
  "    Suggested fix:"
  "      Perhaps use ‘GLFW.withWindow’ (imported from Graphics.UI.GLFW)")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Graphics.UI.GLFW qualified as GLFW"
  ""
  "main = do"
  "  _|_withWindow width height \"Turgtle Geometry\" $ \win -> do"
  "    pure ()"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Graphics.UI.GLFW qualified as GLFW"
  ""
  "main = do"
  "  _|_GLFW.withWindow width height \"Turgtle Geometry\" $ \win -> do"
  "    pure ()"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/replace-2a
 :error-message
 (tests-utils--multiline
  "error: [GHC-88464] [-Wdeferred-out-of-scope-variables, Werror=deferred-out-of-scope-variables]"
  "    Variable not in scope:"
  "      withWindow"
  "        :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO a0"
  "    Suggested fix:"
  "      Perhaps use ‘GLFW.withWindow’ (imported from Graphics.UI.GLFW)")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Graphics.UI.GLFW qualified as GLFW"
  ""
  "main = do"
  "  _|_withWindow width height \"Turgtle Geometry\" $ \win -> do"
  "    pure ()"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Graphics.UI.GLFW qualified as GLFW"
  ""
  "main = do"
  "  _|_GLFW.withWindow width height \"Turgtle Geometry\" $ \win -> do"
  "    pure ()"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/replace-3
 :error-message
 (tests-utils--multiline
  "error: [GHC-76037]"
  "    Not in scope: data constructor ‘SocketType’"
  "    Suggested fix:"
  "      Perhaps use ‘DirInternals.SocketType’ (imported from System.Posix.Directory.Internals)")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo typ = do  "
  "  case typ of"
  "    UnknownType -> getFileType $ coerce path"
  "    _|_SocketType -> pure Other"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo typ = do  "
  "  case typ of"
  "    UnknownType -> getFileType $ coerce path"
  "    _|_DirInternals.SocketType -> pure Other"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/replace-4
 :error-message
 (tests-utils--multiline
  "warning: [GHC-88464] [-Wdeferred-out-of-scope-variables]"
  "    Variable not in scope: GHC.Stack.prettyCallSack"
  "    Note: The module ‘GHC.Stack’ does not export ‘prettyCallSack’."
  "    Suggested fix:"
  "      Perhaps use one of these:"
  "        ‘GHC.Stack.prettyCallStack’ (imported from GHC.Stack),"
  "        ‘GHC.Stack.getCallStack’ (imported from GHC.Stack),"
  "        ‘GHC.Stack.emptyCallStack’ (imported from GHC.Stack)")
 :action
 (let ((attrap-select-predefined-option
        "replace GHC.Stack.prettyCallSack by GHC.Stack.prettyCallStack from GHC.Stack"))
   (attrap-tests--run-attrap-check-fixes
    '("replace GHC.Stack.prettyCallSack by GHC.Stack.prettyCallStack from GHC.Stack"
      "replace GHC.Stack.prettyCallSack by GHC.Stack.getCallStack from GHC.Stack"
      "replace GHC.Stack.prettyCallSack by GHC.Stack.emptyCallStack from GHC.Stack")))
 :contents
 (tests-utils--multiline
  ""
  "import qualified GHC.Stack"
  ""
  "foo = _|_GHC.Stack.prettyCallSack"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import qualified GHC.Stack"
  ""
  "foo = _|_GHC.Stack.prettyCallStack"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/replace-5a
 :error-message
 (tests-utils--multiline
  "error: [GHC-22385]"
  "    Not in scope: record field ‘recordFiled1’"
  "    Suggested fix:"
  "      Perhaps use record field of Record ‘recordField1’ (line 26)")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "data Record = Record"
  "  { recordField1   :: Int"
  "  , recordWhatever :: Int"
  "  }"
  ""
  "test :: Record -> Record"
  "test x = x { _|_recordFiled1 = 100 }"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "data Record = Record"
  "  { recordField1   :: Int"
  "  , recordWhatever :: Int"
  "  }"
  ""
  "test :: Record -> Record"
  "test x = x { _|_recordField1 = 100 }"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/replace-5b
 :error-message
 (tests-utils--multiline
  "error: [GHC-22385]"
  "    Not in scope: record field ‘recordFiled1’"
  "    Suggested fix:"
  "      Perhaps use one of these:"
  "        record field of Record ‘recordField1’ (line 26),"
  "        record field of Record ‘recordField2’ (line 27)")
 :action
 (let ((attrap-select-predefined-option "replace recordFiled1 by recordField2 from line 27"))
   (attrap-tests--run-attrap-check-fixes
    '("replace recordFiled1 by recordField1 from line 26"
      "replace recordFiled1 by recordField2 from line 27")))
 :contents
 (tests-utils--multiline
  ""
  "data Record = Record"
  "  { recordField1 :: Int"
  "  , recordField2 :: Int"
  "  }"
  ""
  "test :: Record -> Record"
  "test x = x { _|_recordFiled1 = 100 }"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "data Record = Record"
  "  { recordField1 :: Int"
  "  , recordField2 :: Int"
  "  }"
  ""
  "test :: Record -> Record"
  "test x = x { _|_recordField2 = 100 }"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-binding-1
 :error-message
 (tests-utils--multiline
  "error: [GHC-44432]"
  "    The type signature for ‘foo’"
  "      lacks an accompanying binding")
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
 :error-message
 (tests-utils--multiline
  "error: [GHC-44432]"
  "    The type signature for ‘foo’"
  "      lacks an accompanying binding")
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
 :error-message
 (tests-utils--multiline
  "warning: [GHC-40910] [-Wunused-top-binds]"
  "    Defined but not used: ‘foo’")
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
 :name attrap/haskell-dante/export-1a
 :error-message
 (tests-utils--multiline
  "error: [GHC-40910] [-Wunused-top-binds, Werror=unused-top-binds]"
  "    Defined but not used: ‘foo’")
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
 :name attrap/haskell-dante/export-2
 :error-message
 (tests-utils--multiline
  "error: [GHC-40910] [-Wunused-top-binds]"
  "    Defined but not used: ‘foo’")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "module Foo () where"
  ""
  "_|_foreign import ccall unsafe \"foo.h foo\" foo"
  "  :: Ptr CInt -> CInt -> IO CInt"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "module Foo (foo) where"
  ""
  "_|_foreign import ccall unsafe \"foo.h foo\" foo"
  "  :: Ptr CInt -> CInt -> IO CInt"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-import-1
 :error-message
 (tests-utils--multiline
  "error: [GHC-76037]"
  "    • Not in scope: ‘osstr’"
  "    • In the quasi-quotation: [osstr|test|]")
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
 :error-message
 (tests-utils--multiline
  "error: [GHC-76037]"
  "    Not in scope: type constructor or class ‘WithCallStack’")
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

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-import-3
 :error-message
 (tests-utils--multiline
  "warning: [GHC-88464] [-Wdeferred-out-of-scope-variables]"
  "    Data constructor not in scope: Compose")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Decombobulate"
  ""
  "main :: IO ()"
  "main = do"
  "  print _|_Compose"
  "  pure ()"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Decombobulate"
  "import Test.Foo.Bar (Compose(Compose))"
  ""
  "main :: IO ()"
  "main = do"
  "  print _|_Compose"
  "  pure ()"
  "")
 :eproj-project
 (attrap-tests-make-ephemeral-haskell-eproj-project
  '(("Compose" "/tmp/Test/Foo/Bar.hs" 100 ?C ((parent "Compose" . ?t)))
    ("Compose" "/tmp/Test/Foo/Bar.hs" 100 ?t nil))))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-import-3a
 :error-message
 (tests-utils--multiline
  "warning: [GHC-88464] [-Wdeferred-out-of-scope-variables]"
  "    Data constructor not in scope: Compose")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Decombobulate"
  ""
  "main :: IO ()"
  "main = do"
  "  print _|_Compose"
  "  pure ()"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Decombobulate"
  "import Test.Foo.Bar (ComposeT(Compose))"
  ""
  "main :: IO ()"
  "main = do"
  "  print _|_Compose"
  "  pure ()"
  "")
 :eproj-project
 (attrap-tests-make-ephemeral-haskell-eproj-project
  '(("Compose" "/tmp/Test/Foo/Bar.hs" 100 ?C ((parent "ComposeT" . ?t)))
    ("ComposeT" "/tmp/Test/Foo/Bar.hs" 100 ?t nil))))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-import-3b
 :error-message
 (tests-utils--multiline
  "error: [GHC-88464] [-Wdeferred-out-of-scope-variables, Werror=deferred-out-of-scope-variables]"
  "    Data constructor not in scope: Compose")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Decombobulate"
  ""
  "main :: IO ()"
  "main = do"
  "  print _|_Compose"
  "  pure ()"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Decombobulate"
  "import Test.Foo.Bar (ComposeT(Compose))"
  ""
  "main :: IO ()"
  "main = do"
  "  print _|_Compose"
  "  pure ()"
  "")
 :eproj-project
 (attrap-tests-make-ephemeral-haskell-eproj-project
  '(("Compose" "/tmp/Test/Foo/Bar.hs" 100 ?C ((parent "ComposeT" . ?t)))
    ("ComposeT" "/tmp/Test/Foo/Bar.hs" 100 ?t nil))))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-import-4
 :error-message
 (tests-utils--multiline
  "error: [GHC-76037]"
  "    Not in scope: data constructor ‘Compose’")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Decombobulate"
  ""
  "main :: IO ()"
  "main = do"
  "  print (\\(_|_Compose y) -> y) $ x"
  "  pure ()"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Decombobulate"
  "import Test.Foo.Bar (Compose(Compose))"
  ""
  "main :: IO ()"
  "main = do"
  "  print (\\(_|_Compose y) -> y) $ x"
  "  pure ()"
  "")
 :eproj-project
 (attrap-tests-make-ephemeral-haskell-eproj-project
  '(("Compose" "/tmp/Test/Foo/Bar.hs" 100 ?C ((parent "Compose" . ?t)))
    ("Compose" "/tmp/Test/Foo/Bar.hs" 100 ?t nil))))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-import-4a
 :error-message
 (tests-utils--multiline
  "error: [GHC-76037]"
  "    Not in scope: data constructor ‘Compose’")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Decombobulate"
  ""
  "main :: IO ()"
  "main = do"
  "  print (\\(_|_Compose y) -> y) $ x"
  "  pure ()"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Decombobulate"
  "import Test.Foo.Bar ((:++:)(Compose))"
  ""
  "main :: IO ()"
  "main = do"
  "  print (\\(_|_Compose y) -> y) $ x"
  "  pure ()"
  "")
 :eproj-project
 (attrap-tests-make-ephemeral-haskell-eproj-project
  '(("Compose" "/tmp/Test/Foo/Bar.hs" 100 ?C ((parent ":++:" . ?t)))
    (":++:" "/tmp/Test/Foo/Bar.hs" 100 ?t nil))))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-import-4aa
 :error-message
 (tests-utils--multiline
  "error: [GHC-76037]"
  "    Not in scope: data constructor ‘Foo’")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Decombobulate"
  ""
  "main :: IO ()"
  "main = do"
  "  print (\\(_|_Foo y) -> y) $ x"
  "  pure ()"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Decombobulate"
  "import Test.Foo.Bar (Bar(Foo))"
  ""
  "main :: IO ()"
  "main = do"
  "  print (\\(_|_Foo y) -> y) $ x"
  "  pure ()"
  "")
 :eproj-project
 (attrap-tests-make-ephemeral-haskell-eproj-project
  '(
    ;; Tags produced by fast-tags may not contain parent annotation
    ;; for patterns. But in case they do then this test ensures that
    ;; everything on the Emacs side will be handled correctly.
    ("Foo" "/tmp/Test/Foo/Bar.hs" 100 ?p ((parent "Bar" . ?t)))
    ("Bar" "/tmp/Test/Foo/Bar.hs" 100 ?t nil))))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-import-4b
 :error-message
 (tests-utils--multiline
  "error: [GHC-76037]"
  "    Not in scope: data constructor ‘:++:’")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Decombobulate"
  ""
  "main :: IO ()"
  "main = do"
  "  print (\\(x _|_:++: y) -> y) $ x"
  "  pure ()"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Decombobulate"
  "import Test.Foo.Bar (Compose((:++:)))"
  ""
  "main :: IO ()"
  "main = do"
  "  print (\\(x _|_:++: y) -> y) $ x"
  "  pure ()"
  "")
 :eproj-project
 (attrap-tests-make-ephemeral-haskell-eproj-project
  '((":++:" "/tmp/Test/Foo/Bar.hs" 100 ?C ((parent "Compose" . ?t)))
    ("Compose" "/tmp/Test/Foo/Bar.hs" 100 ?t nil))))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-import-4c
 :error-message
 (tests-utils--multiline
  "error: [GHC-76037]"
  "    Not in scope: data constructor ‘:++:’")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Decombobulate"
  ""
  "main :: IO ()"
  "main = do"
  "  print (\\(x _|_:++: y) -> y) $ x"
  "  pure ()"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Decombobulate"
  "import Test.Foo.Bar ((:||:)((:++:)))"
  ""
  "main :: IO ()"
  "main = do"
  "  print (\\(x _|_:++: y) -> y) $ x"
  "  pure ()"
  "")
 :eproj-project
 (attrap-tests-make-ephemeral-haskell-eproj-project
  '((":++:" "/tmp/Test/Foo/Bar.hs" 100 ?C ((parent ":||:" . ?t)))
    (":||:" "/tmp/Test/Foo/Bar.hs" 100 ?t nil))))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-import-generically-from-base-gets-wildcard-5a
 :error-message
 (tests-utils--multiline
  "error: [GHC-76037]"
  "    Not in scope: type constructor or class ‘Generically’")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Decombobulate"
  ""
  "data Foo = Foo"
  "  { fooField :: Any"
  "  }"
  "  deriving (Eq, Ord, Show, Generic)"
  "  deriving (Semigroup, Monoid) via _|_Generically FooField"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Decombobulate"
  "import GHC.Generics (Generically(..))"
  ""
  "data Foo = Foo"
  "  { fooField :: Any"
  "  }"
  "  deriving (Eq, Ord, Show, Generic)"
  "  deriving (Semigroup, Monoid) via _|_Generically FooField"
  "")
 :eproj-project
 (attrap-tests-make-ephemeral-haskell-eproj-project
  '(("Generically" "/tmp/GHC/Generics.hs" 100 ?t nil))))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-import-generically-from-nonbase-no-wildcard-5b
 :error-message
 (tests-utils--multiline
  "error: [GHC-76037]"
  "    Not in scope: type constructor or class ‘Generically’")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Decombobulate"
  ""
  "data Foo = Foo"
  "  { fooField :: Any"
  "  }"
  "  deriving (Eq, Ord, Show, Generic)"
  "  deriving (Semigroup, Monoid) via _|_Generically FooField"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Decombobulate"
  "import Foo.Bar (Generically)"
  ""
  "data Foo = Foo"
  "  { fooField :: Any"
  "  }"
  "  deriving (Eq, Ord, Show, Generic)"
  "  deriving (Semigroup, Monoid) via _|_Generically FooField"
  "")
 :eproj-project
 (attrap-tests-make-ephemeral-haskell-eproj-project
  '(("Generically" "/tmp/Foo/Bar.hs" 100 ?t nil))))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-import-generically-from-nonbase-no-wildcard-5c
 :error-message
 (tests-utils--multiline
  "error: [GHC-76037]"
  "    Not in scope: type constructor or class ‘Generically’")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Decombobulate"
  ""
  "data Foo = Foo"
  "  { fooField :: Any"
  "  }"
  "  deriving (Eq, Ord, Show, Generic)"
  "  deriving (Semigroup, Monoid) via _|_Generically FooField"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Decombobulate"
  "import Foo.Bar (Generically)"
  ""
  "data Foo = Foo"
  "  { fooField :: Any"
  "  }"
  "  deriving (Eq, Ord, Show, Generic)"
  "  deriving (Semigroup, Monoid) via _|_Generically FooField"
  "")
 :eproj-project
 (attrap-tests-make-ephemeral-haskell-eproj-project
  '(("Generically" "/tmp/Foo/Bar.hs" 100 ?t nil)
    ("Generically" "/tmp/GHC/Generics.hs" 100 ?C nil))))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-to-import-import-list-1
 :error-message
 (tests-utils--multiline
  "warning: [GHC-88464] [-Wdeferred-out-of-scope-variables]"
  "Variable not in scope: foo :: Int -> Double"
  "Suggested fix:"
  "  Add ‘foo’ to the import list in the import of"
  "  ‘Decombobulate’"
  "  (at /foo/bar/baz/Quux.hs:2:1-26)")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Decombobulate (bar)"
  ""
  "test :: Int -> Double"
  "test x = _|_foo x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Decombobulate (bar, foo)"
  ""
  "test :: Int -> Double"
  "test x = _|_foo x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-to-import-import-list-2
 :error-message
 (tests-utils--multiline
  "warning: [GHC-88464] [-Wdeferred-out-of-scope-variables]"
  "    Variable not in scope: hPutChar :: Handle -> Char -> IO ()"
  "    Suggested fixes:"
  "      • Perhaps use ‘putChar’ (imported from Prelude)"
  "      • Add ‘hPutChar’ to the import list in the import of ‘System.IO’"
  "        (at /foo/bar/baz/Quux.hs:2:1-25).")
 :action
 (let ((attrap-select-predefined-option
        "add to import list of ‘System.IO’"))
   (attrap-tests--run-attrap))
 :contents
 (tests-utils--multiline
  ""
  "import System.IO (Handle)"
  ""
  "writeTo :: Handle -> [String] -> IO ()"
  "writeTo dest xs = do"
  "    list $ traverse (traverse char) xs"
  "    where"
  "        list :: IO a -> IO a"
  "        list action = C8.hPut dest \"(\" *> action <* C8.hPut dest \")\""
  ""
  "        char :: Char -> IO ()"
  "        char = _|_hPutChar dest"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import System.IO (Handle, hPutChar)"
  ""
  "writeTo :: Handle -> [String] -> IO ()"
  "writeTo dest xs = do"
  "    list $ traverse (traverse char) xs"
  "    where"
  "        list :: IO a -> IO a"
  "        list action = C8.hPut dest \"(\" *> action <* C8.hPut dest \")\""
  ""
  "        char :: Char -> IO ()"
  "        char = _|_hPutChar dest"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-to-import-import-list-3
 :error-message
 (tests-utils--multiline
  "warning: [GHC-88464] [-Wdeferred-out-of-scope-variables]"
  "    Data constructor not in scope:"
  "      Foo :: Bar (Either Int Double)"
  "    Suggested fixes:"
  "      • Perhaps use one of these:"
  "          variable ‘foo’ (imported from Foo.Bar),"
  "          ‘Foo1’ (imported from Foo.Foo1),"
  "          ‘Foo2’ (imported from Foo.Foo2)"
  "      • Add ‘Foo’ to the import list in the import of ‘Foo.Decombobulate’"
  "        (at /foo/bar/baz/Quux.hs:2:1-27).")
 :action
 (let ((attrap-select-predefined-option
        "add to import list of ‘Foo.Decombobulate’"))
   (attrap-tests--run-attrap))
 :contents
 (tests-utils--multiline
  ""
  "import Foo.Decombobulate ()"
  ""
  "test :: FooType -> IO ()"
  "test (_|_Foo x) = undefined"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Foo.Decombobulate (FooType(Foo))"
  ""
  "test :: FooType -> IO ()"
  "test (_|_Foo x) = undefined"
  "")
 :eproj-project
 (attrap-tests-make-ephemeral-haskell-eproj-project
  '(("Foo" "/tmp/Foo/Decombobulate.hs" 100 ?C ((parent "FooType" . ?t)))
    ("FooType" "/tmp/Foo/Decombobulate.hs" 100 ?t nil)

    ("Foo" "/tmp/Foo/Quux.hs" 100 ?C ((parent "FooType1" . ?t)))
    ("FooType1" "/tmp/Foo/Quux.hs" 100 ?t nil)

    ("FooType" "/tmp/Foo/Frobnicate.hs" 200 ?t nil))))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-to-import-import-list-4
 :error-message
 (tests-utils--multiline
  "warning: [GHC-88464] [-Wdeferred-out-of-scope-variables]"
  "    Data constructor not in scope:"
  "      Foo :: Bar (Either Int Double)"
  "    Suggested fixes:"
  "      • Perhaps use one of these:"
  "          variable ‘foo’ (imported from Foo.Bar),"
  "          ‘Foo1’ (imported from Foo.Foo1),"
  "          ‘Foo2’ (imported from Foo.Foo2)"
  "      • Add ‘Foo’ to the import list in the import of ‘Foo.Decombobulate’"
  "        (at /foo/bar/baz/Quux.hs:2:1-39).")
 :action
 (let ((attrap-select-predefined-option
        "add to import list of ‘Foo.Decombobulate’"))
   (attrap-tests--run-attrap))
 :contents
 (tests-utils--multiline
  ""
  "import Foo.Decombobulate (Bar(..), Baz)"
  ""
  "test :: FooType -> IO ()"
  "test (_|_Foo x) = undefined"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Foo.Decombobulate (Bar(..), Baz, FooType(Foo))"
  ""
  "test :: FooType -> IO ()"
  "test (_|_Foo x) = undefined"
  "")
 :eproj-project
 (attrap-tests-make-ephemeral-haskell-eproj-project
  '(("Foo" "/tmp/Foo/Decombobulate.hs" 100 ?C ((parent "FooType" . ?t)))
    ("FooType" "/tmp/Foo/Decombobulate.hs" 100 ?t nil)

    ("Foo" "/tmp/Foo/Quux.hs" 100 ?C ((parent "FooType1" . ?t)))
    ("FooType1" "/tmp/Foo/Quux.hs" 100 ?t nil)

    ("FooType" "/tmp/Foo/Frobnicate.hs" 200 ?t nil))))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-to-import-import-list-5
 :error-message
 (tests-utils--multiline
  "error: [GHC-76037]"
  "    Not in scope:"
  "      type constructor or class ‘Decombobulate’"
  "    Suggested fix:"
  "      Add ‘Decombobulate’ to the import list"
  "      in the import of ‘Foobar’"
  "      (at /foo/bar/baz/Quux.hs:2:1-28).")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Foobar (Bar(..), Baz)"
  ""
  "test :: _|_Decombobulate a => FooType a -> IO ()"
  "test (Foo x) = undefined"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Foobar (Bar(..), Baz, Decombobulate)"
  ""
  "test :: _|_Decombobulate a => FooType a -> IO ()"
  "test (Foo x) = undefined"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-extension-1a
 :error-message
 (tests-utils--multiline
  "error: [GHC-17779]"
  "    • Illegal role annotation for Foo"
  "    • while checking a role annotation for ‘Foo’"
  "    Suggested fix:"
  "      Perhaps you intended to use RoleAnnotations"
  "      You may enable this language extension in GHCi with:"
  "        :set -XRoleAnnotations")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  "#!/usr/bin/env -S cabal run"
  "{- cabal:"
  "build-depends:"
  "  , base"
  "-}"
  "{- project:"
  "allow-newer:"
  "  , *:base"
  "-}"
  ""
  "module Test where"
  ""
  "import GHC.TypeNats"
  ""
  "newtype Foo n = Foo Int"
  "_|_type role Foo nominal"
  "")
 :expected-value
 (tests-utils--multiline
  "#!/usr/bin/env -S cabal run"
  "{- cabal:"
  "build-depends:"
  "  , base"
  "-}"
  "{- project:"
  "allow-newer:"
  "  , *:base"
  "-}"
  ""
  "{-# LANGUAGE RoleAnnotations #-}"
  ""
  "module Test where"
  ""
  "import GHC.TypeNats"
  ""
  "newtype Foo n = Foo Int"
  "_|_type role Foo nominal"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-extension-1b
 :error-message
 (tests-utils--multiline
  "error: [GHC-17779]"
  "    • Illegal role annotation for Foo"
  "    • while checking a role annotation for ‘Foo’"
  "    Suggested fix:"
  "      Perhaps you intended to use RoleAnnotations"
  "      You may enable this language extension in GHCi with:"
  "        :set -XRoleAnnotations")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  "#!/usr/bin/env -S cabal run"
  ""
  " {-project :"
  "allow-newer:"
  "  , *:base"
  "-}"
  ""
  " {-cabal :"
  "build-depends:"
  "  , base"
  "-}"
  ""
  "module Test where"
  ""
  "import GHC.TypeNats"
  ""
  "newtype Foo n = Foo Int"
  "_|_type role Foo nominal"
  "")
 :expected-value
 (tests-utils--multiline
  "#!/usr/bin/env -S cabal run"
  ""
  " {-project :"
  "allow-newer:"
  "  , *:base"
  "-}"
  ""
  " {-cabal :"
  "build-depends:"
  "  , base"
  "-}"
  ""
  "{-# LANGUAGE RoleAnnotations #-}"
  ""
  "module Test where"
  ""
  "import GHC.TypeNats"
  ""
  "newtype Foo n = Foo Int"
  "_|_type role Foo nominal"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/explicit-type-wildcard-1
 :error-message
 (tests-utils--multiline
  "error: [GHC-88464]"
  "    • Found type wildcard ‘_’ standing for ‘Frobnicator’"
  "      To use the inferred type, enable PartialTypeSignatures"
  "    • In the type signature: tests :: _")
 :action
 (let ((attrap-select-predefined-option "explicit type wildcard"))
   (attrap-tests--run-attrap))
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|__"
  "foo = bar"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_Frobnicator"
  "foo = bar"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/explicit-type-wildcard-2
 :error-message
 (tests-utils--multiline
  "error: [GHC-88464]"
  "    • Found type wildcard ‘_’ standing for ‘Frobnicator’"
  "      To use the inferred type, enable PartialTypeSignatures"
  "    • In the type signature: tests :: _")
 :action
 (let ((attrap-select-predefined-option "explicit type wildcard"))
   (attrap-tests--run-attrap))
 :contents
 (tests-utils--multiline
  ""
  "foo :: Maybe _|__"
  "foo = bar"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Maybe _|_Frobnicator"
  "foo = bar"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/explicit-type-wildcard-3
 :error-message
 (tests-utils--multiline
  "error: [GHC-88464]"
  "    • Found type wildcard ‘_’ standing for ‘Frobnicator Int’"
  "      To use the inferred type, enable PartialTypeSignatures"
  "    • In the type signature: tests :: _")
 :action
 (let ((attrap-select-predefined-option "explicit type wildcard"))
   (attrap-tests--run-attrap))
 :contents
 (tests-utils--multiline
  ""
  "foo :: Maybe _|__"
  "foo = bar"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Maybe _|_(Frobnicator Int)"
  "foo = bar"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/explicit-type-wildcard-4
 :error-message
 (tests-utils--multiline
  "error: [GHC-88464]"
  "    • Found type wildcard ‘_’ standing for ‘Frobnicator Int’"
  "      To use the inferred type, enable PartialTypeSignatures"
  "    • In the type signature: tests :: _")
 :action
 (let ((attrap-select-predefined-option "explicit type wildcard"))
   (attrap-tests--run-attrap))
 :contents
 (tests-utils--multiline
  ""
  "foo :: [_|__]"
  "foo = bar"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: [_|_(Frobnicator Int)]"
  "foo = bar"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-missing-class-methods-1a
 :error-message
 (tests-utils--multiline
  "warning: [GHC-06201] [-Wmissing-methods]"
  "    • No explicit implementation for"
  "        ‘basicLength’, ‘basicUnsafeSlice’, ‘basicOverlaps’, ‘basicUnsafeNew’, ‘basicInitialize’, ‘basicUnsafeRead’, and ‘basicUnsafeWrite’"
  "    • In the instance declaration for ‘MVector MVector ActiveFinalMatchesEmptyEntry’")
 :action
 (let ((attrap-select-predefined-option "explicit type wildcard"))
   (attrap-tests--run-attrap))
 :contents
 (tests-utils--multiline
  ""
  "instance _|_GM.MVector U.MVector ActiveFinalMatchesEmptyEntry"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "instance _|_GM.MVector U.MVector ActiveFinalMatchesEmptyEntry where"
  "  basicLength = _"
  "  basicUnsafeSlice = _"
  "  basicOverlaps = _"
  "  basicUnsafeNew = _"
  "  basicInitialize = _"
  "  basicUnsafeRead = _"
  "  basicUnsafeWrite = _"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-missing-class-methods-1b
 :error-message
 (tests-utils--multiline
  "warning: [GHC-06201] [-Wmissing-methods]"
  "    • No explicit implementation for"
  "        ‘basicLength’, ‘basicUnsafeSlice’, ‘basicOverlaps’, ‘basicUnsafeNew’, ‘basicInitialize’, ‘basicUnsafeRead’, and ‘basicUnsafeWrite’"
  "    • In the instance declaration for ‘MVector MVector ActiveFinalMatchesEmptyEntry’")
 :action
 (let ((attrap-select-predefined-option "explicit type wildcard"))
   (attrap-tests--run-attrap))
 :contents
 (tests-utils--multiline
  ""
  "instance _|_GM.MVector U.MVector ActiveFinalMatchesEmptyEntry where"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "instance _|_GM.MVector U.MVector ActiveFinalMatchesEmptyEntry where"
  "  basicLength = _"
  "  basicUnsafeSlice = _"
  "  basicOverlaps = _"
  "  basicUnsafeNew = _"
  "  basicInitialize = _"
  "  basicUnsafeRead = _"
  "  basicUnsafeWrite = _"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-missing-class-methods-1c
 :error-message
 (tests-utils--multiline
  "warning: [GHC-06201] [-Wmissing-methods]"
  "    • No explicit implementation for"
  "        ‘basicLength’, ‘basicUnsafeSlice’, ‘basicOverlaps’, ‘basicUnsafeNew’, ‘basicInitialize’, ‘basicUnsafeRead’, and ‘basicUnsafeWrite’"
  "    • In the instance declaration for ‘MVector MVector ActiveFinalMatchesEmptyEntry’")
 :action
 (let ((attrap-select-predefined-option "explicit type wildcard"))
   (attrap-tests--run-attrap))
 :contents
 (tests-utils--multiline
  ""
  "instance _|_GM.MVector U.MVector ActiveFinalMatchesEmptyEntry   "
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "instance _|_GM.MVector U.MVector ActiveFinalMatchesEmptyEntry where"
  "  basicLength = _"
  "  basicUnsafeSlice = _"
  "  basicOverlaps = _"
  "  basicUnsafeNew = _"
  "  basicInitialize = _"
  "  basicUnsafeRead = _"
  "  basicUnsafeWrite = _"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/add-missing-class-methods-1d
 :error-message
 (tests-utils--multiline
  "warning: [GHC-06201] [-Wmissing-methods]"
  "    • No explicit implementation for"
  "        ‘basicLength’, ‘basicUnsafeSlice’, ‘basicOverlaps’, ‘basicUnsafeNew’, ‘basicInitialize’, ‘basicUnsafeRead’, and ‘basicUnsafeWrite’"
  "    • In the instance declaration for ‘MVector MVector ActiveFinalMatchesEmptyEntry’")
 :action
 (let ((attrap-select-predefined-option "explicit type wildcard"))
   (attrap-tests--run-attrap))
 :contents
 (tests-utils--multiline
  ""
  "instance _|_GM.MVector U.MVector ActiveFinalMatchesEmptyEntry   where     "
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "instance _|_GM.MVector U.MVector ActiveFinalMatchesEmptyEntry   where"
  "  basicLength = _"
  "  basicUnsafeSlice = _"
  "  basicOverlaps = _"
  "  basicUnsafeNew = _"
  "  basicInitialize = _"
  "  basicUnsafeRead = _"
  "  basicUnsafeWrite = _"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/file-name-does-not-match-module-name-1
 :error-message
 (tests-utils--multiline
  "error: [GHC-28623]"
  "    File name does not match module name:"
  "    Saw     : ‘Data.Regex.FFI.Rure’"
  "    Expected: ‘Data.Regex.Rure.FFI’"
  "  |")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "module _|_Data.Regex.FFI.Rure"
  "  ( Match(..)"
  "  ) where")
 :expected-value
 (tests-utils--multiline
  ""
  "module _|_Data.Regex.Rure.FFI"
  "  ( Match(..)"
  "  ) where"))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-1a
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraint: HasCallStack"
  "    In the type signature for:"
  "         foo :: HasCallStack => Int -> Int")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|_HasCallStack => Int -> Int"
  "foo x = x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_Int -> Int"
  "foo x = x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-1b
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraint: HasCallStack"
  "    In the type signature for:"
  "         foo :: HasCallStack => Int -> Int")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|_(HasCallStack) => Int -> Int"
  "foo x = x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_Int -> Int"
  "foo x = x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-1c
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraint: HasCallStack"
  "    In the type signature for:"
  "         foo :: (HasCallStack, Monad m) => Int -> m Int")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|_(HasCallStack, Monad m) => Int -> m Int"
  "foo x = return x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_Monad m => Int -> m Int"
  "foo x = return x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-1d
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraint: HasCallStack"
  "    In the type signature for:"
  "         foo :: (Monad m, HasCallStack) => Int -> m Int")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|_(Monad m, HasCallStack) => Int -> m Int"
  "foo x = return x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_Monad m => Int -> m Int"
  "foo x = return x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-1e
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraint: HasCallStack"
  "    In the type signature for:"
  "         foo :: (Monad m, HasCallStack, Num a) => a -> m a")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|_(Monad m, HasCallStack, Num a) => a -> m a"
  "foo x = return (x + x)"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_(Monad m, Num a) => a -> m a"
  "foo x = return (x + x)"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-1f
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraint: HasCallStack"
  "    In the type signature for:"
  "         foo :: (Monad m, HasCallStack) => Int -> m Int")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|_Monad m => HasCallStack => Int -> m Int"
  "foo x = return x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_Monad m => Int -> m Int"
  "foo x = return x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-1g
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraint: (HasCallStack, Num a)"
  "    In the type signature for:"
  "         foo :: (Monad m, HasCallStack, Num a) => Int -> m Int")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|_Monad m => (HasCallStack, Num a) => Int -> m Int"
  "foo x = return x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_Monad m => Int -> m Int"
  "foo x = return x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-1h
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraint: (HasCallStack, Num a)"
  "    In the type signature for:"
  "         foo :: (Monad m, HasCallStack, Num a) => Int -> m Int")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|_(Monad m, Num a) => HasCallStack => Int -> m Int"
  "foo x = return x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_Monad m => Int -> m Int"
  "foo x = return x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-1i
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraint: (HasCallStack, Num a)"
  "    In the type signature for:"
  "         foo :: (Monad m, HasCallStack, Num a) => m Int")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|_(Monad m, Num a) => HasCallStack => m Int"
  "foo x = return x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_Monad m => m Int"
  "foo x = return x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-1j
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraint: (Monad m, Num a)"
  "    In the type signature for:"
  "         foo :: (Monad m, HasCallStack, Num a) => Int")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|_(Monad m, Num a) => HasCallStack => Int"
  "foo x = return x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_HasCallStack => Int"
  "foo x = return x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-2
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraints: (HasCallStack, Num a)"
  "    In the type signature for:"
  "         foo :: forall (m :: * -> *) a."
  "                (Monad m, HasCallStack, Num a) =>"
  "                a -> m a")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|_(Monad m, HasCallStack, Num a) => a -> m a"
  "foo x = return x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_Monad m => a -> m a"
  "foo x = return x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-3
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraints: (HasCallStack, Foo a Int)"
  "    In the type signature for:"
  "         foo :: forall (m :: * -> *) a."
  "                (Monad m, HasCallStack, Foo a Int) =>"
  "                a -> m a")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|_(Monad m, HasCallStack, a `Foo` Int) => a -> m a"
  "foo x = return x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_Monad m => a -> m a"
  "foo x = return x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-4a
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraints: (HasCallStack, Foo a Int,"
  "                            Bar a (Double, Double) Double)"
  "    In the type signature for:"
  "         foo :: forall (m :: * -> *) a."
  "                (Monad m, HasCallStack, Foo a Int,"
  "                 Bar a (Double, Double) Double) =>"
  "                a -> m a")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|_(Monad m, HasCallStack, a `Foo` Int, Bar a (Double, Double) Double) => a -> m a"
  "foo x = return x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_Monad m => a -> m a"
  "foo x = return x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-4b
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraints: (HasCallStack, Foo a Int,"
  "                            Bar a (Double, Double) Double)"
  "    In the type signature for:"
  "         foo :: forall (m :: * -> *) a."
  "                (Monad m, HasCallStack, Foo a Int,"
  "                 Bar a (Double, Double) Double) =>"
  "                a -> m a")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|_(HasCallStack, a `Foo` Int, Bar a (Double, Double) Double, Monad m) => a -> m a"
  "foo x = return x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_Monad m => a -> m a"
  "foo x = return x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-4c
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraints: (HasCallStack, Foo a Int,"
  "                            Bar a (Double, Double) Double)"
  "    In the type signature for:"
  "         foo :: forall (m :: * -> *) a."
  "                (Monad m, HasCallStack, Foo a Int,"
  "                 Bar a (Double, Double) Double) =>"
  "                a -> m a")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo :: _|_(HasCallStack, a `Foo` Int, Monad m, Bar a (Double, Double) Double) => a -> m a"
  "foo x = return x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: _|_Monad m => a -> m a"
  "foo x = return x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-4d
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraints: (Foo a Int,"
  "                            Bar a (Double, Double) Double)"
  "    In the type signature for:"
  "         foo :: forall (m :: * -> *) a."
  "                (Monad m, HasCallStack, Foo a Int,"
  "                 Bar a (Double, Double) Double) =>"
  "                a -> m a")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo"
  "  :: _|_( Monad m"
  "     , HasCallStack"
  "     , a `Foo` Int"
  "     , Bar a (Double, Double) Double"
  "     )"
  "  => a"
  "  -> m a"
  "foo x = return x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo"
  "  :: _|_( Monad m"
  "     , HasCallStack"
  "     )"
  "  => a"
  "  -> m a"
  "foo x = return x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/redundant-constraint-4e
 :modes (haskell-ts-mode)
 :error-message
 (tests-utils--multiline
  "warning: [GHC-30606] [-Wredundant-constraints]"
  "    Redundant constraints: (HasCallStack, Foo a Int,"
  "                            Bar a (Double, Double) Double)"
  "    In the type signature for:"
  "         foo :: forall (m :: * -> *) a."
  "                (Monad m, HasCallStack, Foo a Int,"
  "                 Bar a (Double, Double) Double) =>"
  "                a -> m a")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "foo"
  "  :: _|_( Monad m"
  "     , HasCallStack"
  "     , a `Foo` Int"
  "     , Bar a (Double, Double) Double"
  "     )"
  "  => a"
  "  -> m a"
  "foo x = return x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo"
  "  :: _|_Monad m"
  "  => a"
  "  -> m a"
  "foo x = return x"
  ""))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/unticked-promoted-constructor-1
 :error-message
 (tests-utils--multiline
  "warning: [GHC-49957] [-Wunticked-promoted-constructors]"
  "    Unticked promoted constructor: Bar."
  "    Suggested fix: Use 'Bar instead of Bar.")
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "data Foo = Bar"
  ""
  "data Quux (ix :: Foo) where"
  "  Quux :: Quux _|_Bar"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "data Foo = Bar"
  ""
  "data Quux (ix :: Foo) where"
  "  Quux :: Quux _|_'Bar"
  ""))

(provide 'attrap-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; attrap-tests.el ends here
