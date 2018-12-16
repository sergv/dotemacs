----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.AnalyzeHeaderTests
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Friday, 23 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.Server.Tags.AnalyzeHeaderTests (tests) where

import Control.Arrow
import Control.Monad.ErrorExcept
import Control.Monad.Except.Ext
import Control.Monad.Writer

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext
import Test.Tasty
import Test.Tasty.HUnit

import Haskell.Language.Lexer (tokenize)
import Haskell.Language.Lexer.FastTags (Pos, ServerToken, Line(..), Type(..))

import Control.Monad.Logging.Simple
import qualified Data.KeyMap as KM
import Data.Path
import qualified Data.SubkeyMap as SubkeyMap
import Data.Symbols
import Haskell.Language.Server.Tags.AnalyzeHeader
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

import TestUtils
import Haskell.Language.Server.Tags.AnalyzeHeaderTests.Regressions

type Test = TestCase T.Text ModuleHeader

filename :: FullPath 'File
filename = "/foo/bar/test.hs"

pt :: Int -> Type -> PosAndType
pt n = PosAndType filename (Line n)

simpleHeaderTest :: Test
simpleHeaderTest = TestCase
  { testName       = "Simple header"
  , input          =
      "module Foo where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Foo"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithUnqualifiedImportTest :: Test
moduleWithUnqualifiedImportTest = TestCase
  { testName       = "Unqualified import"
  , input          =
      "module ModuleWithUnqualifiedImport where\n\
      \import Imported1"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithUnqualifiedImport"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithUnqualifiedSourceImportTest :: Test
moduleWithUnqualifiedSourceImportTest = TestCase
  { testName       = "Unqualified import with {-# SOURCE #-}"
  , input          =
      "module ModuleWithUnqualifiedImport where\n\
      \import {-# SOURCE #-} Imported1"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithUnqualifiedImport"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = HsBootModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithUnqualifiedSafeImportTest :: Test
moduleWithUnqualifiedSafeImportTest = TestCase
  { testName       = "Unqualified safe import"
  , input          =
      "module ModuleWithUnqualifiedImport where\n\
      \import safe Imported1"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithUnqualifiedImport"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithPatternImportTest :: Test
moduleWithPatternImportTest = TestCase
  { testName       = "Pattern import"
  , input          =
      "module ModuleWithPatternImport where\n\
      \import Imported1 (pattern Pat)"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithPatternImport"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Pat"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndEmptyImportListTest :: Test
moduleWithUnqualifiedImportAndEmptyImportListTest = TestCase
  { testName       = "Unqualified import and empty import list"
  , input          =
      "module Test where\n\
      \import Imported1 ()"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = mempty
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndEmptyHiddenImportListTest :: Test
moduleWithUnqualifiedImportAndEmptyHiddenImportListTest = TestCase
  { testName       = "Unqualified import and empty hidden import list"
  , input          =
      "module Test where\n\
      \import Imported1 hiding ()"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = mempty
                  , ilImportType = Hidden
                  }
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndSingletonImportListTest :: Test
moduleWithUnqualifiedImportAndSingletonImportListTest = TestCase
  { testName       = "Unqualified import and singleton import list"
  , input          =
      "module Test where\n\
      \import Imported1 (foo)"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "foo"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndNonemptyImportListTest :: Test
moduleWithUnqualifiedImportAndNonemptyImportListTest = TestCase
  { testName       = "Unqualified import and nonempty import list"
  , input          =
      "module Test where\n\
      \import Imported1 (foo, bar, baz)"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "foo"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "bar"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "baz"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndNonemptyImportListWithDifferentVisibilitiesTest :: Test
moduleWithUnqualifiedImportAndNonemptyImportListWithDifferentVisibilitiesTest = TestCase
  { testName       =
      "Unqualified import and nonempty import list with different visibilities"
  , input          =
      "module Test where\n\
      \import Imported1 (foo, Bar(..), Baz(Quux, Fizz), type Typ, type (++), pattern Pat, pattern (:++), (:$:), (:$$:)(..), (:$$*:)((:$$$*:), (:$$$**:)))"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "foo"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Baz"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName "Quux"
                              , mkUnqualSymName "Fizz"
                              ]
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Typ"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "++"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Pat"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":++"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$:"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$$:"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$$*:"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName ":$$$*:"
                              , mkUnqualSymName ":$$$**:"
                              ]
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithQualifiedImportTest :: Test
moduleWithQualifiedImportTest = TestCase
  { testName       = "Qualified import"
  , input          =
      "module ModuleWithQualifiedImport where\n\
      \import qualified Imported1"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithQualifiedImport"
      , mhExports          = NoExports
      , mhImportQualifiers = M.fromList
          [ ( mkImportQualifier $ mkModuleName "Imported1"
            , neSingleton $ mkModuleName "Imported1"
            )
          ]
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification =
                  Qualified $ mkImportQualifier $ mkModuleName "Imported1"
              , ispecImportList    = NoImportList
              }
          ]
      }
  }


moduleWithQualifiedSafeAndPackageImportTest :: Test
moduleWithQualifiedSafeAndPackageImportTest = TestCase
  { testName       = "Qualified safe import with package import"
  , input          =
      "module ModuleWithQualifiedImport where\n\
      \import safe qualified \"foobar\" Imported1"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithQualifiedImport"
      , mhExports          = NoExports
      , mhImportQualifiers = M.fromList
          [ ( mkImportQualifier $ mkModuleName "Imported1"
            , neSingleton $ mkModuleName "Imported1"
            )
          ]
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification =
                  Qualified $ mkImportQualifier $ mkModuleName "Imported1"
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithQualifiedImportAndAliasTest :: Test
moduleWithQualifiedImportAndAliasTest = TestCase
  { testName       = "Qualified import and alias"
  , input          =
      "module ModuleWithQualifiedImportAndAlias where\n\
      \import qualified Imported1 as Imp"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithQualifiedImportAndAlias"
      , mhExports          = NoExports
      , mhImportQualifiers = M.fromList
          [ ( mkImportQualifier $ mkModuleName "Imp"
            , neSingleton $ mkModuleName "Imported1"
            )
          ]
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification =
                  Qualified $ mkImportQualifier $ mkModuleName "Imp"
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithImportAndAliasTest :: Test
moduleWithImportAndAliasTest = TestCase
  { testName       = "Import and alias"
  , input          =
      "module ModuleWithImportAndAlias where\n\
      \import Imported1 as Imp"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithImportAndAlias"
      , mhExports          = NoExports
      , mhImportQualifiers = M.fromList
          [ ( mkImportQualifier $ mkModuleName "Imp"
            , neSingleton $ mkModuleName "Imported1"
            )
          ]
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification =
                  BothQualifiedAndUnqualified $ mkImportQualifier $ mkModuleName "Imp"
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithImportAndAliasAndHidingImportListTest :: Test
moduleWithImportAndAliasAndHidingImportListTest = TestCase
  { testName       = "Import, alias and hiding import list"
  , input          =
      "module ModuleWithImportAndAliasAandHidingImportList where\n\
      \import Imported1 as Imp hiding (Foo(..), bar, Quux(Baz))"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithImportAndAliasAandHidingImportList"
      , mhExports          = NoExports
      , mhImportQualifiers = M.fromList
          [ ( mkImportQualifier $ mkModuleName "Imp"
            , neSingleton $ mkModuleName "Imported1"
            )
          ]
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification =
                  BothQualifiedAndUnqualified $ mkImportQualifier $ mkModuleName "Imp"
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "bar"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Quux"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName "Baz"
                              ]
                          }
                      ]
                  , ilImportType = Hidden
                  }
              }
          ]
      }
  }

-- Operators with these names have standalone tokens for them after lexing,
-- so it's important to account for them during header recognition.
moduleWithImportOfSpeciallyNamedOperatorsTest :: Test
moduleWithImportOfSpeciallyNamedOperatorsTest = TestCase
  { testName       = "import of operators with special names"
  , input          =
      "module ModuleWithImportOfSpeciallyNamedOperators where\n\
      \import Imported1 ((.), (!), (~), (.+.))"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithImportOfSpeciallyNamedOperators"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "."
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "!"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "~"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ".+."
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithMultipleImports :: Test
moduleWithMultipleImports = TestCase
  { testName       = "Module multiple imports"
  , input          =
      "module Test where \n\
      \import Mod1\n\
      \import Mod2 as Foo\n\
      \"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = M.fromList
        [ (mkImportQualifier $ mkModuleName "Foo", neSingleton $ mkModuleName "Mod2")
        ]
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          , neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod2"
                  }
              , ispecQualification =
                BothQualifiedAndUnqualified $ mkImportQualifier $ mkModuleName "Foo"
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

mkModuleWithImportsAfterDefinitionTest :: String -> T.Text -> Test
mkModuleWithImportsAfterDefinitionTest testName thing = TestCase
  { testName
  , input          =
      "module Test where \n\
      \import Quux.Mod1\n\
      \\n\
      \" <> thing <> " :: a -> a\n\
      \" <> thing <> " x = x\n\
      \\n\
      \import Mod2 as Foo\n\
      \"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = M.fromList
        [ (mkImportQualifier $ mkModuleName "Foo", neSingleton $ mkModuleName "Mod2")
        ]
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Quux.Mod1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          , neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod2"
                  }
              , ispecQualification =
                BothQualifiedAndUnqualified $ mkImportQualifier $ mkModuleName "Foo"
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithImportsAfterFunctionDefinition :: Test
moduleWithImportsAfterFunctionDefinition =
  mkModuleWithImportsAfterDefinitionTest
    "Module imports after function definition"
    "foo"

moduleWithImportsAfterOperatorDefinition :: Test
moduleWithImportsAfterOperatorDefinition =
  mkModuleWithImportsAfterDefinitionTest
    "Module imports after operator definition"
    "(+++)"

moduleWithImportsAfterExclamationMarkOperatorDefinition :: Test
moduleWithImportsAfterExclamationMarkOperatorDefinition =
  mkModuleWithImportsAfterDefinitionTest
    "Module imports after (!) operator definition"
    "(!)"

moduleWithImportsAfterDotOperatorDefinition :: Test
moduleWithImportsAfterDotOperatorDefinition =
  mkModuleWithImportsAfterDefinitionTest
    "Module imports after (.) operator definition"
    "(.)"

-- No reasonable way to make work this work. Just check that we don't crash
-- and attempt something remotely sensible.
moduleWithParensInImportList1 :: Test
moduleWithParensInImportList1 = TestCase
  { testName       = "Import of \"pattern\" function 1"
  , input          =
      "module Test where\n\
      \import Foo\n\
      \#ifdef FOO\n\
      \  ( foo\n\
      \#else\n\
      \  ( bar\n\
      \#endif\n\
      \  )\n\
      \import Bar\n\
      \"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Foo"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "foo"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName "bar"
                              ]
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          , neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Bar"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

-- No reasonable way to make work this work. Just check that we don't crash
-- and attempt something remotely sensible.
moduleWithParensInImportList2 :: Test
moduleWithParensInImportList2 = TestCase
  { testName       = "Import of \"pattern\" function 2"
  , input          =
      "module Test where\n\
      \import Foo\n\
      \#ifdef FOO\n\
      \  ( foo\n\
      \  , bar\n\
      \#else\n\
      \  ( baz\n\
      \  , quux\n\
      \#endif\n\
      \  , fizz\n\
      \  )\n\
      \import Bar\n\
      \"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Foo"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "foo"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "bar"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName name
                              | name <- ["baz", "quux", "fizz"]
                              ]
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          , neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Bar"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithMultilinePreprocessor :: Test
moduleWithMultilinePreprocessor = TestCase
  { testName       = "Multiline preprocessor"
  , input          =
      "module Test where\n\
      \import Foo\n\
      \  ( foo\n\
      \#ifdef FOO \\\n\
      \  && !BAR\n\
      \  , bar\n\
      \#else\n\
      \  , baz\n\
      \#endif\n\
      \  , quux\n\
      \  )\n\
      \import Bar\n\
      \"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Foo"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName name
                          , entryChildrenVisibility = Nothing
                          }
                      | name <- ["foo", "bar", "baz", "quux"]
                      ]
                  , ilImportType = Imported
                  }
              }
          , neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Bar"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithImportOfPatternFuncTest :: Test
moduleWithImportOfPatternFuncTest = TestCase
  { testName       = "Import of \"pattern\" function"
  , input          =
      "module Test where\n\
      \import Imported1 (pattern)"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "pattern"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithImportOfManyFuncsAndPatternFuncTest :: Test
moduleWithImportOfManyFuncsAndPatternFuncTest = TestCase
  { testName       = "Import of several functions, including \"pattern\" function"
  , input          =
      "module Test where\n\
      \import Imported1 (Foo(..), pattern, (++), Bar)"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "pattern"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "++"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithoutCommasAndPatternFuncImportBeforeOperator :: Test
moduleWithoutCommasAndPatternFuncImportBeforeOperator = TestCase
  { testName       = "Module without commas in import list and import of pattern function before operator function"
  , input          =
      "module Test where\n\
      \import Imported1 (Foo(..) pattern (++) Bar)"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "pattern"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "++"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithoutCommasAndPatternFuncImportBeforeConstructorWithChildren :: Test
moduleWithoutCommasAndPatternFuncImportBeforeConstructorWithChildren = TestCase
  { testName       = "Module without commas in import list and import of pattern function before constructor with children"
  , input          =
      "module Test where\n\
      \import Imported1 (pattern Foo(..) (++) Bar)"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "pattern"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "++"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithoutCommasAndPatternFuncImportBeforeOperatorConstructorWithChildren :: Test
moduleWithoutCommasAndPatternFuncImportBeforeOperatorConstructorWithChildren = TestCase
  { testName       = "Module without commas in import list and import of pattern function before operator constructor with children"
  , input          =
      "module Test where\n\
      \import Imported1 (pattern (:++)(..) (++) Bar)"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName ":++"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "pattern"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "++"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithoutCommasAndSeveralPatternImports :: Test
moduleWithoutCommasAndSeveralPatternImports = TestCase
  { testName       = "Module without commas in import list"
  , input          =
      "module Test where\n\
      \import Imported1 (Foo(..) pattern (:++) Bar pattern Baz)"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":++"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Baz"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithAmbigousImportList :: Test
moduleWithAmbigousImportList = TestCase
  { testName       = "Ambigous import list"
  , input          =
      "module Test where\n\
      \import Imported1 (Foo (:$$:)(..) Bar (:$$$:)(X) Baz (:?:) (:+:)((:++:)))"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$$:"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$$$:"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName "X"
                              ]
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Baz"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":?:"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":+:"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName ":++:"
                              ]
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithImportListWithoutCommas :: Test
moduleWithImportListWithoutCommas = TestCase
  { testName       = "Import list without commas"
  , input          =
      "module Test where\n\
      \import Imported1 (foo Bar(..) Baz(Quux, Fizz) (:$:) (:$$:)(..) (:$$*:)((:$$$*:), (:$$$**:)) pattern Pat pattern (:++))"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "foo"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Baz"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName "Quux"
                              , mkUnqualSymName "Fizz"
                              ]
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$:"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$$:"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$$*:"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName ":$$$*:"
                              , mkUnqualSymName ":$$$**:"
                              ]
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Pat"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":++"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithImportsThatHaveChildrenListWithoutCommas :: Test
moduleWithImportsThatHaveChildrenListWithoutCommas = TestCase
  { testName       = "Import list where children list has no commas"
  , input          =
      "module Test where\n\
      \import Imported1 (Baz(Quux, Fizz), (:$$*:)((:$$$*:) (:$$$**:)))"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Baz"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName "Quux"
                              , mkUnqualSymName "Fizz"
                              ]
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$$*:"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName ":$$$*:"
                              , mkUnqualSymName ":$$$**:"
                              ]
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithUnbalancedParensInImportList :: Test
moduleWithUnbalancedParensInImportList = TestCase
  { testName       = "Module with unbalanced parens in import list"
  , input          =
      "module Test where \n\
      \import Mod\n\
      \#if FOO\n\
      \  ( Foo(X, Y)\n\
      \#else\n\
      \  ( Foo\n\
      \#endif\n\
      \  , Bar\n\
      \  )"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName "X"
                              , mkUnqualSymName "Y"
                              ]
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithUnbalancedParensInImportChildrenList :: Test
moduleWithUnbalancedParensInImportChildrenList = TestCase
  { testName       = "Module with unbalanced parens in import children list"
  , input          =
      "module Test where \n\
      \import Mod\n\
      \  ( Foo\n\
      \#if FOO\n\
      \    ( X\n\
      \#else\n\
      \    ( Z\n\
      \#endif\n\
      \    , Y\n\
      \    )\n\
      \  , Bar\n\
      \  )"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName "X"
                              , mkUnqualSymName "Y"
                              , mkUnqualSymName "Z"
                              ]
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithSingleHSC2HSDirectiveInImportList :: Test
moduleWithSingleHSC2HSDirectiveInImportList = TestCase
  { testName       = "Module with single hsc2hs directive in import list"
  , input          =
      "module Test where \n\
      \import Mod\n\
      \  ( #{type int64_t} \n\
      \  )"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = AssumedWildcardImportList
              }
          ]
      }
  }

moduleWithSomeHSC2HSDirectivesInImportList1 :: Test
moduleWithSomeHSC2HSDirectivesInImportList1 = TestCase
  { testName       = "Module with some hsc2hs directives in import list #1"
  , input          =
      "module Test where \n\
      \import Mod\n\
      \  ( Foo\n\
      \  , #{type int64_t} \n\
      \  , Bar \n\
      \  , #{type baz_t} \n\
      \  )"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = AssumedWildcardImportList
              }
          ]
      }
  }

moduleWithSomeHSC2HSDirectivesInImportList2 :: Test
moduleWithSomeHSC2HSDirectivesInImportList2 = TestCase
  { testName       = "Module with some hsc2hs directives in import list #2"
  , input          =
      "module Test where \n\
      \import Mod\n\
      \  ( Foo\n\
      \  , #{type int64_t} \n\
      \  , #{type baz_t} \n\
      \  , Bar \n\
      \  )"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = AssumedWildcardImportList
              }
          ]
      }
  }

moduleWithSomeHSC2HSDirectivesInImportList3 :: Test
moduleWithSomeHSC2HSDirectivesInImportList3 = TestCase
  { testName       = "Module with some hsc2hs directives in import list #3"
  , input          =
      "module Test where \n\
      \import Mod\n\
      \  ( pattern Foo\n\
      \  , #{type int64_t} \n\
      \  , pattern Bar \n\
      \  , Frob(..) \n\
      \  , #{type baz_t} \n\
      \  )"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = AssumedWildcardImportList
              }
          ]
      }
  }


moduleWithEmptyExportsTest :: Test
moduleWithEmptyExportsTest = TestCase
  { testName       = "Empty exports"
  , input          =
      "module ModuleWithEmptyExport () where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithEmptyExport"
      , mhExports          = EmptyExports
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithQuaifiedExportsTest :: Test
moduleWithQuaifiedExportsTest = TestCase
  { testName       = "Qualified exports"
  , input          =
      "module ModuleWithEmptyExport (Foo.bar, Baz.Quux(..)) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithEmptyExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo.bar", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz.Quux", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportsTest :: Test
moduleWithExportsTest = TestCase
  { testName       = "Module exports"
  , input          =
      "module ModuleWithExport\
      \ (foo, Bar(..), Baz(Quux, Fizz, wat, (??)),\
      \ Frob(.., Frob', Frob''), pattern Pat, pattern (:!:),\
      \ module Frob, type Typ, type (++),\
      \ (:$:), (:$$:)(..), (:$$*:)((:$$$*:), (:$$$**:))) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "foo", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz", pt 1 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "Quux", pt 1 Constructor)
                      , (mkUnqualSymName "Fizz", pt 1 Constructor)
                      , (mkUnqualSymName "wat",  pt 1 Function)
                      , (mkUnqualSymName "??",   pt 1 Operator)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Frob", pt 1 Type)
                  , entryChildrenVisibility = Just $ VisibleAllChildrenPlusSome $ M.fromList
                      [ (mkUnqualSymName "Frob'", pt 1 Constructor)
                      , (mkUnqualSymName "Frob''", pt 1 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Pat", pt 1 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":!:", pt 1 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Typ", pt 1 Family)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "++", pt 1 Family)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$:", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$:", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$*:", pt 1 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName ":$$$*:",  pt 1 Constructor)
                      , (mkUnqualSymName ":$$$**:", pt 1 Constructor)
                      ]
                  }
              ]
          , meReexports          = S.singleton $ mkModuleName "Frob"
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithMultilineExportsTest :: Test
moduleWithMultilineExportsTest = TestCase
  { testName       = "Peculiarly indented export list"
  , input          =
      "module ModuleWithExport\n\
      \ (\n\
      \   foo\n\
      \ , \n\
      \  Bar\n\
      \    (..)\n\
      \ ,        Baz(\n\
      \  Quux\n\
      \    ,\n\
      \   Fizz \n\
      \  )\n\
      \    ,\n\
      \\n\
      \                   Frob         \n\
      \                     (       \n\
      \                ..  \n\
      \             ,                \n\
      \       Frob'      \n\
      \             ,                  \n\
      \        Frob''            \n\
      \    )                                       \n\
      \              , \n\
      \\n\
      \  pattern\n\
      \     Pat\n\
      \\n\
      \             pattern      \n\
      \        (:!:)   \n\
      \  , \n\
      \    module \n\
      \  Frob\n\
      \     ,     \n\
      \         (      :$:     )             , \n\
      \      (   \n\
      \   :$$:      \n\
      \    )      (  ..    ) \n\
      \      ,       (  \n\
      \       :$$*:   )   (  \n\
      \         (:$$$*:)  \n\
      \ , (:$$$**:)   )        \n\
      \   ) \n\
      \   where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "foo", pt 3 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 5 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz", pt 7 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "Quux", pt 8 Constructor)
                      , (mkUnqualSymName "Fizz", pt 10 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Frob", pt 14 Type)
                  , entryChildrenVisibility = Just $ VisibleAllChildrenPlusSome $ M.fromList
                      [ (mkUnqualSymName "Frob'",   pt 18 Constructor)
                      , (mkUnqualSymName "Frob''",  pt 20 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Pat", pt 25 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":!:", pt 28 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$:", pt 33 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$:", pt 35 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$*:", pt 38 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName ":$$$*:",  pt 39 Constructor)
                      , (mkUnqualSymName ":$$$**:", pt 40 Constructor)
                      ]
                  }
              ]
          , meReexports          = S.singleton $ mkModuleName "Frob"
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.empty
      }
  }

moduleWithExportsOfSpeciallyNamedOperatorsTest :: Test
moduleWithExportsOfSpeciallyNamedOperatorsTest = TestCase
  { testName       = "Export of operators with special names"
  , input          =
      "module ModuleWithExport ((.), (!), (~), (.+.), (Test..||.)) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName ".", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "!", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "~", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ".+.", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Test..||.", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleStarExports :: Test
moduleStarExports = TestCase
  { testName       = "Star exports"
  , input          =
      "module Data.Kind ( Type, Constraint, type (*), type (★) ) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Data.Kind"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName name, pt 1 typ)
                  , entryChildrenVisibility = Nothing
                  }
              | (name, typ) <-
                [ ("Type", Type)
                , ("Constraint", Type)
                , ("*", Family)
                , ("★", Family)
                ]
              ]
          , meReexports          = mempty
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithTypeExportsTest1 :: Test
moduleWithTypeExportsTest1 = TestCase
  { testName       = "Exports type children"
  , input          =
      "module ModuleWithTypeExports\n\
      \  ( Foo\n\
      \  , Bar(type Baz)\n\
      \  )\n\
      \  where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithTypeExports"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 2 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 3 Family)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "Baz", pt 3 Type)
                      ]
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithTypeExportsTest2 :: Test
moduleWithTypeExportsTest2 = TestCase
  { testName       = "Export type operator children"
  , input          =
      "module ModuleWithTypeOpExports\n\
      \  ( (+)\n\
      \  , (**)(type (!!))\n\
      \  )\n\
      \  where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithTypeOpExports"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "+", pt 2 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "**", pt 3 Family)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "!!", pt 3 Type)
                      ]
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithQualifiedOperatorChildrenExportTest :: Test
moduleWithQualifiedOperatorChildrenExportTest = TestCase
  { testName       = "Export qualified children operator"
  , input          =
      "module ModuleWithQualifiedOperatorChildrenExports\n\
      \  ( (Foo.+)\n\
      \  , Foo.Bar((Foo.<><>))\n\
      \  )\n\
      \  where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithQualifiedOperatorChildrenExports"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo.+", pt 2 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Foo.Bar", pt 3 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "<><>", pt 3 Operator)
                      ]
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithDisabledSectionTest1 :: Test
moduleWithDisabledSectionTest1 = TestCase
  { testName       = "Module header with a part guarded by #if 0"
  , input          =
      "module Test\n\
      \  (\n\
      \    Foo( X, Y, Z)\n\
      \#if 0\n\
      \  , Bar\n\
      \#endif\n\
      \  , Baz\n\
      \  ) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 3 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "X", pt 3 Constructor)
                      , (mkUnqualSymName "Y", pt 3 Constructor)
                      , (mkUnqualSymName "Z", pt 3 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz", pt 7 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithDisabledSectionTest2 :: Test
moduleWithDisabledSectionTest2 = TestCase
  { testName       = "Module header with a part guarded by a multiline #if 0"
  , input          =
      "module Test\n\
      \  (\n\
      \    Foo( X, Y, Z)\n\
      \#if  \\\n\
      \           0\n\
      \  , Bar\n\
      \#endif\n\
      \  , Baz\n\
      \  ) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 3 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "X", pt 3 Constructor)
                      , (mkUnqualSymName "Y", pt 3 Constructor)
                      , (mkUnqualSymName "Z", pt 3 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz", pt 8 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }


moduleWithDisabledAndEnabledSectionsTest :: Test
moduleWithDisabledAndEnabledSectionsTest = TestCase
  { testName       = "Module header with a part guarded by #if 0 and some parts guarded by #if <nonzero>"
  , input          =
      "module Test\n\
      \  (\n\
      \    Foo( X, Y, Z)\n\
      \#if 0\n\
      \  , Bar\n\
      \#endif\n\
      \  , Baz\n\
      \#if 10\n\
      \  , Quux\n\
      \#endif\n\
      \#if 01\n\
      \  , Fizz\n\
      \#endif\n\
      \#if 101\n\
      \  , Buzz\n\
      \#endif\n\
      \  ) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 3 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "X", pt 3 Constructor)
                      , (mkUnqualSymName "Y", pt 3 Constructor)
                      , (mkUnqualSymName "Z", pt 3 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz", pt 7 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Quux", pt 9 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Fizz", pt 12 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Buzz", pt 15 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportOfPatternFuncTest :: Test
moduleWithExportOfPatternFuncTest = TestCase
  { testName       = "Export of \"pattern\" function"
  , input          =
      "module ModuleWithExport (pattern) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "pattern", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportOfManyFuncsAndPatternFuncTest :: Test
moduleWithExportOfManyFuncsAndPatternFuncTest = TestCase
  { testName       = "Export of several functions, including \"pattern\" function"
  , input          =
      "module ModuleWithExport (Foo(..), pattern, (++), Bar) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "pattern", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "++", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithoutCommasAndPatternFuncExportBeforeOperator :: Test
moduleWithoutCommasAndPatternFuncExportBeforeOperator = TestCase
  { testName       =
      "Module without commas in export list and export of pattern function before operator function"
  , input          =
      "module ModuleWithExport (Foo(..) pattern (++) Bar) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "pattern", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "++", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithoutCommasAndPatternFuncExportBeforeConstructorWithChildren :: Test
moduleWithoutCommasAndPatternFuncExportBeforeConstructorWithChildren = TestCase
  { testName       =
      "Module without commas in export list and export of pattern function before constructor with children"
  , input          =
      "module ModuleWithExport (pattern Foo(..) (++) Bar) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "pattern", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "++", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithoutCommasAndPatternFuncExportBeforeOperatorConstructorWithChildren :: Test
moduleWithoutCommasAndPatternFuncExportBeforeOperatorConstructorWithChildren = TestCase
  { testName       =
      "Module without commas in export list and export of pattern function before operator constructor with children"
  , input          =
      "module ModuleWithExport (pattern (:++)(..) (++) Bar) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName ":++", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "pattern", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "++", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithoutCommasAndSeveralPatternExports :: Test
moduleWithoutCommasAndSeveralPatternExports = TestCase
  { testName       =
      "Export of several functions without commas, including \"pattern\" function"
  , input          =
      "module ModuleWithExport (Foo(..) pattern (:++) Bar pattern Baz) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":++", pt 1 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz", pt 1 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }
moduleWithExportListWithoutCommasTest :: Test
moduleWithExportListWithoutCommasTest = TestCase
  { testName       = "Export list without commas"
  , input          =
      "module ModuleWithExport (foo Bar(..) Baz(Quux, Fizz) pattern Pat pattern (:!:) module Frob (:$:) (:$$:)(..) module Bazzz (:$$*:)((:$$$*:), (:$$$**:)) module Quuxxx) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "foo", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz", pt 1 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "Quux", pt 1 Constructor)
                      , (mkUnqualSymName "Fizz", pt 1 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Pat", pt 1 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":!:", pt 1 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$:", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$:", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$*:", pt 1 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName ":$$$*:",  pt 1 Constructor)
                      , (mkUnqualSymName ":$$$**:", pt 1 Constructor)
                      ]
                  }
              ]
          , meReexports          = S.fromList
              [ mkModuleName name
              | name <- ["Frob", "Bazzz", "Quuxxx"]
              ]
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportListWithoutCommasAndStructuresAfterNameWithoutChildrenTest :: Test
moduleWithExportListWithoutCommasAndStructuresAfterNameWithoutChildrenTest = TestCase
  { testName       = "Export list without commas and structures after name without children"
  , input          =
      "module ModuleWithExport (foo module Foo (++) module Bar baz pattern Baz quux type Quux pattern Pat module Patterns) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName name, pt 1 typ)
                  , entryChildrenVisibility = Nothing
                  }
              | (name, typ) <-
                [ ("foo",  Function)
                , ("++",   Operator)
                , ("baz",  Function)
                , ("Baz",  Pattern)
                , ("quux", Function)
                , ("Quux", Family)
                , ("Pat",  Pattern)
                ]
              ]
          , meReexports          = S.fromList
              [ mkModuleName name
              | name <- ["Foo", "Bar", "Patterns"]
              ]
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithUnbalancedParensInExportList :: Test
moduleWithUnbalancedParensInExportList = TestCase
  { testName       = "Module with unbalanced parens in export list"
  , input          =
      "module Test\n\
      \#if FOO\n\
      \  ( Foo(X, Y)\n\
      \#else\n\
      \  ( Foo\n\
      \#endif\n\
      \  , Bar\n\
      \  ) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 3 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "X", pt 3 Constructor)
                      , (mkUnqualSymName "Y", pt 3 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 5 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 7 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithUnbalancedParensInExportChildrenList :: Test
moduleWithUnbalancedParensInExportChildrenList = TestCase
  { testName       = "Module with unbalanced parens in export children list"
  , input          =
      "module Test\n\
      \  ( Foo\n\
      \#if FOO\n\
      \      ( X\n\
      \#else\n\
      \      ( Z\n\
      \#endif\n\
      \      , Y\n\
      \      )\n\
      \  , Bar\n\
      \  ) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 2 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "X", pt 4 Constructor)
                      , (mkUnqualSymName "Y", pt 8 Constructor)
                      , (mkUnqualSymName "Z", pt 6 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 10 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithDuplicateModuleNameTest :: Test
moduleWithDuplicateModuleNameTest = TestCase
  { testName       = "Module duplicate module name"
  , input          =
      "#if FOO\n\
      \module Test\n\
      \#else\n\
      \module Test\n\
      \#endif\n\
      \  ( Foo( X, Y, Z)\n\
      \  , Bar\n\
      \  ) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 6 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "X", pt 6 Constructor)
                      , (mkUnqualSymName "Y", pt 6 Constructor)
                      , (mkUnqualSymName "Z", pt 6 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 7 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportsThatHaveChildrenListWithoutCommasTest :: Test
moduleWithExportsThatHaveChildrenListWithoutCommasTest = TestCase
  { testName       = "Exports that have children list without commas"
  , input          =
      "module ModuleWithExport (Bar(..), Baz(Quux Fizz), (:$:), (:$$*:)((:$$$*:) (:$$$**:))) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz", pt 1 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "Quux", pt 1 Constructor)
                      , (mkUnqualSymName "Fizz", pt 1 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$:", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$*:", pt 1 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName ":$$$*:",  pt 1 Constructor)
                      , (mkUnqualSymName ":$$$**:", pt 1 Constructor)
                      ]
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

importRegressionTests :: TestTree
importRegressionTests = testGroup "tests that caused problems before"
  [ doTest aesonHeaderTest
  , doTest unixCompatHeaderTest
  ]

tests :: TestTree
tests = testGroup "Header analysis tests"
  [ doTest simpleHeaderTest
  , testGroup "imports"
    [ doTest moduleWithUnqualifiedImportTest
    , doTest moduleWithUnqualifiedSourceImportTest
    , doTest moduleWithUnqualifiedSafeImportTest
    , doTest moduleWithPatternImportTest
    , doTest moduleWithUnqualifiedImportAndEmptyImportListTest
    , doTest moduleWithUnqualifiedImportAndEmptyHiddenImportListTest
    , doTest moduleWithUnqualifiedImportAndSingletonImportListTest
    , doTest moduleWithUnqualifiedImportAndNonemptyImportListTest
    , doTest moduleWithUnqualifiedImportAndNonemptyImportListWithDifferentVisibilitiesTest
    , doTest moduleWithQualifiedImportTest
    , doTest moduleWithQualifiedSafeAndPackageImportTest
    , doTest moduleWithQualifiedImportAndAliasTest
    , doTest moduleWithImportAndAliasTest
    , doTest moduleWithImportAndAliasAndHidingImportListTest
    , doTest moduleWithImportOfSpeciallyNamedOperatorsTest
    , doTest moduleWithMultipleImports
    , doTest moduleWithImportsAfterFunctionDefinition
    , doTest moduleWithImportsAfterOperatorDefinition
    , doTest moduleWithImportsAfterExclamationMarkOperatorDefinition
    , doTest moduleWithImportsAfterDotOperatorDefinition
    , doTest moduleWithParensInImportList1
    , doTest moduleWithParensInImportList2
    , doTest moduleWithMultilinePreprocessor
    , testGroup "pattern as a function name"
        [ doTest moduleWithImportOfPatternFuncTest
        , doTest moduleWithImportOfManyFuncsAndPatternFuncTest
        , doTest moduleWithoutCommasAndPatternFuncImportBeforeOperator
        , doTest moduleWithoutCommasAndPatternFuncImportBeforeConstructorWithChildren
        , doTest moduleWithoutCommasAndPatternFuncImportBeforeOperatorConstructorWithChildren
        , doTest moduleWithoutCommasAndSeveralPatternImports
        ]
    , testGroup "malformed import lists"
        [ doTest moduleWithAmbigousImportList
        , doTest moduleWithImportListWithoutCommas
        , doTest moduleWithImportsThatHaveChildrenListWithoutCommas
        , doTest moduleWithUnbalancedParensInImportList
        , doTest moduleWithUnbalancedParensInImportChildrenList
        , doTest moduleWithSingleHSC2HSDirectiveInImportList
        , doTest moduleWithSomeHSC2HSDirectivesInImportList1
        , doTest moduleWithSomeHSC2HSDirectivesInImportList2
        , doTest moduleWithSomeHSC2HSDirectivesInImportList3
        ]
    , importRegressionTests
    ]
  , testGroup "exports"
    [ doTest moduleWithEmptyExportsTest
    , doTest moduleWithQuaifiedExportsTest
    , doTest moduleWithExportsTest
    , doTest moduleWithMultilineExportsTest
    , doTest moduleWithExportsOfSpeciallyNamedOperatorsTest
    , doTest moduleStarExports
    , doTest moduleWithTypeExportsTest1
    , doTest moduleWithTypeExportsTest2
    , doTest moduleWithQualifiedOperatorChildrenExportTest
    , doTest moduleWithDisabledSectionTest1
    , doTest moduleWithDisabledSectionTest2
    , doTest moduleWithDisabledAndEnabledSectionsTest
    , testGroup "pattern as a function name"
        [ doTest moduleWithExportOfPatternFuncTest
        , doTest moduleWithExportOfManyFuncsAndPatternFuncTest
        , doTest moduleWithoutCommasAndPatternFuncExportBeforeOperator
        , doTest moduleWithoutCommasAndPatternFuncExportBeforeConstructorWithChildren
        , doTest moduleWithoutCommasAndPatternFuncExportBeforeOperatorConstructorWithChildren
        , doTest moduleWithoutCommasAndSeveralPatternExports
        ]
    , testGroup "malformed export lists"
        [ doTest moduleWithExportListWithoutCommasTest
        , doTest moduleWithExportsThatHaveChildrenListWithoutCommasTest
        , doTest moduleWithExportListWithoutCommasAndStructuresAfterNameWithoutChildrenTest
        , doTest moduleWithUnbalancedParensInExportList
        , doTest moduleWithUnbalancedParensInExportChildrenList
        , doTest moduleWithDuplicateModuleNameTest
        ]
    ]
  ]

doTest :: HasCallStack => Test -> TestTree
doTest TestCase{testName, input, expectedResult} =
  testCase testName $ do
    (res, logs) <- runWriterT $ runSimpleLoggerT (Just (Custom (tell . (:[])))) Debug $ runErrorExceptT $ analyzeHeader filename tokens
    let logsDoc = "Logs, size " <> pretty (length logs) <> ":" ## PP.indent 2 (PP.vcat logs)
    case res of
      Left msg               -> assertFailure $ displayDocString $ pretty msg ## logsDoc
      Right (Nothing, _)     -> assertFailure $ displayDocString $
        "No header detected, but was expecting header" ## pretty expectedResult ## logsDoc
      Right (Just header, _) -> do
        let msg = ppDictHeader "Headers are different" $
              ("Input" :-> PP.dquotes (pretty input)) :
              [ name :-> ppDictAssocList ["Actual" :-> x, "Expected" :-> y]
              | (name, different, x, y) <-
                [ ( "ModuleName"
                  , mhModName header /= mhModName expectedResult
                  , pretty $ mhModName header
                  , pretty $ mhModName expectedResult
                  )
                , ( "Exports"
                  , mhExports header /= mhExports expectedResult
                  , pretty $ mhExports header
                  , pretty $ mhExports expectedResult
                  )
                , ( "ImportQualifiers"
                  , mhImportQualifiers header /= mhImportQualifiers expectedResult
                  , ppMapWith pretty ppNE $ mhImportQualifiers header
                  , ppMapWith pretty ppNE $ mhImportQualifiers expectedResult
                  )
                , ( "Imports"
                  , mhImports header /= mhImports expectedResult
                  , ppSubkeyMapWith pretty pretty ppNE $ mhImports header
                  , ppSubkeyMapWith pretty pretty ppNE $ mhImports expectedResult
                  )
                ]
              , different
              ]
        unless (header == expectedResult) $
          assertFailure $ displayDocString $ msg ## logsDoc
  where
    tokens :: [Pos ServerToken]
    tokens = tokenize (T.unpack (unFullPath filename)) $ TE.encodeUtf8 input
