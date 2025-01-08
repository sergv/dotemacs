-- Copyright (C) 2016-2022 Sergey Vinokurov <serg.foo@gmail.com>
-- Copyright (C) 2014-2016 Sebastian Wiesner <swiesner@lunaryorn.com>
-- Copyright (C) 2016-2018 Danny Navarro <j@dannynavarro.net>
-- Copyright (C) 2015 Mark Karpov <markkarpov@opmbx.org>
-- Copyright (C) 2015 Michael Alan Dorman <mdorman@ironicdesign.com>
-- Copyright (C) 2014 Gracjan Polak <gracjanpolak@gmail.com>

-- This file is not part of GNU Emacs.

-- This program is free software; you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.

-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.

-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

#if __GLASGOW_HASKELL__ >= 800
#define GHC_INCLUDES_VERSION_MACRO 1
#endif

#if defined(GHC_INCLUDES_VERSION_MACRO)

# if MIN_VERSION_Cabal(3, 14, 0)
#  define Cabal314OrLater 1
#  define Cabal38OrLater 1
#  define Cabal36OrLater 1
#  define Cabal32OrLater 1
#  define Cabal30OrLater 1
#  define Cabal24OrLater 1
#  define Cabal22OrLater 1
#  define Cabal20OrLater 1
# elif MIN_VERSION_Cabal(3, 8, 0)
#  define Cabal38OrLater 1
#  define Cabal36OrLater 1
#  define Cabal32OrLater 1
#  define Cabal30OrLater 1
#  define Cabal24OrLater 1
#  define Cabal22OrLater 1
#  define Cabal20OrLater 1
# elif MIN_VERSION_Cabal(3, 6, 0)
#  define Cabal36OrLater 1
#  define Cabal32OrLater 1
#  define Cabal30OrLater 1
#  define Cabal24OrLater 1
#  define Cabal22OrLater 1
#  define Cabal20OrLater 1
# elif MIN_VERSION_Cabal(3, 2, 0)
#  define Cabal32OrLater 1
#  define Cabal30OrLater 1
#  define Cabal24OrLater 1
#  define Cabal22OrLater 1
#  define Cabal20OrLater 1
# elif MIN_VERSION_Cabal(3, 0, 0)
#  define Cabal30OrLater 1
#  define Cabal24OrLater 1
#  define Cabal22OrLater 1
#  define Cabal20OrLater 1
# elif MIN_VERSION_Cabal(2, 3, 0)
#  define Cabal24OrLater 1
#  define Cabal22OrLater 1
#  define Cabal20OrLater 1
# elif MIN_VERSION_Cabal(2, 1, 0)
#  define Cabal22 1
#  define Cabal22OrLater 1
#  define Cabal20OrLater 1
# elif MIN_VERSION_Cabal(2, 0, 0)
#  define Cabal20OrLater 1
# endif

# if MIN_VERSION_bytestring(0, 10, 2)
#  define bytestring_10_2_or_later 1
#  define bytestring_10_0_or_later 1
# elif MIN_VERSION_bytestring(0, 10, 0)
#  define bytestring_10_0_or_later 1
# endif

#else

# if __GLASGOW_HASKELL__ > 810
#  define bytestring_10_2_or_later 1
#  define bytestring_10_0_or_later 1
# elif __GLASGOW_HASKELL__ > 704
#  define bytestring_10_0_or_later 1
#  define bytestring_10_0_or_later 1
# endif

#endif

#if __GLASGOW_HASKELL__ >= 704
# define Cabal114OrMore 1
#endif

#if __GLASGOW_HASKELL__ < 710
# define Cabal118OrLess 1
#endif

#if __GLASGOW_HASKELL__ <= 706
#undef HAVE_DATA_FUNCTOR_IDENTITY
#else
#define HAVE_DATA_FUNCTOR_IDENTITY
#endif

import Prelude (IO, Maybe(..), Either(..), FilePath, Bool(..), Char, Eq(..), Ord(..), Semigroup(..), String, Enum(..), Bounded(..), Show(..), ($), unlines, (++), concatMap, putStrLn, (=<<), any, elem, pure, flip, (.), otherwise, const, Functor(..), fst, map, Monoid(..), id, maybe, Num(..), not, fromIntegral)

import qualified Data.ByteString.Char8 as C8
#if defined(bytestring_10_2_or_later)
import qualified Data.ByteString.Builder as BS.Builder
#elif defined(bytestring_10_0_or_later)
import qualified Data.ByteString.Lazy.Builder as BS.Builder
#else
import qualified Data.ByteString.Lazy.Char8 as CL8
#endif

import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription ()

#if defined(Cabal20OrLater)
import Distribution.Types.Benchmark (benchmarkBuildInfo, benchmarkInterface)
import Distribution.Types.ForeignLib (foreignLibBuildInfo)
import Distribution.Types.TestSuite (testBuildInfo, testInterface)

import Distribution.PackageDescription (allLibraries, libName)
#else
import Distribution.PackageDescription (library)
#endif


import qualified Control.Applicative as A
import Control.Monad (when)
#if defined(Cabal22OrLater)
import qualified Data.ByteString as BS
#endif
import Data.Bits (unsafeShiftL, unsafeShiftR, xor)
import Data.Char (ord)
#if defined(HAVE_DATA_FUNCTOR_IDENTITY)
import Data.Functor.Identity
#endif
import Data.Function (on)
import Data.List (nub, foldl', intersperse)
import Data.Maybe (maybeToList)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
#if defined(Cabal32OrLater)
import Data.List.NonEmpty (toList)
#endif
import Data.Set (Set)
import qualified Data.Set as S
#if defined(Cabal118OrLess)
import Distribution.Compiler
  (CompilerFlavor(GHC), CompilerId(CompilerId), buildCompilerFlavor)
#else
import Distribution.Compiler
  (AbiTag(NoAbiTag), CompilerFlavor(GHC), CompilerId(CompilerId),
   CompilerInfo, buildCompilerFlavor, unknownCompilerInfo)
#endif
import Distribution.Package
  (pkgName, Dependency(..))
import Distribution.PackageDescription
  (GenericPackageDescription, PackageDescription(..),
   TestSuiteInterface(..), BuildInfo(..), Library, Executable,
   allBuildInfo, usedExtensions, allLanguages, hcOptions, exeName,
   buildInfo, modulePath, libBuildInfo, exposedModules)
import Distribution.System (buildPlatform)
import Language.Haskell.Extension (Extension(..),Language(..))
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropFileName, normalise, isPathSeparator, dropTrailingPathSeparator)
import System.Info (compilerVersion)
import System.IO (Handle, hPutStrLn, stderr, stdout)

#if __GLASGOW_HASKELL__ >= 710 && !defined(Cabal20OrLater) && !defined(Cabal22OrLater)
import Data.Version (Version)
#endif

#if defined(Cabal114OrMore)
import Distribution.PackageDescription (BenchmarkInterface(..))
#endif

#if defined(Cabal20OrLater)
import Distribution.Package (unPackageName, depPkgName, PackageName)
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec(..))
import Distribution.Types.ForeignLib (ForeignLib(foreignLibName))
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import qualified Distribution.Version as CabalVersion
import Distribution.Types.Benchmark (Benchmark(benchmarkName))
import Distribution.Types.TestSuite (TestSuite(testName))
#else
import Control.Arrow (second)
import Data.Version (showVersion)
import Distribution.Package (PackageName(..))
import Distribution.PackageDescription.Configuration
  (finalizePackageDescription, mapTreeData)

# if defined(Cabal114OrMore)
import Distribution.PackageDescription
  (TestSuite(..), Benchmark(..),
   condTestSuites, condBenchmarks, benchmarkEnabled, testEnabled)
# else
import Distribution.PackageDescription
  (TestSuite(..), condTestSuites, testEnabled)
# endif
#endif

#if defined(Cabal32OrLater)
import Distribution.PackageDescription (mkFlagAssignment)
#elif defined(Cabal22OrLater)
import Distribution.Types.GenericPackageDescription (mkFlagAssignment)
#endif


#if defined(Cabal22OrLater)
import Distribution.PackageDescription.Parsec
  (runParseResult, parseGenericPackageDescription)
# if defined(Cabal30OrLater)
import Distribution.Parsec.Error (showPError)
# else
import Distribution.Parsec.Common (showPError)
# endif
#elif defined(Cabal20OrLater)
import Distribution.PackageDescription.Parse
  (ParseResult(..), parseGenericPackageDescription)
import Distribution.ParseUtils (locatedErrorMsg)
#else
import Distribution.PackageDescription.Parse
  (ParseResult(..), parsePackageDescription, readPackageDescription)
import Distribution.ParseUtils (locatedErrorMsg)
#endif

#if defined(Cabal30OrLater)
import Distribution.Types.LibraryName (libraryNameString)
#endif

#if defined(Cabal314OrLater)
import Distribution.Utils.Path (SymbolicPathX)
#endif

#if defined(Cabal36OrLater)
import Distribution.Utils.Path (getSymbolicPath)
#endif

#if defined(Cabal20OrLater)
import Distribution.Types.Library (signatures)
#endif

import Data.Int

newtype Hash = Hash Int64
  deriving (Eq, Ord)

mkHash :: C8.ByteString -> Hash
-- getHash = C8.foldl' (\hash c -> hash * 33 + ord c) 5381
mkHash = Hash . C8.foldl' (\hash c -> (hash `unsafeShiftL` 5 + hash) + fromIntegral (ord c)) 5381

instance Semigroup Hash where
  Hash x <> Hash y = Hash $
    x `xor` (y + 0x9e3779b9 + x `unsafeShiftL` 6 + x `unsafeShiftR` 2)

data UnixFilepath = UnixFilepath
  { ufContents :: Builder
  , ufHash     :: {-# UNPACK #-} !Hash
  }

instance Eq UnixFilepath where
  (==) = (==) `on` ufHash

instance Ord UnixFilepath where
  compare = compare `on` ufHash

mkUnixFilepath :: FilePath -> UnixFilepath
mkUnixFilepath x = UnixFilepath
  { ufContents = builderFromByteString x'
  , ufHash     = mkHash x'
  }
  where
    x' = C8.map (\c -> if isPathSeparator c then '/' else c) $ C8.pack $ normalise x

joinPaths :: UnixFilepath -> UnixFilepath -> UnixFilepath
joinPaths (UnixFilepath x xh) (UnixFilepath y yh) = UnixFilepath
  { ufContents = x <> builderFromChar '/' <> y
  , ufHash     = xh <> yh
  }

data Sexp
  = SList [Sexp]
  | SString C8.ByteString
  | SStringBuilder Builder
  | SSymbol C8.ByteString

#if defined(bytestring_10_0_or_later)
type Builder = BS.Builder.Builder

builderFromByteString :: C8.ByteString -> Builder
builderFromByteString = BS.Builder.byteString

builderFromChar :: Char -> Builder
builderFromChar = BS.Builder.char8

hPutBuilder :: Handle -> Builder -> IO ()
hPutBuilder = BS.Builder.hPutBuilder

#else
type Builder = Endo CL8.ByteString

builderFromByteString :: C8.ByteString -> Builder
builderFromByteString x = Endo (CL8.fromChunks [x] `mappend`)

builderFromChar :: Char -> Builder
builderFromChar c = Endo (CL8.singleton c `mappend`)

hPutBuilder :: Handle -> Builder -> IO ()
hPutBuilder h (Endo f) = CL8.hPut h $ f CL8.empty
#endif

sym :: C8.ByteString -> Sexp
sym = SSymbol

renderSexp :: Sexp -> Builder
renderSexp (SSymbol s) = builderFromByteString s
renderSexp (SString s) = dquote `mappend` builderFromByteString s `mappend` dquote
  where
    dquote = builderFromChar '"'
renderSexp (SStringBuilder s) = dquote `mappend` s `mappend` dquote
  where
    dquote = builderFromChar '"'
renderSexp (SList xs)  =
  lparen `mappend` mconcat (intersperse space (map renderSexp xs)) `mappend` rparen
  where
    lparen = builderFromChar '('
    rparen = builderFromChar ')'
    space  = builderFromChar ' '

class ToSexp a  where
  toSexp :: a -> Sexp

instance ToSexp C8.ByteString where
  toSexp = SString

instance ToSexp UnixFilepath where
  toSexp = SStringBuilder . ufContents

instance ToSexp Extension where
  toSexp (EnableExtension ext) = toSexp (C8.pack (show ext))
  toSexp (DisableExtension ext) = toSexp ("No" `mappend` C8.pack (show ext))
  toSexp (UnknownExtension ext) = toSexp (C8.pack ext)

instance ToSexp Language where
  toSexp (UnknownLanguage lang) = toSexp (C8.pack lang)
  toSexp lang = toSexp (C8.pack (show lang))

instance ToSexp Dependency where
  toSexp = toSexp . C8.pack . unPackageName' . depPkgName'

instance ToSexp Sexp where
  toSexp = id

instance ToSexp Bool where
  toSexp b = SSymbol $ if b then "t" else "nil"

instance ToSexp a => ToSexp [a] where
  toSexp = SList . map toSexp

instance ToSexp a => ToSexp (Maybe a) where
  toSexp Nothing  = SSymbol "nil"
  toSexp (Just x) = toSexp x

instance ToSexp ModuleName where
  toSexp = toSexp . map C8.pack . ModuleName.components

instance (ToSexp a, ToSexp b, ToSexp c, ToSexp d) => ToSexp (a, b, c, d) where
  toSexp (a, b, c, d) = SList [toSexp a, toSexp b, toSexp c, toSexp d]

instance (ToSexp a, ToSexp b, ToSexp c, ToSexp d, ToSexp e) => ToSexp (a, b, c, d, e) where
  toSexp (a, b, c, d, e) = SList [toSexp a, toSexp b, toSexp c, toSexp d, toSexp e]

cons :: (ToSexp a, ToSexp b) => a -> [b] -> Sexp
cons h t = SList (toSexp h : map toSexp t)

getSourceDirectories :: [BuildInfo] -> UnixFilepath -> [UnixFilepath]
getSourceDirectories buildInfo cabalDir =
  map (cabalDir `joinPaths`) (concatMap hsSourceDirs' buildInfo)

hsSourceDirs' :: BuildInfo -> [UnixFilepath]
hsSourceDirs' =
  map mkUnixFilepath .
#if defined(Cabal36OrLater)
  map getSymbolicPath . hsSourceDirs
#else
  hsSourceDirs
#endif

serializePackageDescription :: PackageDescription -> FilePath -> Sexp
serializePackageDescription pkgDesc projectDir =
  SList
    [ cons (sym "source-directories") sourceDirs
    , cons (sym "extensions") exts
    , cons (sym "languages") langs
    , cons (sym "other-options") (cppOpts ++ ghcOpts)
    , cons (sym "should-include-version-header") [not ghcIncludesVersionMacro]
    , cons (sym "package-name") [packageName]
    , cons (sym "components") $ getComponents packageName pkgDesc
    ]
  where
    packageName = C8.pack $ unPackageName' $ pkgName $ package pkgDesc

    buildInfo :: [BuildInfo]
    buildInfo = allBuildInfo pkgDesc

    projectDir' :: UnixFilepath
    projectDir' = mkUnixFilepath projectDir

    sourceDirs :: [UnixFilepath]
    sourceDirs = ordNub (getSourceDirectories buildInfo projectDir')

    exts :: [Extension]
#if MIN_VERSION_Cabal(1, 22, 0)
    exts = ordNub (concatMap usedExtensions buildInfo)
#else
    exts = nub (concatMap usedExtensions buildInfo)
#endif

    langs :: [Language]
    langs = nub (concatMap allLanguages buildInfo)

    -- The "cpp-options" configuration field.
    cppOpts :: [C8.ByteString]
    cppOpts =
      ordNub (map C8.pack (concatMap cppOptions buildInfo))

    -- The "ghc-options" configuration field.
    ghcOpts :: [C8.ByteString]
    ghcOpts =
      ordNub (map C8.pack (concatMap (hcOptions GHC) buildInfo))

data ComponentType
  = CTLibrary
  | CTForeignLibrary
  | CTExecutable
  | CTTestSuite
  | CTBenchmark
  deriving (Eq, Ord, Show, Enum, Bounded)

componentTypePrefix :: ComponentType -> C8.ByteString
componentTypePrefix x = case x of
  CTLibrary        -> "lib"
  CTForeignLibrary -> "flib"
  CTExecutable     -> "exe"
  CTTestSuite      -> "test"
  CTBenchmark      -> "bench"

instance ToSexp ComponentType where
  toSexp = toSexp . componentTypePrefix

-- | Gather files and modules that constitute each component.
getComponents
  :: C8.ByteString
  -> PackageDescription
  -> [(ComponentType, C8.ByteString, Maybe C8.ByteString, [ModuleName], [UnixFilepath])]
getComponents packageName pkgDescr =
  [ (CTLibrary, name, Nothing, exposedModules lib ++ libSignatures lib ++ biMods bi, hsSourceDirs' bi)
  | lib <- allLibraries' pkgDescr
  , let bi = libBuildInfo lib
  , let name = maybe packageName C8.pack $ libName' lib
  ] ++
#if defined(Cabal20OrLater)
  [ (CTForeignLibrary, C8.pack (foreignLibName' flib), Nothing, biMods bi, hsSourceDirs' bi)
  | flib <- foreignLibs pkgDescr
  , let bi = foreignLibBuildInfo flib
  ] ++
#endif
  [ (CTExecutable, C8.pack (exeName' exe), Just (getSymbolicPath' (modulePath exe)), biMods bi, hsSourceDirs' bi)
  | exe <- executables pkgDescr
  , let bi = buildInfo exe
  ] ++
  [ (CTTestSuite, C8.pack (testName' tst), exeFile, maybeToList extraMod ++ biMods bi, hsSourceDirs' bi)
  | tst <- testSuites pkgDescr
  , let bi = testBuildInfo tst
  , let (exeFile, extraMod) = case testInterface tst of
          TestSuiteExeV10 _ path  -> (Just (getSymbolicPath' path), Nothing)
          TestSuiteLibV09 _ modName -> (Nothing, Just modName)
          TestSuiteUnsupported{}  -> (Nothing, Nothing)
  ]
#if defined(Cabal114OrMore)
  ++
  [ (CTBenchmark, C8.pack (benchmarkName' tst), exeFile, biMods bi, hsSourceDirs' bi)
  | tst <- benchmarks pkgDescr
  , let bi = benchmarkBuildInfo tst
  , let exeFile = case benchmarkInterface tst of
          BenchmarkExeV10 _ path -> Just (getSymbolicPath' path)
          BenchmarkUnsupported{} -> Nothing
  ]
#endif
  where
#if defined(Cabal20OrLater)
    biMods bi =
      otherModules bi
# if defined(Cabal22OrLater)
      ++ virtualModules bi
# endif
      ++ autogenModules bi
#else
    biMods = otherModules
#endif


#if defined(Cabal314OrLater)
getSymbolicPath' :: SymbolicPathX a b c -> C8.ByteString
getSymbolicPath' = C8.pack . getSymbolicPath
#endif

#if !defined(Cabal314OrLater)
getSymbolicPath' :: String -> C8.ByteString
getSymbolicPath' = C8.pack
#endif


libSignatures :: Library -> [ModuleName]
libSignatures =
#if defined(Cabal20OrLater)
  signatures
#else
  const []
#endif

getCabalConfiguration :: ConfigurationFile -> IO Sexp
getCabalConfiguration configFile = do
  genericDesc <-
    case configFile of
      CabalFile path -> readGenericPkgDescr path
  case getConcretePackageDescription genericDesc of
    Left e        -> die' $ "Issue with package configuration\n" ++ show e
    Right pkgDesc -> pure $ serializePackageDescription pkgDesc projectDir
  where
    projectDir :: FilePath
    projectDir = dropTrailingPathSeparator $ dropFileName $ configFilePath configFile

readGenericPkgDescr :: FilePath -> IO GenericPackageDescription
readGenericPkgDescr path = do
  contents <- readCabalFileContentsFromFile path
  case parsePkgDescr path contents of
    Left msgs ->
      die' $ "Failed to parse cabal '" ++ path ++ "':\n" ++
        unlines msgs
    Right x   -> pure x

newtype CabalFileContents = CabalFileContents
  { unCabalFileContents ::
#if defined(Cabal22OrLater)
      BS.ByteString
#else
      String
#endif
  }

readCabalFileContentsFromFile :: FilePath -> IO CabalFileContents
readCabalFileContentsFromFile =
  fmap CabalFileContents .
#if defined(Cabal22OrLater)
    BS.readFile
#else
    readFile
#endif

parsePkgDescr :: FilePath -> CabalFileContents -> Either [String] GenericPackageDescription
parsePkgDescr _fileName cabalFileContents =
#if defined(Cabal32OrLater)
  case runParseResult $ parseGenericPackageDescription $ unCabalFileContents cabalFileContents of
    (_warnings, res) ->
      case res of
        Left (_version, errs) -> Left $ map (showPError _fileName) $ toList errs
        Right x -> pure x
#elif defined(Cabal22OrLater)
  case runParseResult $ parseGenericPackageDescription $ unCabalFileContents cabalFileContents of
    (_warnings, res) ->
      case res of
        Left (_version, errs) -> Left $ map (showPError _fileName) errs
        Right x -> pure x
#elif defined(Cabal20OrLater)
  case parseGenericPackageDescription $ unCabalFileContents cabalFileContents of
    ParseFailed failure ->
      let (_line, msg) = locatedErrorMsg failure
      in Left [msg]
    ParseOk _warnings x  -> Right x
#else
  case parsePackageDescription $ unCabalFileContents cabalFileContents of
    ParseFailed failure ->
      let (_line, msg) = locatedErrorMsg failure
      in Left [msg]
    ParseOk _warnings x  -> Right x
#endif

getConcretePackageDescription
  :: GenericPackageDescription
  -> Either [Dependency] PackageDescription
getConcretePackageDescription genericDesc = do
#if defined(Cabal22OrLater)
  let enabled :: ComponentRequestedSpec
      enabled = ComponentRequestedSpec
        { testsRequested    = True
        , benchmarksRequested = True
        }
  fst A.<$> finalizePD
    (mkFlagAssignment []) -- Flag assignment
    enabled         -- Enable all components
    (const True)      -- Whether given dependency is available
    buildPlatform
    buildCompilerId
    []          -- Additional constraints
    genericDesc
#elif defined(Cabal20OrLater)
  let enabled :: ComponentRequestedSpec
    enabled = ComponentRequestedSpec
      { testsRequested    = True
      , benchmarksRequested = True
      }
  fst A.<$> finalizePD
    []       -- Flag assignment
    enabled    -- Enable all components
    (const True) -- Whether given dependency is available
    buildPlatform
    buildCompilerId
    []       -- Additional constraints
    genericDesc
#elif Cabal114OrMore
   -- This let block is eerily like one in Cabal.Distribution.Simple.Configure
   let enableTest :: TestSuite -> TestSuite
     enableTest t = t { testEnabled = True }
     enableBenchmark :: Benchmark -> Benchmark
     enableBenchmark bm = bm { benchmarkEnabled = True }
     flaggedTests =
       map (second (mapTreeData enableTest)) (condTestSuites genericDesc)
     flaggedBenchmarks =
       map
         (second (mapTreeData enableBenchmark))
         (condBenchmarks genericDesc)
     genericDesc' =
       genericDesc
       { condTestSuites = flaggedTests
       , condBenchmarks = flaggedBenchmarks
       }
   fst A.<$> finalizePackageDescription
     []
     (const True)
     buildPlatform
     buildCompilerId
     []
     genericDesc'
#else
   -- This let block is eerily like one in Cabal.Distribution.Simple.Configure
   let enableTest :: TestSuite -> TestSuite
     enableTest t = t { testEnabled = True }
     flaggedTests =
       map (second (mapTreeData enableTest)) (condTestSuites genericDesc)
     genericDesc' =
       genericDesc
       { condTestSuites = flaggedTests
       }
   fst A.<$> finalizePackageDescription
     []
     (const True)
     buildPlatform
     buildCompilerId
     []
     genericDesc'
#endif

#if defined(Cabal118OrLess)
buildCompilerId :: CompilerId
buildCompilerId = CompilerId buildCompilerFlavor compilerVersion
#else
buildCompilerId :: CompilerInfo
buildCompilerId = unknownCompilerInfo compId NoAbiTag
  where
    compId :: CompilerId
    compId = CompilerId buildCompilerFlavor compVersion
# if defined(Cabal20OrLater)
    compVersion :: CabalVersion.Version
    compVersion = CabalVersion.mkVersion' compilerVersion
# else
    compVersion :: Version
    compVersion = compilerVersion
# endif
#endif

allLibraries' :: PackageDescription -> [Library]
allLibraries' =
#if defined(Cabal20OrLater)
  allLibraries
#else
  maybeToList . library
#endif

libName' :: Library -> Maybe String
libName' =
#if defined(Cabal30OrLater)
  fmap unUnqualComponentName . libraryNameString . libName
#elif defined(Cabal20OrLater)
  fmap unUnqualComponentName . libName
#else
  const Nothing
#endif

exeName' :: Executable -> String
exeName' =
#if defined(Cabal20OrLater)
  unUnqualComponentName . exeName
#else
  exeName
#endif

#if defined(Cabal20OrLater)
foreignLibName' :: ForeignLib -> String
foreignLibName' =
  unUnqualComponentName . foreignLibName
#endif

testName' :: TestSuite -> String
testName' =
#if defined(Cabal20OrLater)
  unUnqualComponentName . testName
#else
  testName
#endif

#if defined(Cabal114OrMore)
benchmarkName' :: Benchmark -> FilePath
benchmarkName' =
# if defined(Cabal20OrLater)
  unUnqualComponentName . benchmarkName
# else
  benchmarkName
# endif
#endif


depPkgName' :: Dependency -> PackageName
depPkgName' =
#if defined(Cabal20OrLater)
  depPkgName
#else
  let f (Dependency x _) = x in f
#endif

unPackageName' :: PackageName -> String
unPackageName' =
#if defined(Cabal20OrLater)
  unPackageName
#else
  let f (PackageName x) = x in f
#endif


ghcIncludesVersionMacro :: Bool
ghcIncludesVersionMacro =
#if defined(GHC_INCLUDES_VERSION_MACRO)
  True
#else
  False
#endif

ordNub :: forall a. Ord a => [a] -> [a]
ordNub = go S.empty
  where
    go :: Set a -> [a] -> [a]
    go _   []   = []
    go !acc (x:xs)
      | S.member x acc = go acc xs
      | otherwise      = x : go (S.insert x acc) xs

die' :: String -> IO a
die' msg = do
  hPutStrLn stderr msg
  exitFailure

#if !defined(HAVE_DATA_FUNCTOR_IDENTITY)
newtype Identity a = Identity { runIdentity :: a }
#endif

newtype ConfigurationFile = CabalFile FilePath

configFilePath :: ConfigurationFile -> FilePath
configFilePath (CabalFile path) = path

data Config f = Config
  { cfgInputFile :: f ConfigurationFile
  }

reifyConfig :: Config Maybe -> IO (Config Identity)
reifyConfig Config{cfgInputFile} = do
  cfgInputFile' <- case cfgInputFile of
    Nothing   -> die' "Input file not specified. Use --cabal-file to specify one."
    Just path -> pure path
  pure Config
    { cfgInputFile = Identity cfgInputFile'
    }

optionDescr :: [OptDescr (Config Maybe -> Config Maybe)]
optionDescr =
  [ Option
      []
      ["cabal-file"]
      (ReqArg (\path cfg -> cfg { cfgInputFile = Just (CabalFile path) }) "FILE")
      "Cabal file to process"
  ]

defaultConfig :: Config Maybe
defaultConfig = Config
  { cfgInputFile = Nothing
  }

main' :: Config Identity -> IO ()
main' Config{cfgInputFile} =
  hPutBuilder stdout . renderSexp =<<
    getCabalConfiguration (runIdentity cfgInputFile)

main :: IO ()
main = do
  args <- getArgs
  when (any (`elem` ["-h", "--help"]) args) $ do
    putStrLn usage
    exitSuccess
  case getOpt' RequireOrder optionDescr args of
    (fs, [],  [],  []) -> do
      let cfg = foldl' (flip ($)) defaultConfig fs
      main' =<< reifyConfig cfg
    (_,  x:_, [],  []) ->
      die' $ "Unrecognised argument: " ++ x
    (_,  [],  y:_, []) ->
      die' $ "Unrecognised command-line option: " ++ y
    (_,  _,   _,   es) ->
      die' $ "Failed to parse command-line options:\n" ++ unlines es
  where
    header = "Usage: get-cabal-configuration [OPTION...]"
    usage = usageInfo header optionDescr
