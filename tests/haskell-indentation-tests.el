;; haskell-indentation-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  5 September 2024
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'cl))

(require 'dante)
(require 'alex-mode)
(require 'happy-mode)
(require 'haskell-abbrev+)
(require 'haskell-block-indent)
(require 'haskell-format)
(require 'haskell-misc)
(require 'haskell-regexen)
(require 'haskell-smart-operators-mode)
(require 'haskell-sort-imports)

(require 'common)
(require 'ert)
(require 'search)
(require 'tests-utils)
(require 'treesit-setup)

(cl-defmacro haskell-indentation-tests--test-treesitter
    (&key name
          contents
          expected-value
          expected-result)
  (let ((mode 'haskell-ts-mode))
    `(ert-deftest ,name ()
       :expected-result ,(or expected-result :passed) ;;:failed
       (tests-utils--test-buffer-contents
        :action
        (progn
          (let ((fallback-indentations (haskell-indentation-find-indentations)))
            ;; Mostly test that it doesn’t throw an error. Should always
            ;; produce some entries because it would include treesitter
            ;; indentation which these tests are expected to always have.
            (should (not (null fallback-indentations))))
          (haskell-misc--indent-line-with-treesitter))
        :contents ,contents
        :expected-value ,expected-value
        :initialisation (,mode)
        :buffer-id
        ,(string->symbol (format "haskell-indentation-tests-%s" mode))))))

(cl-defmacro haskell-indentation-tests--test-treesitter-region
    (&key name
          contents
          expected-value)
  (let ((mode 'haskell-ts-mode))
    `(ert-deftest ,name ()
       (tests-utils--test-buffer-contents
        :action
        (save-excursion
          (let ((start nil)
                (end nil))
            (goto-char (point-min))
            (if (re-search-forward "_|_" nil t)
                (replace-match "")
              (error "No _|_ marker for point position within contents:\n%s" ,contents))
            (when (save-excursion
                    (goto-char (point-min))
                    (re-search-forward "_|_" nil t))
              (error "More than one occurrence of _|_ in source"))
            (setf start (point))
            (goto-char (point-min))
            (if (re-search-forward "_||_" nil t)
                (replace-match "")
              (error "No _||_ marker for point position within contents:\n%s" ,contents))
            (when (save-excursion
                    (goto-char (point-min))
                    (re-search-forward "_||_" nil t))
              (error "More than one occurrence of _||_ in source"))
            (setf end (point))
            ;; ‘indent-region’ produces incorrect results because
            ;; of too small ‘treesit--indent-region-batch-size’.
            ;; Increasing it to cover everything is not possible.
            (haskell-format-region-with-treesitter-preserving-position! start end)
            (insert "_|_")))
        :contents ,contents
        :expected-value ,expected-value
        :initialisation (,mode)
        :suppress-cursor t
        :buffer-id
        ,(string->symbol (format "haskell-indentation-tests-%s" mode))))))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-1
 :contents
 (tests-utils--multiline
  "scoreMatches (R seps (R needle (R haystacks Stop))) = do"
  ""
  "  (matches :: U.Vector SortKey) <- runWithEarlyTermination $ do"
  "    let processOne :: forall ss. ReusableState ss -> Text -> Int -> ST ss ()"
  "        processOne !store !haystack !n = do"
  "          let haystackLen :: Int"
  "              !haystackLen = T.length haystack"
  "          -- frobnicator"
  "          !match <- foo $ fuzzyMatch"
  "              _|_store"
  "              (computeHeatmap store haystack haystackLen seps')"
  "              needleSegments"
  "              haystack"
  "          for_ match $ \\Match{mScore} -> do"
  "            let !sortKey = mkSortKey mScore (fromIntegral haystackLen) (fromIntegral n)"
  "            k <- unsafeIOToST $ Counter.add scoresCount 1"
  "            unsafeIOToST $ UM.unsafeWrite scores k sortKey"
  "    pure todo"
  ""
  "  putStrLn todo2")
 :expected-value
 (tests-utils--multiline
  "scoreMatches (R seps (R needle (R haystacks Stop))) = do"
  ""
  "  (matches :: U.Vector SortKey) <- runWithEarlyTermination $ do"
  "    let processOne :: forall ss. ReusableState ss -> Text -> Int -> ST ss ()"
  "        processOne !store !haystack !n = do"
  "          let haystackLen :: Int"
  "              !haystackLen = T.length haystack"
  "          -- frobnicator"
  "          !match <- foo $ fuzzyMatch"
  "            _|_store"
  "              (computeHeatmap store haystack haystackLen seps')"
  "              needleSegments"
  "              haystack"
  "          for_ match $ \\Match{mScore} -> do"
  "            let !sortKey = mkSortKey mScore (fromIntegral haystackLen) (fromIntegral n)"
  "            k <- unsafeIOToST $ Counter.add scoresCount 1"
  "            unsafeIOToST $ UM.unsafeWrite scores k sortKey"
  "    pure todo"
  ""
  "  putStrLn todo2"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-2
 :contents
 (tests-utils--multiline
  "scoreMatches (R seps (R needle (R haystacks Stop))) = do"
  ""
  "  (matches :: U.Vector SortKey) <- runWithEarlyTermination $ do"
  "    let processOne :: forall ss. ReusableState ss -> Text -> Int -> ST ss ()"
  "        processOne !store !haystack !n = do"
  "          let haystackLen :: Int"
  "              !haystackLen = T.length haystack"
  "          -- frobnicator"
  "          !match <- foo $"
  "            fuzzyMatch"
  "                _|_store"
  "              (computeHeatmap store haystack haystackLen seps')"
  "              needleSegments"
  "              haystack"
  "          for_ match $ \\Match{mScore} -> do"
  "            let !sortKey = mkSortKey mScore (fromIntegral haystackLen) (fromIntegral n)"
  "            k <- unsafeIOToST $ Counter.add scoresCount 1"
  "            unsafeIOToST $ UM.unsafeWrite scores k sortKey"
  "    pure todo"
  ""
  "  putStrLn todo2")
 :expected-value
 (tests-utils--multiline
  "scoreMatches (R seps (R needle (R haystacks Stop))) = do"
  ""
  "  (matches :: U.Vector SortKey) <- runWithEarlyTermination $ do"
  "    let processOne :: forall ss. ReusableState ss -> Text -> Int -> ST ss ()"
  "        processOne !store !haystack !n = do"
  "          let haystackLen :: Int"
  "              !haystackLen = T.length haystack"
  "          -- frobnicator"
  "          !match <- foo $"
  "            fuzzyMatch"
  "              _|_store"
  "              (computeHeatmap store haystack haystackLen seps')"
  "              needleSegments"
  "              haystack"
  "          for_ match $ \\Match{mScore} -> do"
  "            let !sortKey = mkSortKey mScore (fromIntegral haystackLen) (fromIntegral n)"
  "            k <- unsafeIOToST $ Counter.add scoresCount 1"
  "            unsafeIOToST $ UM.unsafeWrite scores k sortKey"
  "    pure todo"
  ""
  "  putStrLn todo2"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-3
 :contents
 (tests-utils--multiline
  "scoreMatches (R seps (R needle (R haystacks Stop))) = do"
  ""
  "  (matches :: U.Vector SortKey) <- runWithEarlyTermination $ do"
  "    let processOne :: forall ss. ReusableState ss -> Text -> Int -> ST ss ()"
  "        processOne !store !haystack !n = do"
  "          let haystackLen :: Int"
  "              !haystackLen = T.length haystack"
  "          -- frobnicator"
  "          !match <-"
  "            foo"
  "              $ fuzzyMatch"
  "               _|_store"
  "               (computeHeatmap store haystack haystackLen seps')"
  "               needleSegments"
  "               haystack"
  "          for_ match $ \\Match{mScore} -> do"
  "            let !sortKey = mkSortKey mScore (fromIntegral haystackLen) (fromIntegral n)"
  "            k <- unsafeIOToST $ Counter.add scoresCount 1"
  "            unsafeIOToST $ UM.unsafeWrite scores k sortKey"
  "    pure todo"
  ""
  "  putStrLn todo2")
 :expected-value
 (tests-utils--multiline
  "scoreMatches (R seps (R needle (R haystacks Stop))) = do"
  ""
  "  (matches :: U.Vector SortKey) <- runWithEarlyTermination $ do"
  "    let processOne :: forall ss. ReusableState ss -> Text -> Int -> ST ss ()"
  "        processOne !store !haystack !n = do"
  "          let haystackLen :: Int"
  "              !haystackLen = T.length haystack"
  "          -- frobnicator"
  "          !match <-"
  "            foo"
  "              $ fuzzyMatch"
  "                  _|_store"
  "               (computeHeatmap store haystack haystackLen seps')"
  "               needleSegments"
  "               haystack"
  "          for_ match $ \\Match{mScore} -> do"
  "            let !sortKey = mkSortKey mScore (fromIntegral haystackLen) (fromIntegral n)"
  "            k <- unsafeIOToST $ Counter.add scoresCount 1"
  "            unsafeIOToST $ UM.unsafeWrite scores k sortKey"
  "    pure todo"
  ""
  "  putStrLn todo2"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-4
 :contents
 (tests-utils--multiline
  "foo = do"
  "  xxx <- foo"
  "    $ bar"
  "          _|_baz"
  "  pure yyy")
 :expected-value
 (tests-utils--multiline
  "foo = do"
  "  xxx <- foo"
  "    $ bar"
  "        _|_baz"
  "  pure yyy"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-5
 :contents
 (tests-utils--multiline
  "foo = do"
  "  xxx <- foo"
  "        _|_quux"
  "    $ bar"
  "        baz"
  "  pure yyy")
 :expected-value
 (tests-utils--multiline
  "foo = do"
  "  xxx <- foo"
  "    _|_quux"
  "    $ bar"
  "        baz"
  "  pure yyy"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-6
 :contents
 (tests-utils--multiline
  "foo ="
  "  flip evalState 0 $ do"
  "    xxx <- foo quux"
  "_|_"
  "    pure yyy")
 :expected-value
 (tests-utils--multiline
  "foo ="
  "  flip evalState 0 $ do"
  "    xxx <- foo quux"
  "    _|_"
  "    pure yyy"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-7
 :contents
 (tests-utils--multiline
  "foo ="
  "  let foo ="
  "        flip evalState 0 $ do"
  "          xxx <- foo quux"
  "  _|_"
  "          pure yyy"
  "  in bar")
 :expected-value
 (tests-utils--multiline
  "foo ="
  "  let foo ="
  "        flip evalState 0 $ do"
  "          xxx <- foo quux"
  "          _|_"
  "          pure yyy"
  "  in bar"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-8
 :contents
 (tests-utils--multiline
  "foo ="
  "  let foo = flip evalState 0 $ do"
  "        xxx <- foo quux"
  "_|_"
  "        pure yyy"
  "  in bar")
 :expected-value
 (tests-utils--multiline
  "foo ="
  "  let foo = flip evalState 0 $ do"
  "        xxx <- foo quux"
  "        _|_"
  "        pure yyy"
  "  in bar"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-9
 :contents
 (tests-utils--multiline
  "foo ref = Bar $ Baz"
  "  foo"
  "  ((\\x y -> XXX $"
  "               _|_M.singleton key $ g value)"
  "   <$> a"
  "   <*> b)")
 :expected-value
 (tests-utils--multiline
  "foo ref = Bar $ Baz"
  "  foo"
  "  ((\\x y -> XXX $"
  "     _|_M.singleton key $ g value)"
  "   <$> a"
  "   <*> b)"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-10
 :contents
 (tests-utils--multiline
  "foo ="
  "  for_ bar $ \\x ->"
  "      _|_bar")
 :expected-value
 (tests-utils--multiline
  "foo ="
  "  for_ bar $ \\x ->"
  "    _|_bar"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-11
 :contents
 (tests-utils--multiline
  "foo ="
  "  for_ bar $ \\x ->"
  "      _|_bar $ baz")
 :expected-value
 (tests-utils--multiline
  "foo ="
  "  for_ bar $ \\x ->"
  "    _|_bar $ baz"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-11a
 :contents
 (tests-utils--multiline
  "foo ="
  "  for_ bar $ \\x ->"
  "      bar $ _|_baz")
 :expected-value
 (tests-utils--multiline
  "foo ="
  "  for_ bar $ \\x ->"
  "    bar $ _|_baz"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-12a
 :contents
 (tests-utils--multiline
  "foo"
  "  :: Foo"
  "         _|_(Bar"
  "         Baz"
  "         Quux)"
  "  -> Int"
  "foo = undefined")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: Foo"
  "       _|_(Bar"
  "         Baz"
  "         Quux)"
  "  -> Int"
  "foo = undefined"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-12b
 :contents
 (tests-utils--multiline
  "foo"
  "  :: Int"
  "  -> Foo"
  "         _|_(Bar"
  "         Baz"
  "         Quux)"
  "  -> Int"
  "foo = undefined")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: Int"
  "  -> Foo"
  "       _|_(Bar"
  "         Baz"
  "         Quux)"
  "  -> Int"
  "foo = undefined"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-12c
 :contents
 (tests-utils--multiline
  "foo"
  "  :: Int"
  "  -> Foo"
  "         _|_(Bar"
  "         Baz"
  "         Quux)"
  "foo = undefined")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: Int"
  "  -> Foo"
  "       _|_(Bar"
  "         Baz"
  "         Quux)"
  "foo = undefined"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-13a
 :contents
 (tests-utils--multiline
  "foo"
  "  :: ( Bar"
  "       _|_, Baz"
  "     , Quux"
  "     )"
  "  -> Int"
  "foo = undefined")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: ( Bar"
  "     _|_, Baz"
  "     , Quux"
  "     )"
  "  -> Int"
  "foo = undefined"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-13b
 :contents
 (tests-utils--multiline
  "foo"
  "  :: Int"
  "  -> ( Bar"
  "       _|_, Baz"
  "     , Quux"
  "     )"
  "foo = undefined")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: Int"
  "  -> ( Bar"
  "     _|_, Baz"
  "     , Quux"
  "     )"
  "foo = undefined"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-14a
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar"
  "      = decombobulate"
  "            _|_frobnicator"
  "      $ x")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar"
  "      = decombobulate"
  "          _|_frobnicator"
  "      $ x"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-14b
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar"
  "      = decombobulate x"
  "            _|_frobnicator"
  "      $ x")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar"
  "      = decombobulate x"
  "          _|_frobnicator"
  "      $ x"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-15a
 :contents
 (tests-utils--multiline
  ""
  "foo = "
  "  [ bar"
  "  , withResource"
  "     _|_(do"
  "        tmp <- getTemporaryDirectory >>= canonicalizePath"
  "        createFreshTempDir tmp [osp|test|])"
  "      removeDirectoryRecursive"
  "  ]"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo = "
  "  [ bar"
  "  , withResource"
  "      _|_(do"
  "        tmp <- getTemporaryDirectory >>= canonicalizePath"
  "        createFreshTempDir tmp [osp|test|])"
  "      removeDirectoryRecursive"
  "  ]"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-apply-15b
 :contents
 (tests-utils--multiline
  ""
  "foo = "
  "  ( bar"
  "  , withResource"
  "     _|_(do"
  "        tmp <- getTemporaryDirectory >>= canonicalizePath"
  "        createFreshTempDir tmp [osp|test|])"
  "      removeDirectoryRecursive"
  "  )"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo = "
  "  ( bar"
  "  , withResource"
  "      _|_(do"
  "        tmp <- getTemporaryDirectory >>= canonicalizePath"
  "        createFreshTempDir tmp [osp|test|])"
  "      removeDirectoryRecursive"
  "  )"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-comment-1
 :contents
 (tests-utils--multiline
  "foo :: Int -> IO ()"
  "foo x = do"
  "  if x > 0"
  "  then"
  "   _|_-- frobnicate"
  "    pure ()"
  "  else"
  "    -- decombobulate"
  "    pure ()")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> IO ()"
  "foo x = do"
  "  if x > 0"
  "  then"
  "    _|_-- frobnicate"
  "    pure ()"
  "  else"
  "    -- decombobulate"
  "    pure ()"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-comment-1a
 :contents
 (tests-utils--multiline
  "foo :: Int -> IO ()"
  "foo x = do"
  "  if x > 0"
  "  then"
  "    -- frobnicate"
  "    pure ()"
  "  else"
  "   _|_-- decombobulate"
  "    pure ()")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> IO ()"
  "foo x = do"
  "  if x > 0"
  "  then"
  "    -- frobnicate"
  "    pure ()"
  "  else"
  "    _|_-- decombobulate"
  "    pure ()"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-comment-2
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  bar"
  "    { quux = 1"
  "       _|_-- Test"
  "    , baz  = 2"
  "    }"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo ="
  "  bar"
  "    { quux = 1"
  "    _|_-- Test"
  "    , baz  = 2"
  "    }"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-comment-3
 :contents
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "         _|_-- foobar"
  "  , quux"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "    _|_-- foobar"
  "  , quux"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-where-1
 :contents
 (tests-utils--multiline
  "loopM :: Applicative m => Int -> Int -> (Int -> m ()) -> m ()"
  "loopM !from !to action = go from"
  "  where"
  "_|_      go !n"
  "        | n == to"
  "        = pure ()"
  "        | otherwise"
  "        = action n *> go (n + 1)")
 :expected-value
 (tests-utils--multiline
  "loopM :: Applicative m => Int -> Int -> (Int -> m ()) -> m ()"
  "loopM !from !to action = go from"
  "  where"
  "    _|_go !n"
  "        | n == to"
  "        = pure ()"
  "        | otherwise"
  "        = action n *> go (n + 1)"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-where-2
 :contents
 (tests-utils--multiline
  "loopM :: Applicative m => Int -> Int -> (Int -> m ()) -> m ()"
  "loopM !from !to action = go from"
  "      _|_where"
  "      go !n"
  "        | n == to"
  "        = pure ()"
  "        | otherwise"
  "        = action n *> go (n + 1)")
 :expected-value
 (tests-utils--multiline
  "loopM :: Applicative m => Int -> Int -> (Int -> m ()) -> m ()"
  "loopM !from !to action = go from"
  "  _|_where"
  "      go !n"
  "        | n == to"
  "        = pure ()"
  "        | otherwise"
  "        = action n *> go (n + 1)"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-where-3
 :contents
 (tests-utils--multiline
  "foo x = bar x"
  "  where"
  "     _|_-- Bar"
  "    bar y = y")
 :expected-value
 (tests-utils--multiline
  "foo x = bar x"
  "  where"
  "    _|_-- Bar"
  "    bar y = y"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-where-4a
 :contents
 (tests-utils--multiline
  "foo x = baz $ bar x"
  "  where"
  "    -- Bar"
  "    bar y = y"
  "     _|_-- Baz"
  "    baz z = z")
 :expected-value
 (tests-utils--multiline
  "foo x = baz $ bar x"
  "  where"
  "    -- Bar"
  "    bar y = y"
  "    _|_-- Baz"
  "    baz z = z"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-where-4b
 :contents
 (tests-utils--multiline
  "foo x = baz $ bar x"
  "  where"
  "    -- Bar"
  "    bar y = y"
  "   _|_-- Baz"
  "    baz z = z")
 :expected-value
 (tests-utils--multiline
  "foo x = baz $ bar x"
  "  where"
  "    -- Bar"
  "    bar y = y"
  "    _|_-- Baz"
  "    baz z = z"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-where-5a
 :contents
 (tests-utils--multiline
  "instance Show AlexInput where"
  "                          _|_show AlexInput{aiPtr, aiIntStore} ="
  "                            printf \"AlexInput 0x%08x 0x%08x\" ptr aiIntStore"
  "                            where"
  "                              ptr :: Word"
  "                              ptr = fromIntegral $ ptrToWordPtr aiPtr")
 :expected-value
 (tests-utils--multiline
  "instance Show AlexInput where"
  "  _|_show AlexInput{aiPtr, aiIntStore} ="
  "                            printf \"AlexInput 0x%08x 0x%08x\" ptr aiIntStore"
  "                            where"
  "                              ptr :: Word"
  "                              ptr = fromIntegral $ ptrToWordPtr aiPtr"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-where-5b
 :contents
 (tests-utils--multiline
  "instance Show AlexInput where"
  "                          _|_show AlexInput{aiPtr, aiIntStore} ="
  "                            printf \"AlexInput 0x%08x 0x%08x\" ptr aiIntStore"
  "                            where"
  "                              ptr :: Word"
  "                              ptr = fromIntegral $ ptrToWordPtr aiPtr"
  ""
  "                          showsPrec = foo")
 :expected-value
 (tests-utils--multiline
  "instance Show AlexInput where"
  "  _|_show AlexInput{aiPtr, aiIntStore} ="
  "    printf \"AlexInput 0x%08x 0x%08x\" ptr aiIntStore"
  "    where"
  "      ptr :: Word"
  "      ptr = fromIntegral $ ptrToWordPtr aiPtr"
  ""
  "  showsPrec = foo"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-class-1
 :contents
 (tests-utils--multiline
  ""
  "class Measurable a where"
  "    _|_measure :: a -> Int"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "class Measurable a where"
  "  _|_measure :: a -> Int"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-class-2a
 :contents
 (tests-utils--multiline
  ""
  "class Measurable a where"
  "    _|_foo :: a -> Int"
  "    bar :: a -> Int"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "class Measurable a where"
  "  _|_foo :: a -> Int"
  "  bar :: a -> Int"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-class-2b
 :contents
 (tests-utils--multiline
  ""
  "class Measurable a where"
  "    foo :: a -> Int"
  "    _|_bar :: a -> Int"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "class Measurable a where"
  "  foo :: a -> Int"
  "  _|_bar :: a -> Int"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-let-1
 :contents
 (tests-utils--multiline
  "foo ="
  "  let xxx = do"
  "          _|_frobnicator"
  "  in y xxx")
 :expected-value
 (tests-utils--multiline
  "foo ="
  "  let xxx = do"
  "        _|_frobnicator"
  "  in y xxx"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-let-2
 :contents
 (tests-utils--multiline
  "foo = do"
  "  let xxx = do"
  "            _|_frobnicator"
  "  pure ok")
 :expected-value
 (tests-utils--multiline
  "foo = do"
  "  let xxx = do"
  "        _|_frobnicator"
  "  pure ok"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-let-3a
 :contents
 (tests-utils--multiline
  "foo :: Int -> IO ()"
  "foo x = do"
  "  let bar ="
  "       _|_baz"
  "          x"
  "          quux"
  "  decombobulate")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> IO ()"
  "foo x = do"
  "  let bar ="
  "        _|_baz"
  "          x"
  "          quux"
  "  decombobulate"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-let-3b
 :contents
 (tests-utils--multiline
  "foo :: Int -> IO ()"
  "foo x = do"
  "  let bar ="
  "        baz"
  "         _|_x"
  "          quux"
  "  decombobulate")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> IO ()"
  "foo x = do"
  "  let bar ="
  "        baz"
  "          _|_x"
  "          quux"
  "  decombobulate"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-let-4a
 :contents
 (tests-utils--multiline
  "foo :: Int -> IO ()"
  "foo x ="
  "  let bar ="
  "       _|_baz"
  "          x"
  "          quux"
  "  in decombobulate")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> IO ()"
  "foo x ="
  "  let bar ="
  "        _|_baz"
  "          x"
  "          quux"
  "  in decombobulate"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-let-4b
 :contents
 (tests-utils--multiline
  "foo :: Int -> IO ()"
  "foo x ="
  "  let bar ="
  "        baz"
  "         _|_x"
  "          quux"
  "  in decombobulate")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> IO ()"
  "foo x ="
  "  let bar ="
  "        baz"
  "          _|_x"
  "          quux"
  "  in decombobulate"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-let-5
 :contents
 (tests-utils--multiline
  "tests :: TestTree"
  "tests = testGroup \"Data.Filesystem.Grep.Tests\""
  "  [ testCase \"grep 1\" $ do"
  "      pwd <- getCurrentDirectory"
  "      let expected = MatchEntry"
  "           _|_{"
  "           }"
  "      xs  <- grep' pwd \"module Data.Filesystem.Grep.Tests\" [\"*.hs\"] False dummyIgnores"
  "      checkEqual xs [expected]"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "tests :: TestTree"
  "tests = testGroup \"Data.Filesystem.Grep.Tests\""
  "  [ testCase \"grep 1\" $ do"
  "      pwd <- getCurrentDirectory"
  "      let expected = MatchEntry"
  "            _|_{"
  "           }"
  "      xs  <- grep' pwd \"module Data.Filesystem.Grep.Tests\" [\"*.hs\"] False dummyIgnores"
  "      checkEqual xs [expected]"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-let-6a
 :contents
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x = do"
  "   let !ptr  = aiPtr input' `plusPtr` 1 -- Drop first newline"
  "       _|_!size = inputSize - 1"
  "       !idx  = positionsIndex ptr size"
  "   pure 1"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x = do"
  "   let !ptr  = aiPtr input' `plusPtr` 1 -- Drop first newline"
  "       _|_!size = inputSize - 1"
  "       !idx  = positionsIndex ptr size"
  "   pure 1"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-let-6b
 :contents
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x = do"
  "   let !ptr  = aiPtr input' `plusPtr` 1"
  "       _|_!size = inputSize - 1"
  "       !idx  = positionsIndex ptr size"
  "   pure 1"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x = do"
  "   let !ptr  = aiPtr input' `plusPtr` 1"
  "       _|_!size = inputSize - 1"
  "       !idx  = positionsIndex ptr size"
  "   pure 1"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-let-6c
 :contents
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x = do"
  "   let ptr  = aiPtr input' `plusPtr` 1"
  "       _|_!size = inputSize - 1"
  "       !idx  = positionsIndex ptr size"
  "   pure 1"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x = do"
  "   let ptr  = aiPtr input' `plusPtr` 1"
  "       _|_!size = inputSize - 1"
  "       !idx  = positionsIndex ptr size"
  "   pure 1"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-let-6d
 :contents
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x = do"
  "   let ptr  = aiPtr input' `plusPtr` 1"
  "       _|_size = inputSize - 1"
  "       !idx  = positionsIndex ptr size"
  "   pure 1"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x = do"
  "   let ptr  = aiPtr input' `plusPtr` 1"
  "       _|_size = inputSize - 1"
  "       !idx  = positionsIndex ptr size"
  "   pure 1"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-let-6e
 :contents
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x = do"
  "   let !ptr  = aiPtr input' `plusPtr` 1"
  "       _|_size = inputSize - 1"
  "       !idx  = positionsIndex ptr size"
  "   pure 1"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x = do"
  "   let !ptr  = aiPtr input' `plusPtr` 1"
  "       _|_size = inputSize - 1"
  "       !idx  = positionsIndex ptr size"
  "   pure 1"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-let-7
 :contents
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x = do"
  "   _|_let !ptr  = aiPtr input' `plusPtr` 1"
  "       !size = inputSize - 1"
  "       !idx  = positionsIndex ptr size"
  "   pure 1"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x = do"
  "  _|_let !ptr  = aiPtr input' `plusPtr` 1"
  "      !size = inputSize - 1"
  "      !idx  = positionsIndex ptr size"
  "  pure 1"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-record-1a
 :contents
 (tests-utils--multiline
  "foo x ="
  "  let bar = baz"
  "          _|_{ quux = x"
  "        }"
  "  in bar")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  let bar = baz"
  "        _|_{ quux = x"
  "        }"
  "  in bar"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-record-1b
 :contents
 (tests-utils--multiline
  "foo x ="
  "  let bar = baz"
  "        { quux = x"
  "         _|_}"
  "  in bar")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  let bar = baz"
  "        { quux = x"
  "        _|_}"
  "  in bar"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-record-2
 :contents
 (tests-utils--multiline
  "foo x ="
  "  let bar = baz"
  "        _|_{ quux = x"
  "        }"
  "  in bar")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  let bar = baz"
  "        _|_{ quux = x"
  "        }"
  "  in bar"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-record-3a
 :contents
 (tests-utils--multiline
  "newtype Foo a = Foo"
  "  { unFoo ::"
  "      IO a"
  "   _|_}")
 :expected-value
 (tests-utils--multiline
  "newtype Foo a = Foo"
  "  { unFoo ::"
  "      IO a"
  "  _|_}"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-record-3b
 :contents
 (tests-utils--multiline
  "newtype Foo a = Foo"
  "  { unFoo ::"
  "     _|_IO a"
  "  }")
 :expected-value
 (tests-utils--multiline
  "newtype Foo a = Foo"
  "  { unFoo ::"
  "      _|_IO a"
  "  }"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-record-3c
 :contents
 (tests-utils--multiline
  "newtype Foo a = Foo"
  "   _|_{ unFoo ::"
  "      IO a"
  "  }")
 :expected-value
 (tests-utils--multiline
  "newtype Foo a = Foo"
  "  _|_{ unFoo ::"
  "      IO a"
  "  }"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-record-3d
 :contents
 (tests-utils--multiline
  "newtype Foo a = Foo"
  "  { unFoo ::"
  "      IO a"
  "   _|_}")
 :expected-value
 (tests-utils--multiline
  "newtype Foo a = Foo"
  "  { unFoo ::"
  "      IO a"
  "  _|_}"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-record-4a
 :contents
 (tests-utils--multiline
  "data Foo a = Foo"
  "  { unFoo ::"
  "      IO a"
  "   _|_}")
 :expected-value
 (tests-utils--multiline
  "data Foo a = Foo"
  "  { unFoo ::"
  "      IO a"
  "  _|_}"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-record-4b
 :contents
 (tests-utils--multiline
  "data Foo a = Foo"
  "  { unFoo ::"
  "     _|_IO a"
  "  }")
 :expected-value
 (tests-utils--multiline
  "data Foo a = Foo"
  "  { unFoo ::"
  "      _|_IO a"
  "  }"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-record-4c
 :contents
 (tests-utils--multiline
  "data Foo a = Foo"
  "   _|_{ unFoo ::"
  "      IO a"
  "  }")
 :expected-value
 (tests-utils--multiline
  "data Foo a = Foo"
  "  _|_{ unFoo ::"
  "      IO a"
  "  }"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-record-5a
 :contents
 (tests-utils--multiline
  "data AlexState = AlexState"
  "  { asInput        :: {-# UNPACK #-} !AlexInput"
  "    _|_, asIntStore     :: {-# UNPACK #-} !Word64"
  "    -- ^ Integer field that stores all the other useful fields for lexing."
  "    , asContextStack :: [Context]"
  "  } deriving (Show, Eq, Ord)")
 :expected-value
 (tests-utils--multiline
  "data AlexState = AlexState"
  "  { asInput        :: {-# UNPACK #-} !AlexInput"
  "  _|_, asIntStore     :: {-# UNPACK #-} !Word64"
  "    -- ^ Integer field that stores all the other useful fields for lexing."
  "    , asContextStack :: [Context]"
  "  } deriving (Show, Eq, Ord)"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-record-5b
 :contents
 (tests-utils--multiline
  "data AlexState = AlexState"
  "  { asInput        :: {-# UNPACK #-} !AlexInput"
  "  , asIntStore     :: {-# UNPACK #-} !Word64"
  "    -- ^ Integer field that stores all the other useful fields for lexing."
  "    _|_, asContextStack :: [Context]"
  "  } deriving (Show, Eq, Ord)")
 :expected-value
 (tests-utils--multiline
  "data AlexState = AlexState"
  "  { asInput        :: {-# UNPACK #-} !AlexInput"
  "  , asIntStore     :: {-# UNPACK #-} !Word64"
  "    -- ^ Integer field that stores all the other useful fields for lexing."
  "  _|_, asContextStack :: [Context]"
  "  } deriving (Show, Eq, Ord)"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-field-update-1a
 :contents
 (tests-utils--multiline
  "foo x ="
  "  let bar = baz"
  "        { quux ="
  "              _|_foo bar baaz"
  "        }"
  "  in bar")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  let bar = baz"
  "        { quux ="
  "            _|_foo bar baaz"
  "        }"
  "  in bar"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-field-update-1b
 :contents
 (tests-utils--multiline
  "foo x ="
  "  let bar = baz"
  "        { quux ="
  "            foo bar baaz"
  "         _|_}"
  "  in bar")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  let bar = baz"
  "        { quux ="
  "            foo bar baaz"
  "        _|_}"
  "  in bar"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-field-update-2
 :contents
 (tests-utils--multiline
  "foo x ="
  "  let bar = baz"
  "        { quux ="
  "              _|_foo $ bar baaz"
  "        }"
  "  in bar")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  let bar = baz"
  "        { quux ="
  "            _|_foo $ bar baaz"
  "        }"
  "  in bar"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-field-update-3a
 :contents
 (tests-utils--multiline
  "foo x ="
  "  baz"
  "    { quux = bar"
  "          _|_{ frobnicate = baar $ baaz"
  "      }"
  "    }")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  baz"
  "    { quux = bar"
  "        _|_{ frobnicate = baar $ baaz"
  "      }"
  "    }"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-field-update-3b
 :contents
 (tests-utils--multiline
  "foo x ="
  "  baz"
  "    { quux = bar"
  "          _|_baar $ baaz"
  "    }")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  baz"
  "    { quux = bar"
  "        _|_baar $ baaz"
  "    }"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-field-update-3c
 :contents
 (tests-utils--multiline
  "foo x ="
  "  baz"
  "    { quux = bar"
  "          _|_(baar, baaz)"
  "    }")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  baz"
  "    { quux = bar"
  "        _|_(baar, baaz)"
  "    }"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-field-update-3d
 :contents
 (tests-utils--multiline
  "foo x ="
  "  baz"
  "    { quux = bar"
  "          _|_[baar, baaz]"
  "    }")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  baz"
  "    { quux = bar"
  "        _|_[baar, baaz]"
  "    }"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-field-update-3e
 :contents
 (tests-utils--multiline
  "foo x ="
  "  baz"
  "    { quux = bar"
  "          _|_[baar| baaz |]"
  "    }")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  baz"
  "    { quux = bar"
  "        _|_[baar| baaz |]"
  "    }"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-field-update-3f
 :contents
 (tests-utils--multiline
  "foo x ="
  "  baz"
  "    { quux = bar"
  "      { frobnicate = baar $ baaz"
  "       _|_}"
  "    }")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  baz"
  "    { quux = bar"
  "      { frobnicate = baar $ baaz"
  "      _|_}"
  "    }"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-alternatives-1
 :contents
 (tests-utils--multiline
  "foo x = case x of"
  "     _|_Bar y -> y * y")
 :expected-value
 (tests-utils--multiline
  "foo x = case x of"
  "  _|_Bar y -> y * y"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-alternatives-2
 :contents
 (tests-utils--multiline
  "quux = do"
  "  let bar ="
  "        [ frobnicate"
  "        , case xyz of"
  "             _|_True  -> 1"
  "            False -> 2"
  "        ]"
  "  pure ()")
 :expected-value
 (tests-utils--multiline
  "quux = do"
  "  let bar ="
  "        [ frobnicate"
  "        , case xyz of"
  "            _|_True  -> 1"
  "            False -> 2"
  "        ]"
  "  pure ()"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-list-1
 :contents
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "    _|_, baz"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  _|_, baz"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-list-1a
 :contents
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , quux"
  "    _|_, baz"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , quux"
  "  _|_, baz"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-list-1b
 :contents
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "    _|_, baz"
  "  , quux"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  _|_, baz"
  "  , quux"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-list-2
 :contents
 (tests-utils--multiline
  "foo x ="
  "  ["
  "  _|_bar"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  ["
  "    _|_bar"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-list-3
 :contents
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , let quux = x in"
  "          _|_y"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , let quux = x in"
  "    _|_y"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-list-3a
 :contents
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , let quux = x"
  "      _|_in y"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , let quux = x"
  "    _|_in y"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-list-4a
 :contents
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , test"
  "      _|_y"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , test"
  "      _|_y"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-list-4b
 :contents
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , Test"
  "        _|_{ frobnicate = 1 }"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , Test"
  "      _|_{ frobnicate = 1 }"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-list-5a
 :contents
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , quux $"
  "      mkQuux"
  "        [ test $ test2 $"
  "                _|_y $ z"
  "        ]"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , quux $"
  "      mkQuux"
  "        [ test $ test2 $"
  "            _|_y $ z"
  "        ]"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-list-5b
 :contents
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , quux $"
  "      mkQuux"
  "        [ test $ test2 $"
  "                _|_y $ z"
  "        | z <- process x"
  "        ]"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , quux $"
  "      mkQuux"
  "        [ test $ test2 $"
  "            _|_y $ z"
  "        | z <- process x"
  "        ]"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-list-6a
 :contents
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , \\x ->"
  "   _|_x + 1"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , \\x ->"
  "      _|_x + 1"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-list-7a
 :contents
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , quux $"
  "             _|_\\x ->"
  "           x + 1"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , quux $"
  "      _|_\\x ->"
  "           x + 1"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-list-7b
 :contents
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , quux $"
  "             \\x ->"
  "           _|_x + 1"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  [ bar"
  "  , baz"
  "  , quux $"
  "             \\x ->"
  "               _|_x + 1"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-complex-1
 :contents
 (tests-utils--multiline
  "fuzzyMatchImpl"
  "  :: forall s. WithCallStack"
  "  => Text"
  "  -> ST s (Maybe (Submatch, Heatmap s))"
  "fuzzyMatchImpl x = do"
  "  heatmap'@(Heatmap heatmap) <- unsafeInterleaveST mkHeatmap"
  "  let"
  "    bigger :: StrCharIdx Int32 -> U.Vector PackedStrCharIdxAndStrByteIdx -> U.Vector PackedStrCharIdxAndStrByteIdx"
  "    bigger x xs = U.unsafeSlice i (U.length xs - i) xs"
  "      where"
  "        (isMember, !i) ="
  "            _|_foo bar"
  "  pure XXX")
 :expected-value
 (tests-utils--multiline
  "fuzzyMatchImpl"
  "  :: forall s. WithCallStack"
  "  => Text"
  "  -> ST s (Maybe (Submatch, Heatmap s))"
  "fuzzyMatchImpl x = do"
  "  heatmap'@(Heatmap heatmap) <- unsafeInterleaveST mkHeatmap"
  "  let"
  "    bigger :: StrCharIdx Int32 -> U.Vector PackedStrCharIdxAndStrByteIdx -> U.Vector PackedStrCharIdxAndStrByteIdx"
  "    bigger x xs = U.unsafeSlice i (U.length xs - i) xs"
  "      where"
  "        (isMember, !i) ="
  "          _|_foo bar"
  "  pure XXX"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-complex-2
 :contents
 (tests-utils--multiline
  ""
  "foo :: (XXX, YYY m) => a -> m a"
  "foo x@Test{test} ="
  "  case x of"
  "    Left err -> throwError $ \"Too bad:\" ## pretty err"
  "    Right (x, y) ->"
  "      baz quux $"
  "        pure result"
  "_|_"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: (XXX, YYY m) => a -> m a"
  "foo x@Test{test} ="
  "  case x of"
  "    Left err -> throwError $ \"Too bad:\" ## pretty err"
  "    Right (x, y) ->"
  "      baz quux $"
  "        pure result"
  "        _|_"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-complex-3
 :contents
 (tests-utils--multiline
  "foo :: (XXX, YYY m) => a -> m a"
  "foo x@Test{test} ="
  "  case x of"
  "    Left err -> throwError $ \"Too bad:\" ## pretty err"
  "    Right (x, y) ->"
  "      baz quux $"
  "        pure result"
  "_|_"
  "  where"
  "    xxx = yyy")
 :expected-value
 (tests-utils--multiline
  "foo :: (XXX, YYY m) => a -> m a"
  "foo x@Test{test} ="
  "  case x of"
  "    Left err -> throwError $ \"Too bad:\" ## pretty err"
  "    Right (x, y) ->"
  "      baz quux $"
  "        pure result"
  "        _|_"
  "  where"
  "    xxx = yyy"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-complex-4
 :contents
 (tests-utils--multiline
  "foo :: Int -> IO Int"
  "foo n ="
  "      _|_bracket (getResource n) releaseResource $ \\_ -> do"
  "           pure n")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> IO Int"
  "foo n ="
  "  _|_bracket (getResource n) releaseResource $ \\_ -> do"
  "           pure n"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-infix-1
 :contents
 (tests-utils--multiline
  "foo :: Int -> IO Int"
  "foo x = do"
  "  bar <-"
  "    baz >>="
  "     _|_quux x"
  "  pure bar")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> IO Int"
  "foo x = do"
  "  bar <-"
  "    baz >>="
  "      _|_quux x"
  "  pure bar"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-infix-2a
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo 0 = 0"
  "foo x ="
  " _|_foo $"
  "    foo $ x - 1")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo 0 = 0"
  "foo x ="
  "  _|_foo $"
  "    foo $ x - 1"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-infix-2b
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo 0 = 0"
  "foo x ="
  "  foo $"
  "   _|_foo $ x - 1")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo 0 = 0"
  "foo x ="
  "  foo $"
  "    _|_foo $ x - 1"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-infix-3
 :contents
 (tests-utils--multiline
  "foo :: Int -> IO Int"
  "foo x = do"
  "  let (a, xs) = evalState (runWriterT action)"
  "                      _|_$ mkAlexState litLoc startCode input'"
  "  pure undefined")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> IO Int"
  "foo x = do"
  "  let (a, xs) = evalState (runWriterT action)"
  "              _|_$ mkAlexState litLoc startCode input'"
  "  pure undefined"))


(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-infix-4a
 :contents
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x ="
  "  let bar = Quux $"
  "              _|_x"
  "  in bar"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x ="
  "  let bar = Quux $"
  "        _|_x"
  "  in bar"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-infix-4b
 :contents
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x ="
  "  let bar = Quux"
  "              _|_x"
  "  in bar"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x ="
  "  let bar = Quux"
  "        _|_x"
  "  in bar"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-infix-4c
 :contents
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x = do"
  "  let bar = Quux"
  "              _|_x"
  "  baz"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Int -> IO Int"
  "foo x = do"
  "  let bar = Quux"
  "        _|_x"
  "  baz"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-infix-5a
 :contents
 (tests-utils--multiline
  ""
  "foo :: [Test]"
  "foo ="
  "  [ \"abc\" ==> [A, B, C]"
  "  , \"deriveJSON defaultOptions ''CI.CI\""
  "   _|_==>"
  "        [T \"deriveJSON\", T \"defaultOptions\", T \"''CI.CI\", Newline 0]"
  "  ]"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: [Test]"
  "foo ="
  "  [ \"abc\" ==> [A, B, C]"
  "  , \"deriveJSON defaultOptions ''CI.CI\""
  "    _|_==>"
  "        [T \"deriveJSON\", T \"defaultOptions\", T \"''CI.CI\", Newline 0]"
  "  ]"
  "")
 )

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-infix-5b
 :contents
 (tests-utils--multiline
  ""
  "foo :: [Test]"
  "foo ="
  "  [ \"abc\" ==> [A, B, C]"
  "  , \"deriveJSON defaultOptions ''CI.CI\""
  "    ==>"
  "     _|_[T \"deriveJSON\", T \"defaultOptions\", T \"''CI.CI\", Newline 0]"
  "  ]"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: [Test]"
  "foo ="
  "  [ \"abc\" ==> [A, B, C]"
  "  , \"deriveJSON defaultOptions ''CI.CI\""
  "    ==>"
  "    _|_[T \"deriveJSON\", T \"defaultOptions\", T \"''CI.CI\", Newline 0]"
  "  ]"
  "")
 )

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-infix-5c
 :contents
 (tests-utils--multiline
  ""
  "foo :: [Test]"
  "foo ="
  "  [ \"abc\" ==> [A, B, C]"
  "  , \"deriveJSON defaultOptions ''CI.CI\""
  "    ==>"
  "                      _|_[T \"deriveJSON\", T \"defaultOptions\", T \"''CI.CI\", Newline 0]"
  "  ]"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: [Test]"
  "foo ="
  "  [ \"abc\" ==> [A, B, C]"
  "  , \"deriveJSON defaultOptions ''CI.CI\""
  "    ==>"
  "    _|_[T \"deriveJSON\", T \"defaultOptions\", T \"''CI.CI\", Newline 0]"
  "  ]"
  "")
 )

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-infix-6
 :contents
 (tests-utils--multiline
  ""
  "foo = "
  "  [ bar"
  "  , x +"
  "     _|_(baz x y z)"
  "  ]"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo = "
  "  [ bar"
  "  , x +"
  "      _|_(baz x y z)"
  "  ]"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-vertical-op-1
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo"
  "  = bar"
  " _|_. baz")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo"
  "  = bar"
  "  _|_. baz"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-vertical-op-2
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo"
  "  = bar"
  " _|_+ baz")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo"
  "  = bar"
  "  _|_+ baz"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-vertical-op-3
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo"
  "  = bar"
  " _|_++ baz")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo"
  "  = bar"
  "  _|_++ baz"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-data-1
 :contents
 (tests-utils--multiline
  ""
  "data Foo"
  "  = Foo Int"
  "  | Bar Foo Foo"
  "_|_")
 :expected-value
 (tests-utils--multiline
  ""
  "data Foo"
  "  = Foo Int"
  "  | Bar Foo Foo"
  "    _|_"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-data-2
 :contents
 (tests-utils--multiline
  "module Test where"
  ""
  "data Foo"
  "  = Foo Int"
  "  | Bar Foo Foo"
  " _|_deriving (Eq, Ord, Show)")
 :expected-value
 (tests-utils--multiline
  "module Test where"
  ""
  "data Foo"
  "  = Foo Int"
  "  | Bar Foo Foo"
  "  _|_deriving (Eq, Ord, Show)"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-data-3
 :contents
 (tests-utils--multiline
  "module Test where"
  ""
  "data Foo"
  "  = Foo Int"
  "  | Bar Foo Foo"
  "_|_")
 :expected-value
 (tests-utils--multiline
  "module Test where"
  ""
  "data Foo"
  "  = Foo Int"
  "  | Bar Foo Foo"
  "    _|_"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-data-4
 :contents
 (tests-utils--multiline
  "module Test where"
  ""
  "data Foo"
  "  = Foo Int"
  " _|_| Bar Foo Foo"
  "  deriving (Eq)")
 :expected-value
 (tests-utils--multiline
  "module Test where"
  ""
  "data Foo"
  "  = Foo Int"
  "  _|_| Bar Foo Foo"
  "  deriving (Eq)"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-gadt-1
 :contents
 (tests-utils--multiline
  "module Test where"
  ""
  "data Foo2 ix where"
  " _|_XXX :: Foo2 Int"
  "  YYY :: a -> Foo2 a")
 :expected-value
 (tests-utils--multiline
  "module Test where"
  ""
  "data Foo2 ix where"
  "  _|_XXX :: Foo2 Int"
  "   YYY :: a -> Foo2 a"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-haddock-1
 :contents
 (tests-utils--multiline
  "foo"
  "  :: Int"
  "   _|_-- ^ test"
  "  -> Int"
  "foo x = x")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: Int"
  "  _|_-- ^ test"
  "  -> Int"
  "foo x = x"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-haddock-1a
 :contents
 (tests-utils--multiline
  "foo"
  "  :: (Int"
  "   _|_-- ^ test"
  "  -> Int)"
  "foo x = x")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: (Int"
  "  _|_-- ^ test"
  "  -> Int)"
  "foo x = x"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-haddock-2
 :contents
 (tests-utils--multiline
  "foo"
  "  :: Int"
  "  -> Int"
  "   _|_-- ^ test"
  "  -> Int"
  "foo x y = x + y")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: Int"
  "  -> Int"
  "  _|_-- ^ test"
  "  -> Int"
  "foo x y = x + y"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-haddock-3
 :contents
 (tests-utils--multiline
  "foo"
  "  :: Int"
  "  -> (Int"
  "   _|_-- ^ test"
  "  -> Int)"
  "foo x y = x + y")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: Int"
  "  -> (Int"
  "  _|_-- ^ test"
  "  -> Int)"
  "foo x y = x + y"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-haddock-4
 :contents
 (tests-utils--multiline
  "bar1 ::"
  "  Int ->"
  "   _|_-- ^ test"
  "  Double ->"
  "  String"
  "bar1 _ _ = \"\"")
 :expected-value
 (tests-utils--multiline
  "bar1 ::"
  "  Int ->"
  "  _|_-- ^ test"
  "  Double ->"
  "  String"
  "bar1 _ _ = \"\""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-haddock-5
 :contents
 (tests-utils--multiline
  "foo2"
  "  :: Int"
  "  ->"
  "    Int"
  "   _|_-- ^ test"
  "  -> Int"
  "foo2 x y = x + y")
 :expected-value
 (tests-utils--multiline
  "foo2"
  "  :: Int"
  "  ->"
  "    Int"
  "    _|_-- ^ test"
  "  -> Int"
  "foo2 x y = x + y"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-haddock-6
 :contents
 (tests-utils--multiline
  "foo"
  "  :: Int"
  "  -> Int"
  "   _|_-- ^ test"
  "foo x = x")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: Int"
  "  -> Int"
  "  _|_-- ^ test"
  "foo x = x"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-haddock-7
 :contents
 (tests-utils--multiline
  "bar1 ::"
  "  Int ->"
  "  Double ->"
  "  String"
  "   _|_-- ^ test"
  "bar1 _ _ = \"\"")
 :expected-value
 (tests-utils--multiline
  "bar1 ::"
  "  Int ->"
  "  Double ->"
  "  String"
  "  _|_-- ^ test"
  "bar1 _ _ = \"\""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-haddock-7a
 :contents
 (tests-utils--multiline
  "bar1 ::"
  "  Int ->"
  "  (Double ->"
  "  String)"
  "   _|_-- ^ test"
  "bar1 _ _ = \"\"")
 :expected-value
 (tests-utils--multiline
  "bar1 ::"
  "  Int ->"
  "  (Double ->"
  "  String)"
  "  _|_-- ^ test"
  "bar1 _ _ = \"\""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-haddock-8
 :contents
 (tests-utils--multiline
  "bar1 ::"
  "  Int ->"
  "  (Double ->"
  "   _|_-- ^ test"
  "  String)"
  "bar1 _ _ = \"\"")
 :expected-value
 (tests-utils--multiline
  "bar1 ::"
  "  Int ->"
  "  (Double ->"
  "  _|_-- ^ test"
  "  String)"
  "bar1 _ _ = \"\""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-haddock-9a
 :contents
 (tests-utils--multiline
  ""
  "{-# INLINE asIntStoreL #-}"
  "  _|_-- | Foo"
  "asIntStoreL :: Lens' AlexState Word64"
  "asIntStoreL = lens asIntStore (\b s -> s { asIntStore = b })"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "{-# INLINE asIntStoreL #-}"
  "_|_-- | Foo"
  "asIntStoreL :: Lens' AlexState Word64"
  "asIntStoreL = lens asIntStore (\b s -> s { asIntStore = b })"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-haddock-9b
 :contents
 (tests-utils--multiline
  ""
  "data AlexState = AlexState"
  "  { asInput        :: {-# UNPACK #-} !AlexInput"
  "  , asIntStore     :: {-# UNPACK #-} !Word64"
  "    -- ^ Integer field that stores all the other useful fields for lexing."
  "  , asContextStack :: [Context]"
  "  } deriving (Show, Eq, Ord)"
  ""
  "{-# INLINE asIntStoreL #-}"
  "  _|_-- | Foo"
  "asIntStoreL :: Lens' AlexState Word64"
  "asIntStoreL = lens asIntStore (\b s -> s { asIntStore = b })"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "data AlexState = AlexState"
  "  { asInput        :: {-# UNPACK #-} !AlexInput"
  "  , asIntStore     :: {-# UNPACK #-} !Word64"
  "    -- ^ Integer field that stores all the other useful fields for lexing."
  "  , asContextStack :: [Context]"
  "  } deriving (Show, Eq, Ord)"
  ""
  "{-# INLINE asIntStoreL #-}"
  "_|_-- | Foo"
  "asIntStoreL :: Lens' AlexState Word64"
  "asIntStoreL = lens asIntStore (\b s -> s { asIntStore = b })"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-haddock-10
 :contents
 (tests-utils--multiline
  ""
  "data AlexState = AlexState"
  "  { asInput        :: {-# UNPACK #-} !AlexInput"
  "  , asIntStore     :: {-# UNPACK #-} !Word64"
  "      _|_-- ^ Integer field that stores all the other useful fields for lexing."
  "  , asContextStack :: [Context]"
  "  } deriving (Show, Eq, Ord)"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "data AlexState = AlexState"
  "  { asInput        :: {-# UNPACK #-} !AlexInput"
  "  , asIntStore     :: {-# UNPACK #-} !Word64"
  "    _|_-- ^ Integer field that stores all the other useful fields for lexing."
  "  , asContextStack :: [Context]"
  "  } deriving (Show, Eq, Ord)"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-type-1a
 :contents
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "  -> f (PB c d)"
  "  -> Either"
  "       Msg"
  "       (Bar"
  "              _|_Quux)"
  "foo = undefined")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "  -> f (PB c d)"
  "  -> Either"
  "       Msg"
  "       (Bar"
  "         _|_Quux)"
  "foo = undefined"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-type-1b
 :contents
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "  -> f (PB c d)"
  "  -> Either"
  "        _|_Msg"
  "       (Bar"
  "         Quux)"
  "foo = undefined")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "  -> f (PB c d)"
  "  -> Either"
  "       _|_Msg"
  "       (Bar"
  "         Quux)"
  "foo = undefined"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-type-2
 :contents
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "  -> f (PB c d)"
  "  -> Either"
  "       Msg"
  "       ( Bar"
  "             _|_Quux"
  "       , Fizz"
  "       )"
  "foo = undefined")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "  -> f (PB c d)"
  "  -> Either"
  "       Msg"
  "       ( Bar"
  "           _|_Quux"
  "       , Fizz"
  "       )"
  "foo = undefined"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-type-3a
 :contents
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "  -> f (PB c d)"
  "  -> Either"
  "       Msg"
  "       (    [Int]"
  "       _|_-> Bar"
  "              Quux"
  "       , Fizz"
  "       )"
  "foo = undefined")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "  -> f (PB c d)"
  "  -> Either"
  "       Msg"
  "       (    [Int]"
  "         _|_-> Bar"
  "              Quux"
  "       , Fizz"
  "       )"
  "foo = undefined"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-type-3b
 :contents
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "  -> f (PB c d)"
  "  -> Either"
  "       Msg"
  "       (    [Int]"
  "         -> Bar"
  "                _|_Quux"
  "       , Fizz"
  "       )"
  "foo = undefined")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "  -> f (PB c d)"
  "  -> Either"
  "       Msg"
  "       (    [Int]"
  "         -> Bar"
  "              _|_Quux"
  "       , Fizz"
  "       )"
  "foo = undefined"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-type-3c
 :contents
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "    _|_-> f (PB c d)"
  "  -> Either"
  "       Msg"
  "       (    [Int]"
  "         -> Bar"
  "              Quux"
  "       , Fizz"
  "       )"
  "foo = undefined")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "  _|_-> f (PB c d)"
  "  -> Either"
  "       Msg"
  "       (    [Int]"
  "         -> Bar"
  "              Quux"
  "       , Fizz"
  "       )"
  "foo = undefined"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-type-3d
 :contents
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "  -> f (PB c d)"
  "  -> Either"
  "       Msg"
  "       ( [Int]"
  "         -> Bar"
  "                _|_Quux"
  "       , Fizz"
  "       )"
  "foo = undefined")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "  -> f (PB c d)"
  "  -> Either"
  "       Msg"
  "       ( [Int]"
  "         -> Bar"
  "              _|_Quux"
  "       , Fizz"
  "       )"
  "foo = undefined"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-type-3e
 :contents
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "  -> f (PB c d)"
  "  -> Either"
  "       Msg"
  "       (                              [Int]"
  "         -> Bar"
  "                _|_Quux"
  "       , Fizz"
  "       )"
  "foo = undefined")
 :expected-value
 (tests-utils--multiline
  "foo"
  "  :: P A B"
  "  -> f (PB c d)"
  "  -> Either"
  "       Msg"
  "       (                              [Int]"
  "         -> Bar"
  "              _|_Quux"
  "       , Fizz"
  "       )"
  "foo = undefined"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-type-4a
 :contents
 (tests-utils--multiline
  "foo :: IO Int"
  "foo = do"
  "  let bar"
  "        :: Foo"
  "              _|_Int"
  "                Int"
  "                 Int"
  "      bar = Foo 1 2 3"
  "  pure 0")
 :expected-value
 (tests-utils--multiline
  "foo :: IO Int"
  "foo = do"
  "  let bar"
  "        :: Foo"
  "             _|_Int"
  "                Int"
  "                 Int"
  "      bar = Foo 1 2 3"
  "  pure 0"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-type-4b
 :contents
 (tests-utils--multiline
  "foo :: IO Int"
  "foo = do"
  "  let bar :: Foo"
  "              _|_Int"
  "                Int"
  "                 Int"
  "      bar = Foo 1 2 3"
  "  pure 0")
 :expected-value
 (tests-utils--multiline
  "foo :: IO Int"
  "foo = do"
  "  let bar :: Foo"
  "        _|_Int"
  "                Int"
  "                 Int"
  "      bar = Foo 1 2 3"
  "  pure 0"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-guard-1a
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar"
  "       _|_| cond1 = 1"
  "      | cond2 = 2")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar"
  "      _|_| cond1 = 1"
  "      | cond2 = 2"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-guard-1b
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar"
  "      | cond1 = 1"
  "       _|_| cond2 = 2")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar"
  "      | cond1 = 1"
  "      _|_| cond2 = 2"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-guard-1c
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar"
  "      | cond1 = 1"
  "      -- Comment"
  "       _|_| cond2 = 2")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar"
  "      | cond1 = 1"
  "      -- Comment"
  "      _|_| cond2 = 2"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-guard-1d
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar"
  "      | cond1"
  "      = 1"
  "      -- Comment"
  "       _|_| cond2"
  "      = 2")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar"
  "      | cond1"
  "      = 1"
  "      -- Comment"
  "      _|_| cond2"
  "      = 2"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-guard-1e
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar"
  "      -- Comment"
  "      | cond1"
  "      = 1"
  "      -- Comment"
  "       _|_| cond2"
  "      = 2")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar"
  "      -- Comment"
  "      | cond1"
  "      = 1"
  "      -- Comment"
  "      _|_| cond2"
  "      = 2"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-guard-2a
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar _|_| cond1 = 1"
  "      | cond2 = 2")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar _|_| cond1 = 1"
  "      | cond2 = 2"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-guard-2ab
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "     bar _|_| cond1 = 1"
  "      | cond2 = 2")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar _|_| cond1 = 1"
  "      | cond2 = 2"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-guard-2b
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar | cond1 = 1"
  "      _|_| cond2 = 2")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar | cond1 = 1"
  "        _|_| cond2 = 2"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-guard-2c
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar | cond1 = 1"
  "        -- Comment"
  "      _|_| cond2 = 2")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar | cond1 = 1"
  "        -- Comment"
  "        _|_| cond2 = 2"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-guard-2d
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "        bar | cond1 = 1"
  "            -- Comment"
  "                    _|_| cond2 = 2")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "        bar | cond1 = 1"
  "            -- Comment"
  "            _|_| cond2 = 2"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-guard-3a
 :contents
 (tests-utils--multiline
  "foldBSM :: (Monoid a, MonadIO m) => (Char -> m a) -> ByteString -> m a"
  "foldBSM f (BSI.BS ptr len) = do"
  "  let ptr' = unsafeForeignPtrToPtr ptr"
  "  let go !acc !n"
  "        | n == len"
  "                                            _|_= pure acc"
  "        | otherwise"
  "                                = do"
  "                                    b <- liftIO $ peekByteOff ptr' n"
  "                                x <- f (BSI.w2c b)"
  "                                go (acc <> x) (n + 1)"
  "  res <- go mempty 0"
  "  liftIO $ touchForeignPtr ptr"
  "  pure res")
 :expected-value
 (tests-utils--multiline
  "foldBSM :: (Monoid a, MonadIO m) => (Char -> m a) -> ByteString -> m a"
  "foldBSM f (BSI.BS ptr len) = do"
  "  let ptr' = unsafeForeignPtrToPtr ptr"
  "  let go !acc !n"
  "        | n == len"
  "        _|_= pure acc"
  "        | otherwise"
  "                                = do"
  "                                    b <- liftIO $ peekByteOff ptr' n"
  "                                x <- f (BSI.w2c b)"
  "                                go (acc <> x) (n + 1)"
  "  res <- go mempty 0"
  "  liftIO $ touchForeignPtr ptr"
  "  pure res"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-guard-3b
 :contents
 (tests-utils--multiline
  "foldBSM :: (Monoid a, MonadIO m) => (Char -> m a) -> ByteString -> m a"
  "foldBSM f (BSI.BS ptr len) = do"
  "  let ptr' = unsafeForeignPtrToPtr ptr"
  "  let go !acc !n"
  "        | n == len"
  "                                            = pure acc"
  "        | otherwise"
  "                                _|_= do"
  "                                    b <- liftIO $ peekByteOff ptr' n"
  "                                x <- f (BSI.w2c b)"
  "                                go (acc <> x) (n + 1)"
  "  res <- go mempty 0"
  "  liftIO $ touchForeignPtr ptr"
  "  pure res")
 :expected-value
 (tests-utils--multiline
  "foldBSM :: (Monoid a, MonadIO m) => (Char -> m a) -> ByteString -> m a"
  "foldBSM f (BSI.BS ptr len) = do"
  "  let ptr' = unsafeForeignPtrToPtr ptr"
  "  let go !acc !n"
  "        | n == len"
  "                                            = pure acc"
  "        | otherwise"
  "        _|_= do"
  "                                    b <- liftIO $ peekByteOff ptr' n"
  "                                x <- f (BSI.w2c b)"
  "                                go (acc <> x) (n + 1)"
  "  res <- go mempty 0"
  "  liftIO $ touchForeignPtr ptr"
  "  pure res"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-guard-4a
 :contents
 (tests-utils--multiline
  ""
  "fixChar :: Char# -> Word8#"
  "fixChar = \case"
  "  -- These should not be translated since Alex knows about them"
  "  '→'#    -> reservedSym"
  "  c# -> case ord# c# of"
  "    c2# | isTrue# (c2# <=# 0x7f#) ->"
  "          wordToWord8# (int2Word# c2#) -- Plain ascii needs no fixing."
  "        | otherwise   ->"
  "                  _|_case generalCategory (C# c#) of"
  "                    UppercaseLetter -> undefined"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "fixChar :: Char# -> Word8#"
  "fixChar = \case"
  "  -- These should not be translated since Alex knows about them"
  "  '→'#    -> reservedSym"
  "  c# -> case ord# c# of"
  "    c2# | isTrue# (c2# <=# 0x7f#) ->"
  "          wordToWord8# (int2Word# c2#) -- Plain ascii needs no fixing."
  "        | otherwise   ->"
  "          _|_case generalCategory (C# c#) of"
  "                    UppercaseLetter -> undefined"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-guard-4b
 :contents
 (tests-utils--multiline
  ""
  "fixChar :: Char# -> Word8#"
  "fixChar = \case"
  "  -- These should not be translated since Alex knows about them"
  "  '→'#    -> reservedSym"
  "  c# -> case ord# c# of"
  "    c2# | isTrue# (c2# <=# 0x7f#) ->"
  "            _|_wordToWord8# (int2Word# c2#) -- Plain ascii needs no fixing."
  "        | otherwise   ->"
  "                  case generalCategory (C# c#) of"
  "                    UppercaseLetter -> undefined"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "fixChar :: Char# -> Word8#"
  "fixChar = \case"
  "  -- These should not be translated since Alex knows about them"
  "  '→'#    -> reservedSym"
  "  c# -> case ord# c# of"
  "    c2# | isTrue# (c2# <=# 0x7f#) ->"
  "          _|_wordToWord8# (int2Word# c2#) -- Plain ascii needs no fixing."
  "        | otherwise   ->"
  "                  case generalCategory (C# c#) of"
  "                    UppercaseLetter -> undefined"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-do-1a
 :contents
 (tests-utils--multiline
  "tests :: TestTree"
  "tests = testGroup \"Data.Filesystem.Grep.Tests\""
  "  [ testCase \"grep 1\" $ do"
  "      _|_pwd <- getCurrentDirectory"
  "      xs  <- grep' pwd \"module Data.Filesystem.Grep.Tests\" [\"*.hs\"] False dummyIgnores"
  "      checkEqual xs [todo]"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "tests :: TestTree"
  "tests = testGroup \"Data.Filesystem.Grep.Tests\""
  "  [ testCase \"grep 1\" $ do"
  "      _|_pwd <- getCurrentDirectory"
  "      xs  <- grep' pwd \"module Data.Filesystem.Grep.Tests\" [\"*.hs\"] False dummyIgnores"
  "      checkEqual xs [todo]"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-do-1b
 :contents
 (tests-utils--multiline
  "tests :: TestTree"
  "tests = testGroup \"Data.Filesystem.Grep.Tests\""
  "  [ testCase \"grep 1\" $ do"
  "     _|_pwd <- getCurrentDirectory"
  "      xs  <- grep' pwd \"module Data.Filesystem.Grep.Tests\" [\"*.hs\"] False dummyIgnores"
  "      checkEqual xs [todo]"
  "  ]")
 :expected-value
 (tests-utils--multiline
  "tests :: TestTree"
  "tests = testGroup \"Data.Filesystem.Grep.Tests\""
  "  [ testCase \"grep 1\" $ do"
  "      _|_pwd <- getCurrentDirectory"
  "       xs  <- grep' pwd \"module Data.Filesystem.Grep.Tests\" [\"*.hs\"] False dummyIgnores"
  "       checkEqual xs [todo]"
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-do-2
 :contents
 (tests-utils--multiline
  "foldBSM :: (Monoid a, MonadIO m) => (Char -> m a) -> ByteString -> m a"
  "foldBSM f (BSI.BS ptr len) = do"
  "  let ptr' = unsafeForeignPtrToPtr ptr"
  "  let go !acc !n"
  "        | n == len"
  "        = pure acc"
  "        | otherwise"
  "        = do"
  "            _|_b <- liftIO $ peekByteOff ptr' n"
  "            x <- f (BSI.w2c b)"
  "            go (acc <> x) (n + 1)"
  "  res <- go mempty 0"
  "  liftIO $ touchForeignPtr ptr"
  "  pure res")
 :expected-value
 (tests-utils--multiline
  "foldBSM :: (Monoid a, MonadIO m) => (Char -> m a) -> ByteString -> m a"
  "foldBSM f (BSI.BS ptr len) = do"
  "  let ptr' = unsafeForeignPtrToPtr ptr"
  "  let go !acc !n"
  "        | n == len"
  "        = pure acc"
  "        | otherwise"
  "        = do"
  "          _|_b <- liftIO $ peekByteOff ptr' n"
  "          x <- f (BSI.w2c b)"
  "          go (acc <> x) (n + 1)"
  "  res <- go mempty 0"
  "  liftIO $ touchForeignPtr ptr"
  "  pure res"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-do-3a
 :contents
 (tests-utils--multiline
  ""
  "foo = do"
  "  bar"
  "      _|_<- quux"
  ""
  "  pure bar"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo = do"
  "  bar"
  "    _|_<- quux"
  ""
  "  pure bar"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-do-3b
 :contents
 (tests-utils--multiline
  ""
  "foo = do"
  "  bar"
  "    <-"
  "          _|_quux"
  ""
  "  pure bar"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo = do"
  "  bar"
  "    <-"
  "      _|_quux"
  ""
  "  pure bar"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-do-3c
 :contents
 (tests-utils--multiline
  ""
  "foo = do"
  "  bar"
  "    <-"
  "          _|_quux $ x"
  ""
  "  pure bar"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo = do"
  "  bar"
  "    <-"
  "      _|_quux $ x"
  ""
  "  pure bar"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-do-3d
 :contents
 (tests-utils--multiline
  ""
  "foo = do"
  "  bar"
  "    <-"
  "      quux $"
  "           _|_x"
  ""
  "  pure bar"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo = do"
  "  bar"
  "    <-"
  "      quux $"
  "        _|_x"
  ""
  "  pure bar"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-do-3e
 :contents
 (tests-utils--multiline
  ""
  "foo = do"
  "  bar"
  "    <- quux $"
  "             _|_x"
  ""
  "  pure bar"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo = do"
  "  bar"
  "    <- quux $"
  "      _|_x"
  ""
  "  pure bar"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-module-header-1a
 :contents
 (tests-utils--multiline
  "module FasterRicherTags.CompactFormat"
  "    _|_( writeTo"
  "  ) where")
 :expected-value
 (tests-utils--multiline
  "module FasterRicherTags.CompactFormat"
  "  _|_( writeTo"
  "  ) where"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-module-header-1b
 :contents
 (tests-utils--multiline
  "module FasterRicherTags.CompactFormat"
  "  ( writeTo"
  "    _|_) where")
 :expected-value
 (tests-utils--multiline
  "module FasterRicherTags.CompactFormat"
  "  ( writeTo"
  "  _|_) where"))


(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-module-header-2
 :contents
 (tests-utils--multiline
  "module FasterRicherTags.CompactFormat"
  "  ( writeTo"
  "      _|_, foo"
  "  ) where")
 :expected-value
 (tests-utils--multiline
  "module FasterRicherTags.CompactFormat"
  "  ( writeTo"
  "  _|_, foo"
  "  ) where"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-module-header-3
 :contents
 (tests-utils--multiline
  "module FasterRicherTags.CompactFormat"
  "  ( writeTo"
  "      _|_foo"
  "  ) where")
 :expected-value
 (tests-utils--multiline
  "module FasterRicherTags.CompactFormat"
  "  ( writeTo"
  "    _|_foo"
  "  ) where"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-multi-way-if-1
 :contents
 (tests-utils--multiline
  "{-# INLINE utf8SizeChar# #-}"
  "utf8SizeChar# :: Addr# -> Int#"
  "utf8SizeChar# a# ="
  "  case word8ToWord# (indexWord8OffAddr# a# 0#) of"
  "    0## -> 0#"
  "    !x# ->"
  "      let !ch0 = word2Int# x# in"
  "      if  | startsWith0# ch0     -> 1#"
  "                _|_| startsWith110# ch0   -> 2#"
  "                | startsWith1110# ch0  -> 3#"
  "                | startsWith11110# ch0 -> 4#"
  "                | otherwise            -> 1#")
 :expected-value
 (tests-utils--multiline
  "{-# INLINE utf8SizeChar# #-}"
  "utf8SizeChar# :: Addr# -> Int#"
  "utf8SizeChar# a# ="
  "  case word8ToWord# (indexWord8OffAddr# a# 0#) of"
  "    0## -> 0#"
  "    !x# ->"
  "      let !ch0 = word2Int# x# in"
  "      if  | startsWith0# ch0     -> 1#"
  "          _|_| startsWith110# ch0   -> 2#"
  "                | startsWith1110# ch0  -> 3#"
  "                | startsWith11110# ch0 -> 4#"
  "                | otherwise            -> 1#"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-multi-way-if-2aa
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x ="
  "  if         _|_| x > 0"
  "        -> x + 1"
  "      | otherwise"
  "        -> negate x")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x ="
  "  if  _|_| x > 0"
  "        -> x + 1"
  "      | otherwise"
  "        -> negate x"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-multi-way-if-2ab
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x ="
  "             if         _|_| x > 0"
  "        -> x + 1"
  "      | otherwise"
  "        -> negate x")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x ="
  "  if  _|_| x > 0"
  "        -> x + 1"
  "      | otherwise"
  "        -> negate x"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-multi-way-if-2b
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x ="
  "  if  | x > 0"
  "        -> x + 1"
  "        _|_| otherwise"
  "        -> negate x")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x ="
  "  if  | x > 0"
  "        -> x + 1"
  "      _|_| otherwise"
  "        -> negate x"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-multi-way-if-2c
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x ="
  "  if  | x > 0"
  "        _|_-> x + 1"
  "      | otherwise"
  "        -> negate x")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x ="
  "  if  | x > 0"
  "      _|_-> x + 1"
  "      | otherwise"
  "        -> negate x"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-multi-way-if-2d
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x ="
  "  if"
  "            _|_| x > 0"
  "        -> x + 1"
  "      | otherwise"
  "        -> negate x")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x ="
  "  if"
  "    _|_| x > 0"
  "        -> x + 1"
  "      | otherwise"
  "        -> negate x"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-multi-way-if-2e
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x ="
  "  if"
  "    | x > 0"
  "        _|_-> x + 1"
  "      | otherwise"
  "        -> negate x")
 :expected-value
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x ="
  "  if"
  "    | x > 0"
  "    _|_-> x + 1"
  "      | otherwise"
  "        -> negate x"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-vanilla-if-1a
 :contents
 (tests-utils--multiline
  ""
  "foo2 :: Int -> Int"
  "foo2 x ="
  "    _|_if x > 0"
  "      then x + 1"
  "      else"
  "        negate x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo2 :: Int -> Int"
  "foo2 x ="
  "  _|_if x > 0"
  "      then x + 1"
  "      else"
  "        negate x"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-vanilla-if-1b
 :contents
 (tests-utils--multiline
  ""
  "foo2 :: Int -> Int"
  "foo2 x ="
  "  if x > 0"
  "      _|_then x + 1"
  "      else"
  "        negate x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo2 :: Int -> Int"
  "foo2 x ="
  "  if x > 0"
  "  _|_then x + 1"
  "      else"
  "        negate x"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-vanilla-if-1c
 :contents
 (tests-utils--multiline
  ""
  "foo2 :: Int -> Int"
  "foo2 x ="
  "  if x > 0"
  "  then x + 1"
  "      _|_else"
  "        negate x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo2 :: Int -> Int"
  "foo2 x ="
  "  if x > 0"
  "  then x + 1"
  "  _|_else"
  "        negate x"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-vanilla-if-1d
 :contents
 (tests-utils--multiline
  ""
  "foo2 :: Int -> Int"
  "foo2 x ="
  "  if x > 0"
  "  then x + 1"
  "  else"
  "        _|_negate x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo2 :: Int -> Int"
  "foo2 x ="
  "  if x > 0"
  "  then x + 1"
  "  else"
  "    _|_negate x"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-vanilla-if-1e
 :contents
 (tests-utils--multiline
  ""
  "foo2 :: Int -> Int"
  "foo2 x ="
  "  if x > 0"
  "  then"
  "         _|_x + 1"
  "  else"
  "    negate x"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo2 :: Int -> Int"
  "foo2 x ="
  "  if x > 0"
  "  then"
  "    _|_x + 1"
  "  else"
  "    negate x"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-tuple-1a
 :contents
 (tests-utils--multiline
  ""
  "foo bar ="
  "  ( x"
  "  , 4"
  "    _|_)"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo bar ="
  "  ( x"
  "  , 4"
  "  _|_)"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-tuple-1b
 :contents
 (tests-utils--multiline
  ""
  "foo bar ="
  "  ( x"
  "    _|_, 4"
  "  )"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo bar ="
  "  ( x"
  "  _|_, 4"
  "  )"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-tuple-1c
 :contents
 (tests-utils--multiline
  ""
  "foo bar ="
  "  ( x,"
  "  _|_4"
  "  )"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo bar ="
  "  ( x,"
  "    _|_4"
  "  )"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-unboxed-tuple-1a
 :contents
 (tests-utils--multiline
  ""
  "foo bar ="
  "  (# x"
  "  , 4"
  "    _|_#)"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo bar ="
  "  (# x"
  "  , 4"
  "  _|_#)"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-tuple-2a
 :contents
 (tests-utils--multiline
  ""
  "foo bar ="
  "  ( x"
  "  , foo"
  "          _|_bar"
  "  )"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo bar ="
  "  ( x"
  "  , foo"
  "      _|_bar"
  "  )"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-tuple-2b
 :contents
 (tests-utils--multiline
  ""
  "foo bar ="
  "  ( x"
  "  , Test"
  "          _|_{ frobnicate = 1 }"
  "  )"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo bar ="
  "  ( x"
  "  , Test"
  "      _|_{ frobnicate = 1 }"
  "  )"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-unboxed-tuple-1b
 :contents
 (tests-utils--multiline
  ""
  "foo bar ="
  "  (# x"
  "    _|_, 4"
  "  #)"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo bar ="
  "  (# x"
  "  _|_, 4"
  "  #)"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-unboxed-tuple-1c
 :contents
 (tests-utils--multiline
  ""
  "foo bar ="
  "  (# x,"
  "  _|_4"
  "  #)"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo bar ="
  "  (# x,"
  "    _|_4"
  "  #)"
  ""))

(haskell-indentation-tests--test-treesitter-region
 :name haskell-indentation-tests--test-treesitter-region-1a
 :contents
 (tests-utils--multiline
  "foo ="
  " _|_["
  "  bar $ quux"
  "  , baz"
  " _||_]")
 :expected-value
 (tests-utils--multiline
  "foo ="
  "  ["
  "    bar $ quux"
  "  , baz"
  "  _|_]"))

(haskell-indentation-tests--test-treesitter-region
 :name haskell-indentation-tests--test-treesitter-region-1b
 :contents
 (tests-utils--multiline
  "foo ="
  " _||_["
  "  bar $ quux"
  "  , baz"
  " _|_]")
 :expected-value
 (tests-utils--multiline
  "foo ="
  "  _|_["
  "    bar $ quux"
  "  , baz"
  "  ]"))

(haskell-indentation-tests--test-treesitter-region
 :name haskell-indentation-tests--test-treesitter-region-2
 :contents
 (tests-utils--multiline
  "_|_module Haskell.Language.LexerSimple.Types"
  "  ( AlexState(..)"
  "  , mkAlexState"
  "  , alexEnterBirdLiterateEnv"
  "  , alexEnterLiterateLatexEnv"
  "  , alexExitLiterateEnv"
  "  , pushContext"
  "  , modifyCommentDepth"
  "  , modifyQuasiquoterDepth"
  "  , modifyPreprocessorDepth"
  "  , addIndentationSize"
  "  , checkQuasiQuoteEndPresent"
  ""
  "  , AlexM"
  "  , runAlexM"
  "  , alexSetInput"
  "  , alexSetNextCode"
  ""
  "  , AlexInput(..)"
  "  , aiLineL"
  "  , takeText"
  "  , countInputSpace"
  "  , extractDefineOrLetName"
  "  , dropUntilNL"
  "  , dropUntilUnescapedNL"
  "  , dropUntilNLOr"
  "  , dropUntilNLOrEither"
  "  , unsafeTextHeadAscii"
  "  , unsafeTextHeadOfTailAscii"
  "  , unsafeTextHead"
  "  , utf8BS"
  ""
  "  , asCodeL"
  "  , asCommentDepthL"
  "  , asQuasiquoterDepthL"
  "  , asIndentationSizeL"
  "  , asPreprocessorDepthL"
  "  , asLiterateLocL"
  "  , asHaveQQEndL"
  ""
  "    -- * Alex interface"
  "  , alexInputPrevChar"
  "  , alexGetByte"
  "  ) where"
  ""
  "import Control.DeepSeq"
  "import Control.Exception"
  "import Control.Monad.ST"
  "import Control.Monad.State.Strict"
  "import Control.Monad.Writer.Strict"
  ""
  "import Data.ByteString qualified as BS"
  "import Data.ByteString.Char8 qualified as C8"
  "import Data.ByteString.Internal qualified as BSI"
  "import Data.Char"
  "import Data.Int"
  "import Data.Text qualified as T"
  "import Data.Text.Encoding qualified as TE"
  "import Data.Vector.Unboxed qualified as U"
  "import Data.Vector.Unboxed.Mutable qualified as UM"
  "import Foreign.ForeignPtr"
  "import Foreign.Ptr"
  "import GHC.Base"
  "import GHC.Ptr"
  "import GHC.Word"
  "import Text.Printf"
  ""
  "import Haskell.Language.Lexer.Types (Context(..), AlexCode(..), LitMode(..), LitStyle(..), SrcPos(..), ServerToken(..), Pos(..), Line(..), increaseLine, Offset(..))"
  "import Haskell.Language.LexerSimple.LensBlaze"
  ""
  "data AlexState = AlexState"
  "    { asInput        :: {-# UNPACK #-} !AlexInput"
  "    , asIntStore     :: {-# UNPACK #-} !Word64"
  "        -- ^ Integer field that stores all the other useful fields for lexing."
  "    , asContextStack :: [Context]"
  "    } deriving (Show, Eq, Ord)"
  ""
  "{-# INLINE asIntStoreL #-}"
  "asIntStoreL :: Lens' AlexState Word64"
  "asIntStoreL = lens asIntStore (\\b s -> s { asIntStore = b })"
  ""
  "{-# INLINE maybeBoolToInt #-}"
  "-- | Encode 'Maybe Bool' as bit mask to store it within integer store."
  "maybeBoolToInt :: Maybe Bool -> Int"
  "maybeBoolToInt = \\case"
  "    Nothing    -> 0"
  "    Just False -> 1"
  "    Just True  -> 2"
  ""
  "{-# INLINE intToMaybeBool #-}"
  "-- | Decode 'Maybe Bool' from bit mask stored within integer store."
  "intToMaybeBool :: Int -> Maybe Bool"
  "intToMaybeBool = \\case"
  "    0 -> Nothing"
  "    1 -> Just False"
  "    2 -> Just True"
  "    x -> error $ \"Invalid integer representation of 'Maybe Bool': \" ++ show x"
  ""
  "{-# INLINE asCodeL              #-}"
  "{-# INLINE asCommentDepthL      #-}"
  "{-# INLINE asQuasiquoterDepthL  #-}"
  "{-# INLINE asIndentationSizeL   #-}"
  "{-# INLINE asPreprocessorDepthL #-}"
  "{-# INLINE asLiterateLocL       #-}"
  "{-# INLINE asHaveQQEndL         #-}"
  "-- | Current Alex state the lexer is in. E.g. comments, string, TH quasiquoter"
  "-- or vanilla toplevel mode."
  "asCodeL :: Lens' AlexState AlexCode"
  "asCommentDepthL, asQuasiquoterDepthL, asIndentationSizeL :: Lens' AlexState Int16"
  "-- | How many directives deep are we."
  "asPreprocessorDepthL :: Lens' AlexState Int16"
  "-- | Whether we're in bird-style or latex-style literate environment"
  "asLiterateLocL :: Lens' AlexState (LitMode LitStyle)"
  "asHaveQQEndL   :: Lens' AlexState (Maybe Bool)"
  "asCodeL              = asIntStoreL . intL 0  0x000f"
  "asCommentDepthL      = asIntStoreL . intL 4  0x03ff"
  "asQuasiquoterDepthL  = asIntStoreL . intL 14 0x03ff"
  "asIndentationSizeL   = asIntStoreL . int16L  24"
  "asPreprocessorDepthL = asIntStoreL . int16L  40"
  "asLiterateLocL       = \\f -> asIntStoreL (intL 56 0x0003 (fmap litLocToInt    . f . intToLitLoc))"
  "asHaveQQEndL         = \\f -> asIntStoreL (intL 58 0x0003 (fmap maybeBoolToInt . f . intToMaybeBool))"
  ""
  "{-# INLINE litLocToInt #-}"
  "litLocToInt :: LitMode LitStyle -> Int"
  "litLocToInt = \\case"
  "    LitVanilla      -> 0"
  "    LitOutside      -> 1"
  "    LitInside Bird  -> 2"
  "    LitInside Latex -> 3"
  ""
  "{-# INLINE intToLitLoc #-}"
  "intToLitLoc :: Int -> LitMode LitStyle"
  "intToLitLoc = \\case"
  "    0 -> LitVanilla"
  "    1 -> LitOutside"
  "    2 -> LitInside Bird"
  "    3 -> LitInside Latex"
  "    x -> error $ \"Invalid literate location representation: \" ++ show x"
  ""
  "mkAlexState :: LitMode Void -> AlexCode -> AlexInput -> AlexState"
  "mkAlexState litLoc startCode input ="
  "    set asCodeL startCode $"
  "    set asLiterateLocL (vacuous litLoc) AlexState"
  "        { asInput        = input"
  "        , asIntStore     = 0"
  "        , asContextStack = []"
  "        }"
  ""
  "{-# INLINE alexEnterBirdLiterateEnv #-}"
  "alexEnterBirdLiterateEnv :: MonadState AlexState m => m ()"
  "alexEnterBirdLiterateEnv ="
  "    modify $ set asLiterateLocL (LitInside Bird)"
  ""
  "{-# INLINE alexEnterLiterateLatexEnv #-}"
  "alexEnterLiterateLatexEnv :: MonadState AlexState m => m ()"
  "alexEnterLiterateLatexEnv ="
  "    modify $ set asLiterateLocL (LitInside Latex)"
  ""
  "{-# INLINE alexExitLiterateEnv #-}"
  "alexExitLiterateEnv :: MonadState AlexState m => m ()"
  "alexExitLiterateEnv ="
  "    modify $ set asLiterateLocL LitOutside"
  ""
  "{-# INLINE pushContext #-}"
  "pushContext :: MonadState AlexState m => Context -> m ()"
  "pushContext ctx ="
  "    modify (\\s -> s { asContextStack = ctx : asContextStack s })"
  ""
  "{-# INLINE modifyCommentDepth #-}"
  "modifyCommentDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16"
  "modifyCommentDepth f = do"
  "    depth <- gets (view asCommentDepthL)"
  "    let !depth' = f depth"
  "    modify $ \\s -> set asCommentDepthL depth' s"
  "    return depth'"
  ""
  "{-# INLINE modifyQuasiquoterDepth #-}"
  "modifyQuasiquoterDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16"
  "modifyQuasiquoterDepth f = do"
  "    depth <- gets (view asQuasiquoterDepthL)"
  "    let !depth' = f depth"
  "    modify $ \\s -> set asQuasiquoterDepthL depth' s"
  "    return depth'"
  ""
  "{-# INLINE modifyPreprocessorDepth #-}"
  "modifyPreprocessorDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16"
  "modifyPreprocessorDepth f = do"
  "    depth <- gets (view asPreprocessorDepthL)"
  "    let !depth' = f depth"
  "    modify $ \\s -> set asPreprocessorDepthL depth' s"
  "    return depth'"
  ""
  "{-# INLINE alexSetInput #-}"
  "alexSetInput :: MonadState AlexState m => AlexInput -> m ()"
  "alexSetInput input = modify $ \\s -> s { asInput = input }"
  ""
  "{-# INLINE alexSetNextCode #-}"
  "alexSetNextCode :: MonadState AlexState m => AlexCode -> m ()"
  "alexSetNextCode code = modify $ set asCodeL code"
  ""
  "{-# INLINE addIndentationSize #-}"
  "addIndentationSize :: MonadState AlexState m => Int16 -> m ()"
  "addIndentationSize x ="
  "  modify (over asIndentationSizeL (+ x))"
  ""
  "data QQEndsState = QQEndsState"
  "    { qqessPresent  :: Int#"
  "    , qqessPrevChar :: Char#"
  "    }"
  ""
  "checkQuasiQuoteEndPresent :: Ptr Word8 -> Bool"
  "checkQuasiQuoteEndPresent"
  "    = (\\x -> isTrue# (qqessPresent x))"
  "    . utf8Foldl' combine (QQEndsState 0# '\\n'#)"
  "    where"
  "    combine :: QQEndsState -> Char# -> QQEndsState"
  "    combine QQEndsState{qqessPresent, qqessPrevChar} c# = QQEndsState"
  "        { qqessPresent      ="
  "          qqessPresent `orI#`"
  "          case (# qqessPrevChar, c# #) of"
  "              (# '|'#, ']'# #) -> 1#"
  "              (# _,    '⟧'# #) -> 1#"
  "              _                -> 0#"
  "        , qqessPrevChar = c#"
  "        }"
  ""
  "type AlexM = WriterT [(AlexInput, ServerToken)] (State AlexState)"
  ""
  "{-# INLINE runAlexM #-}"
  "runAlexM"
  "  :: Bool"
  "  -> LitMode Void"
  "  -> AlexCode"
  "  -> C8.ByteString"
  "  -> AlexM a"
  "  -> (a, [Pos ServerToken])"
  "runAlexM trackPrefixesAndOffsets litLoc startCode input action ="
  "    performIO $"
  "    withAlexInput input $ \\input' inputSize -> do"
  "        let (a, xs) = evalState (runWriterT action)"
  "                    $ mkAlexState litLoc startCode input'"
  "        if trackPrefixesAndOffsets"
  "        then do"
  "            let !ptr  = aiPtr input' `plusPtr` 1 -- Drop first newline"
  "                !size = inputSize - 1"
  "                !idx  = positionsIndex ptr size"
  "                res   ="
  "                    map (\\(x, y) -> Pos (mkSrcPos idx ptr x) y) xs"
  "            pure $! res `deepseq` (a, res)"
  "        else do"
  "            -- Contents of 'xs' has been seq'ed so TokenVals in there should"
  "            -- have been forced and thus should not contain any references to the"
  "            -- original input bytestring. However, in GHC 9.0 it seems that"
  "            -- GHC does some transformation which results in some entries within 'xs'"
  "            -- being not fully evaluated and thus lead to an error since they get"
  "            -- forced outside of 'withForeignPtr' bounds. The call to 'evaluate' below"
  "            -- is intended to prevent such transformation from occuring."
  "            _ <- evaluate xs"
  "            pure (a, map (\\(x, y) -> Pos (mkSrcPosNoPrefix x) y) xs)"
  ""
  "mkSrcPosNoPrefix :: AlexInput -> SrcPos"
  "mkSrcPosNoPrefix input ="
  "    SrcPos { posLine   = view aiLineL input"
  "           , posOffset = Offset 0"
  "           , posPrefix = mempty"
  "           , posSuffix = mempty"
  "           }"
  ""
  "mkSrcPos :: U.Vector Int -> Ptr Word8 -> AlexInput -> SrcPos"
  "mkSrcPos bytesToCharsMap start (input@AlexInput {aiPtr}) ="
  "    SrcPos { posLine = view aiLineL input"
  "           , posOffset"
  "           , posPrefix"
  "           , posSuffix"
  "           }"
  "    where"
  "    lineLen   = view aiLineLengthL input"
  "    posPrefix = TE.decodeUtf8 $ bytesToUtf8BS lineLen $ plusPtr aiPtr $ negate lineLen"
  "    posSuffix = TE.decodeUtf8 $ regionToUtf8BS aiPtr $ dropUntilNL# aiPtr"
  "    posOffset = Offset $ U.unsafeIndex bytesToCharsMap $ minusPtr aiPtr start"
  ""
  ""
  "-- Vector mapping absolute offsets off a pointer into how many utf8 characters"
  "-- were encoded since the pointer start."
  "positionsIndex :: Ptr Word8 -> Int -> U.Vector Int"
  "positionsIndex (Ptr start#) len ="
  "    U.create $ do"
  "        (vec :: UM.MVector s Int) <- UM.new len"
  "        let assignAfter :: Int -> Int -> Int -> ST s ()"
  "            assignAfter start n item = go' n start"
  "                where"
  "                go' :: Int -> Int -> ST s ()"
  "                go' 0 !i = UM.unsafeWrite vec i item"
  "                go' k !i = UM.unsafeWrite vec i item *> go' (k - 1) (i + 1)"
  "            go :: Int# -> Int -> ST s ()"
  "            go bytes# !nChars ="
  "                case utf8SizeChar# (start# `plusAddr#` bytes#) of"
  "                    0#      -> pure ()"
  "                    nBytes# -> do"
  "                        assignAfter (I# bytes#) (I# nBytes#) nChars"
  "                        go (bytes# +# nBytes#) $ nChars + 1"
  "        go 0# 0"
  "        pure vec"
  ""
  ""
  "data AlexInput = AlexInput"
  "    { aiPtr      :: {-# UNPACK #-} !(Ptr Word8)"
  "    , aiIntStore :: {-# UNPACK #-} !Word64"
  "        -- ^ Integer field that stores all the other useful fields for lexing."
  "    } deriving (Eq, Ord)"
  ""
  "instance Show AlexInput where"
  "    show AlexInput{aiPtr, aiIntStore} ="
  "        printf \"AlexInput 0x%08x 0x%08x\" ptr aiIntStore"
  "        where"
  "        ptr :: Word"
  "        ptr = fromIntegral $ ptrToWordPtr aiPtr"
  ""
  "{-# INLINE aiIntStoreL #-}"
  "aiIntStoreL :: Lens' AlexInput Word64"
  "aiIntStoreL = lens aiIntStore (\\b s -> s { aiIntStore = b })"
  ""
  "lineInt32L :: Lens' Int32 Line"
  "lineInt32L = lens (Line . fromIntegral) (\\(Line x) _ -> fromIntegral x)"
  ""
  "int2Int32L :: Lens' Int32 Int"
  "int2Int32L = lens fromIntegral (\\x _ -> fromIntegral x)"
  ""
  "{-# INLINE aiLineL       #-}"
  "{-# INLINE aiLineLengthL #-}"
  "-- | Current line in input stream."
  "aiLineL       :: Lens' AlexInput Line"
  "-- | Length of current line."
  "aiLineLengthL :: Lens' AlexInput Int"
  ""
  "aiLineL       = aiIntStoreL . int32L 0  . lineInt32L"
  "aiLineLengthL = aiIntStoreL . int32L 32 . int2Int32L"
  ""
  "{-# INLINE takeText #-}"
  "takeText :: AlexInput -> Int -> T.Text"
  "takeText AlexInput{aiPtr} len ="
  "    TE.decodeUtf8 $! utf8BS len aiPtr"
  ""
  "countInputSpace :: AlexInput -> Int -> Int"
  "countInputSpace AlexInput{aiPtr} len ="
  "    utf8FoldlBounded len inc 0 aiPtr"
  "    where"
  "    inc acc ' '#  = acc + 1"
  "    inc acc '\\t'# = acc + 8"
  "    inc acc c#    = case fixChar c# of"
  "        1## -> acc + 1"
  "        _   -> acc"
  ""
  "{-# INLINE performIO #-}"
  "performIO :: IO a -> a"
  "performIO = BSI.accursedUnutterablePerformIO"
  ""
  "{-# INLINE withAlexInput #-}"
  "withAlexInput :: C8.ByteString -> (AlexInput -> Int -> IO a) -> IO a"
  "withAlexInput s f ="
  "    case s' of"
  "        BSI.PS ptr offset len ->"
  "            withForeignPtr ptr $ \\ptr' -> do"
  "                let !input ="
  "                        set aiLineL initLine $"
  "                        AlexInput"
  "                            { aiPtr      = ptr' `plusPtr` offset"
  "                            , aiIntStore = 0"
  "                            }"
  "                f input $! len - offset"
  "    where"
  "    -- Line numbering starts from 0 because we're adding additional newline"
  "    -- at the beginning to simplify processing. Thus, line numbers in the"
  "    -- result are 1-based."
  "    initLine = Line 0"
  ""
  "    -- Add '\\0' at the end so that we'll find the end of stream (just"
  "    -- as in the old C days...)"
  "    s' = C8.cons '\\n' $ C8.snoc (C8.snoc (stripBOM s) '\\n') '\\0'"
  "    stripBOM :: C8.ByteString -> C8.ByteString"
  "    stripBOM xs"
  "        | \"\\xEF\\xBB\\xBF\" `C8.isPrefixOf` xs"
  "        = C8.drop 3 xs"
  "        | otherwise"
  "        = xs"
  ""
  "{-# INLINE extractDefineOrLetName #-}"
  "extractDefineOrLetName :: AlexInput -> Int -> T.Text"
  "extractDefineOrLetName AlexInput{aiPtr} n ="
  "    TE.decodeUtf8 $ regionToUtf8BS (Ptr start#) end"
  "    where"
  "    !end        = aiPtr `plusPtr` n"
  "    !(Ptr end#) = end"
  "    start#      = (goBack# (end# `plusAddr#` -1#)) `plusAddr#` 1#"
  ""
  "    goBack# :: Addr# -> Addr#"
  "    goBack# ptr# = case word8ToWord# (indexWord8OffAddr# ptr# 0#) of"
  "        0##  -> ptr#"
  "        9##  -> ptr# -- '\\n'"
  "        10## -> ptr# -- '\\n'"
  "        13## -> ptr# -- '\\r'"
  "        32## -> ptr# -- ' '"
  "        92## -> ptr# -- '\\\\'"
  "        _    -> goBack# (ptr# `plusAddr#` -1#)"
  ""
  "{-# INLINE dropUntilNL #-}"
  "dropUntilNL :: AlexInput -> AlexInput"
  "dropUntilNL input@AlexInput{aiPtr} ="
  "    input { aiPtr = dropUntilNL# aiPtr }"
  ""
  "{-# INLINE dropUntilUnescapedNL #-}"
  "dropUntilUnescapedNL :: AlexInput -> AlexInput"
  "dropUntilUnescapedNL input@AlexInput{aiPtr = start} ="
  "    case dropUntilUnescapedNL# start of"
  "        (# seenNewlines, end #) ->"
  "            over aiLineL (\\(Line n) -> Line (n + seenNewlines)) $"
  "            input { aiPtr = end }"
  ""
  "{-# INLINE dropUntilNLOr #-}"
  "dropUntilNLOr :: Word8 -> AlexInput -> AlexInput"
  "dropUntilNLOr w input@AlexInput{aiPtr} ="
  "    input { aiPtr = dropUntilNLOr# w aiPtr }"
  ""
  "{-# INLINE dropUntilNLOrEither #-}"
  "-- | Drop until either of two bytes."
  "dropUntilNLOrEither :: Word8 -> Word8 -> AlexInput -> AlexInput"
  "dropUntilNLOrEither w1 w2 input@AlexInput{aiPtr} ="
  "    input { aiPtr = dropUntilNLOrEither# w1 w2 aiPtr }"
  ""
  "-- Alex interface"
  ""
  "{-# INLINE alexInputPrevChar #-}"
  "alexInputPrevChar :: AlexInput -> Char"
  "alexInputPrevChar AlexInput{ aiPtr = Ptr ptr# } ="
  "    case base# `minusAddr#` start# of"
  "        0# -> C# (chr# ch0)"
  "        1# -> let !(# x, _ #) = readChar1# start# ch0 in C# x"
  "        2# -> let !(# x, _ #) = readChar2# start# ch0 in C# x"
  "        3# -> let !(# x, _ #) = readChar3# start# ch0 in C# x"
  "        _  -> '\\0' -- Invalid!"
  "    where"
  "    ch0 :: Int#"
  "    !ch0 = word2Int# (word8ToWord# (indexWord8OffAddr# start# 0#))"
  ""
  "    base# = findCharStart ptr# `plusAddr#` -1#"
  ""
  "    start# = findCharStart base#"
  ""
  "    findCharStart :: Addr# -> Addr#"
  "    findCharStart p#"
  "        | startsWith10# w#"
  "        = findCharStart (p# `plusAddr#` -1#)"
  "        | otherwise"
  "        = p#"
  "        where"
  "        w# = word2Int# (word8ToWord# (indexWord8OffAddr# p# 0#))"
  ""
  "{-# INLINE alexGetByte #-}"
  "alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)"
  "alexGetByte input@AlexInput{aiPtr} ="
  "    case nextChar aiPtr of"
  "        (# c#, n, cs #) ->"
  "            case fixChar c# of"
  "                0##  -> Nothing -- Abort on an unknown character"
  "                -- '\\n'"
  "                10## -> Just (10, input')"
  "                    where"
  "                    !input' ="
  "                        over aiLineL increaseLine $"
  "                        set aiLineLengthL 0 $"
  "                        input { aiPtr = cs }"
  "                c    -> Just (b, input')"
  "                    where"
  "                    !b     = W8# (wordToWord8# c)"
  "                    !input' ="
  "                        over aiLineLengthL (+ I# n) $"
  "                        input { aiPtr = cs }"
  ""
  "-- Translate unicode character into special symbol we teached Alex to recognize."
  "{-# INLINE fixChar #-}"
  "fixChar :: Char# -> Word#"
  "fixChar = \\case"
  "    -- These should not be translated since Alex knows about them"
  "    '→'#    -> reservedSym"
  "    '∷'#    -> reservedSym"
  "    '⇒'#    -> reservedSym"
  "    '∀'#    -> reservedSym"
  "    '⦇'#    -> reservedSym"
  "    '⦈'#    -> reservedSym"
  "    '⟦'#    -> reservedSym"
  "    '⟧'#    -> reservedSym"
  "    '\\x00'# -> fullStop"
  "    '\\x01'# -> fullStop"
  "    '\\x02'# -> fullStop"
  "    '\\x03'# -> fullStop"
  "    '\\x04'# -> fullStop"
  "    '\\x05'# -> fullStop"
  "    '\\x06'# -> fullStop"
  "    '\\x07'# -> fullStop"
  "    '\\x08'# -> other"
  "    c# -> case ord# c# of"
  "        c2# | isTrue# (c2# <=# 0x7f#) ->"
  "              int2Word# c2# -- Plain ascii needs no fixing."
  "            | otherwise   ->"
  "                case generalCategory (C# c#) of"
  "                    UppercaseLetter      -> upper"
  "                    LowercaseLetter      -> lower"
  "                    TitlecaseLetter      -> upper"
  "                    ModifierLetter       -> suffix"
  "                    OtherLetter          -> lower"
  "                    NonSpacingMark       -> suffix"
  "                    DecimalNumber        -> digit"
  "                    OtherNumber          -> digit"
  "                    Space                -> space"
  "                    ConnectorPunctuation -> symbol"
  "                    DashPunctuation      -> symbol"
  "                    OtherPunctuation     -> symbol"
  "                    MathSymbol           -> symbol"
  "                    CurrencySymbol       -> symbol"
  "                    ModifierSymbol       -> symbol"
  "                    OtherSymbol          -> symbol"
  ""
  "                    SpacingCombiningMark -> space"
  "                    EnclosingMark        -> other"
  "                    LetterNumber         -> symbol"
  "                    OpenPunctuation      -> symbol"
  "                    ClosePunctuation     -> symbol"
  "                    InitialQuote         -> symbol"
  "                    FinalQuote           -> symbol"
  "                    LineSeparator        -> space"
  "                    ParagraphSeparator   -> space"
  "                    Control              -> other"
  "                    Format               -> other"
  "                    Surrogate            -> other"
  "                    PrivateUse           -> other"
  "                    NotAssigned          -> other"
  "    where"
  "      fullStop, space, upper, lower, symbol :: Word#"
  "      digit, suffix, reservedSym, other :: Word#"
  "      fullStop    = 0x00## -- Don't care about these"
  "      space       = 0x01##"
  "      upper       = 0x02##"
  "      lower       = 0x03##"
  "      symbol      = 0x04##"
  "      digit       = 0x05##"
  "      suffix      = 0x06##"
  "      reservedSym = 0x07##"
  "      other       = 0x08##"
  ""
  "{-# INLINE unsafeTextHeadAscii #-}"
  "unsafeTextHeadAscii :: Ptr Word8 -> Word8"
  "unsafeTextHeadAscii (Ptr ptr#) = W8# (indexWord8OffAddr# ptr# 0#)"
  ""
  "{-# INLINE unsafeTextHeadOfTailAscii #-}"
  "unsafeTextHeadOfTailAscii :: Ptr Word8 -> Word8"
  "unsafeTextHeadOfTailAscii (Ptr ptr#) = W8# (indexWord8OffAddr# ptr# 1#)"
  ""
  "{-# INLINE unsafeTextHead #-}"
  "unsafeTextHead :: Ptr Word8 -> Char"
  "unsafeTextHead x ="
  "    case nextChar x of"
  "        (# c#, _, _ #) -> C# c#"
  ""
  "{-# INLINE nextChar #-}"
  "nextChar :: Ptr Word8 -> (# Char#, Int#, Ptr Word8 #)"
  "nextChar (Ptr ptr#) ="
  "    case utf8DecodeChar# ptr# of"
  "        (# c#, nBytes# #) -> (# c#, nBytes#, Ptr (ptr# `plusAddr#` nBytes#) #)"
  ""
  "{-# INLINE dropUntilNL# #-}"
  "dropUntilNL# :: Ptr Word8 -> Ptr Word8"
  "dropUntilNL# (Ptr start#) = Ptr (go start#)"
  "    where"
  "    go :: Addr# -> Addr#"
  "    go ptr# = case word8ToWord# (indexWord8OffAddr# ptr# 0#) of"
  "        0##  -> ptr#"
  "        10## -> ptr# -- '\\n'"
  "        _    -> go (ptr# `plusAddr#` 1#)"
  ""
  "{-# INLINE dropUntilUnescapedNL# #-}"
  "dropUntilUnescapedNL# :: Ptr Word8 -> (# Int, Ptr Word8 #)"
  "dropUntilUnescapedNL# (Ptr start#) = go 0 start#"
  "    where"
  "    go :: Int -> Addr# -> (# Int, Ptr Word8 #)"
  "    go !n ptr# = case word8ToWord# (indexWord8OffAddr# ptr# 0#) of"
  "        0##  -> (# n, Ptr ptr# #)"
  "        -- '\\n'"
  "        10## -> (# n, Ptr ptr# #)"
  "        -- '\\\\'"
  "        92## ->"
  "            case word8ToWord# (indexWord8OffAddr# ptr# 1#) of"
  "                0##  -> (# n, Ptr (ptr# `plusAddr#` 1#) #)"
  "                -- '\\n'"
  "                10## -> go (n + 1) (ptr# `plusAddr#` 2#)"
  "                _    -> go n (ptr# `plusAddr#` 2#)"
  "        _    -> go n (ptr# `plusAddr#` 1#)"
  ""
  "{-# INLINE dropUntilNLOr# #-}"
  "dropUntilNLOr# :: Word8 -> Ptr Word8 -> Ptr Word8"
  "dropUntilNLOr# (W8# w#) (Ptr start#) = Ptr (go start#)"
  "    where"
  "    go :: Addr# -> Addr#"
  "    go ptr# = case word8ToWord# (indexWord8OffAddr# ptr# 0#) of"
  "        0##  -> ptr#"
  "        -- '\\n'"
  "        10## -> ptr#"
  "        c# | isTrue# (c# `eqWord#` word8ToWord# w#) -> ptr#"
  "           | otherwise                 -> go (ptr# `plusAddr#` 1#)"
  ""
  "{-# INLINE dropUntilNLOrEither# #-}"
  "dropUntilNLOrEither# :: Word8 -> Word8 -> Ptr Word8 -> Ptr Word8"
  "dropUntilNLOrEither# (W8# w1#) (W8# w2#) (Ptr start#) = Ptr (go start#)"
  "    where"
  "    go :: Addr# -> Addr#"
  "    go ptr# = case word8ToWord# (indexWord8OffAddr# ptr# 0#) of"
  "        0##  -> ptr#"
  "        -- '\\n'"
  "        10## -> ptr#"
  "        c# | isTrue# ((c# `eqWord#` word8ToWord# w1#) `orI#` (c# `eqWord#` word8ToWord# w2#))"
  "           -> ptr#"
  "           | otherwise"
  "           -> go (ptr# `plusAddr#` 1#)"
  ""
  "{-# INLINE utf8Foldl' #-}"
  "utf8Foldl' :: forall a. (a -> Char# -> a) -> a -> Ptr Word8 -> a"
  "utf8Foldl' f x0 (Ptr ptr#) ="
  "    go x0 ptr#"
  "    where"
  "    go :: a -> Addr# -> a"
  "    go !acc addr# ="
  "        case utf8DecodeChar# addr# of"
  "            (# _,  0#      #) -> acc"
  "            (# c#, nBytes# #) -> go (acc `f` c#) (addr# `plusAddr#` nBytes#)"
  ""
  "{-# INLINE utf8FoldlBounded #-}"
  "utf8FoldlBounded :: forall a. Int -> (a -> Char# -> a) -> a -> Ptr Word8 -> a"
  "utf8FoldlBounded (I# len#) f x0 (Ptr ptr#) ="
  "    go len# x0 ptr#"
  "    where"
  "    go :: Int#-> a -> Addr# -> a"
  "    go 0# !acc _     = acc"
  "    go n# !acc addr# ="
  "        case utf8DecodeChar# addr# of"
  "            (# _,  0#      #) -> acc"
  "            (# c#, nBytes# #) ->"
  "                go (n# -# 1#) (acc `f` c#) (addr# `plusAddr#` nBytes#)"
  ""
  "{-# INLINE utf8BS #-}"
  "utf8BS :: Int -> Ptr Word8 -> BS.ByteString"
  "utf8BS (I# nChars#) (Ptr start#) ="
  "    BSI.PS (performIO (newForeignPtr_ (Ptr start#))) 0 (I# (go nChars# 0#))"
  "    where"
  "    go :: Int# -> Int# -> Int#"
  "    go 0# bytes# = bytes#"
  "    go k# bytes# ="
  "        case utf8SizeChar# (start# `plusAddr#` bytes#)  of"
  "            0#      -> bytes#"
  "            nBytes# -> go (k# -# 1#) (bytes# +# nBytes#)"
  ""
  "{-# INLINE bytesToUtf8BS #-}"
  "bytesToUtf8BS :: Int -> Ptr Word8 -> BS.ByteString"
  "bytesToUtf8BS (I# nbytes#) (Ptr start#) ="
  "    BSI.PS (performIO (newForeignPtr_ (Ptr start#))) 0 (I# nbytes#)"
  ""
  "{-# INLINE regionToUtf8BS #-}"
  "regionToUtf8BS :: Ptr Word8 -> Ptr Word8 -> BS.ByteString"
  "regionToUtf8BS start end ="
  "    BSI.PS (performIO (newForeignPtr_ start)) 0 (minusPtr end start)"
  ""
  "{-# INLINE utf8DecodeChar# #-}"
  "utf8DecodeChar# :: Addr# -> (# Char#, Int# #)"
  "utf8DecodeChar# a# ="
  "    case word8ToWord# (indexWord8OffAddr# a# 0#) of"
  "        0## -> (# '\\0'#, 0# #)"
  "        x#  ->"
  "            let !ch0 = word2Int# x# in"
  "            if  | startsWith0# ch0     -> (# chr# ch0, 1# #)"
  "                | startsWith110# ch0   -> readChar1# a# ch0"
  "                | startsWith1110# ch0  -> readChar2# a# ch0"
  "                | startsWith11110# ch0 -> readChar3# a# ch0"
  "                | otherwise            -> invalid# 1#"
  ""
  "-- all invalid# sequences end up here:"
  "{-# INLINE invalid# #-}"
  "invalid# :: Int# -> (# Char#, Int# #)"
  "invalid# nBytes# = (# '\\8'#, nBytes# #)"
  "-- TODO: check whether following note from ghc applies to server's lexer:"
  "-- '\\xFFFD' would be the usual replacement character, but"
  "-- that's a valid symbol in Haskell, so will result in a"
  "-- confusing parse error later on.  Instead we use '\\0' which"
  "-- will signal a lexer error immediately."
  ""
  "{-# INLINE readChar1# #-}"
  "readChar1# :: Addr# -> Int# -> (# Char#, Int# #)"
  "readChar1# a# ch0 ="
  "    let !ch1 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 1#)) in"
  "    if noValidUtf8Cont# ch1 then invalid# 1# else"
  "    (# chr# (((ch0 `andI#` 0x3F#) `uncheckedIShiftL#` 6#) `orI#`"
  "              (ch1 `andI#` 0x7F#)),"
  "       2# #)"
  ""
  "{-# INLINE readChar2# #-}"
  "readChar2# :: Addr# -> Int# -> (# Char#, Int# #)"
  "readChar2# a# ch0 ="
  "    let !ch1 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 1#)) in"
  "    if noValidUtf8Cont# ch1 then invalid# 1# else"
  "    let !ch2 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 2#)) in"
  "    if noValidUtf8Cont# ch2 then invalid# 2# else"
  "    (# chr# (((ch0 `andI#` 0x1F#) `uncheckedIShiftL#` 12#) `orI#`"
  "             ((ch1 `andI#` 0x7F#) `uncheckedIShiftL#` 6#)  `orI#`"
  "              (ch2 `andI#` 0x7F#)),"
  "       3# #)"
  ""
  "{-# INLINE readChar3# #-}"
  "readChar3# :: Addr# -> Int# -> (# Char#, Int# #)"
  "readChar3# a# ch0 ="
  "    let !ch1 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 1#)) in"
  "    if noValidUtf8Cont# ch1 then invalid# 1# else"
  "    let !ch2 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 2#)) in"
  "    if noValidUtf8Cont# ch2 then invalid# 2# else"
  "    let !ch3 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 3#)) in"
  "    if noValidUtf8Cont# ch3 then invalid# 3# else"
  "    (# chr# (((ch0 `andI#` 0x0F#) `uncheckedIShiftL#` 18#) `orI#`"
  "             ((ch1 `andI#` 0x7F#) `uncheckedIShiftL#` 12#) `orI#`"
  "             ((ch2 `andI#` 0x7F#) `uncheckedIShiftL#` 6#)  `orI#`"
  "              (ch3 `andI#` 0x7F#)),"
  "       4# #)"
  ""
  "{-# INLINE noValidUtf8Cont# #-}"
  "noValidUtf8Cont# :: Int# -> Bool"
  "noValidUtf8Cont# x = isTrue# ((x <# 0x80#) `orI#` (x ># 0xBF#))"
  ""
  "{-# INLINE startsWith0# #-}"
  "startsWith0# :: Int# -> Bool"
  "startsWith0# x = isTrue# ((x `andI#` 0x80#) ==# 0#)"
  ""
  "{-# INLINE startsWith10# #-}"
  "startsWith10# :: Int# -> Bool"
  "startsWith10# x = isTrue# ((x `andI#` 0xC0#) ==# 0x80#)"
  ""
  "{-# INLINE startsWith110# #-}"
  "startsWith110# :: Int# -> Bool"
  "startsWith110# x = isTrue# ((x `andI#` 0xE0#) ==# 0xC0#)"
  ""
  "{-# INLINE startsWith1110# #-}"
  "startsWith1110# :: Int# -> Bool"
  "startsWith1110# x = isTrue# ((x `andI#` 0xF0#) ==# 0xE0#)"
  ""
  "{-# INLINE startsWith11110# #-}"
  "startsWith11110# :: Int# -> Bool"
  "startsWith11110# x = isTrue# ((x `andI#` 0xF8#) ==# 0xF0#)"
  ""
  "{-# INLINE utf8SizeChar# #-}"
  "utf8SizeChar# :: Addr# -> Int#"
  "utf8SizeChar# a# ="
  "    case word8ToWord# (indexWord8OffAddr# a# 0#) of"
  "        0## -> 0#"
  "        x#  ->"
  "            let !ch0 = word2Int# x# in"
  "            if  | startsWith0# ch0     -> 1#"
  "                | startsWith110# ch0   -> 2#"
  "                | startsWith1110# ch0  -> 3#"
  "                | startsWith11110# ch0 -> 4#"
  "                | otherwise            -> 1#"
  "_||_")
 :expected-value
 (tests-utils--multiline
  "module Haskell.Language.LexerSimple.Types"
  "  ( AlexState(..)"
  "  , mkAlexState"
  "  , alexEnterBirdLiterateEnv"
  "  , alexEnterLiterateLatexEnv"
  "  , alexExitLiterateEnv"
  "  , pushContext"
  "  , modifyCommentDepth"
  "  , modifyQuasiquoterDepth"
  "  , modifyPreprocessorDepth"
  "  , addIndentationSize"
  "  , checkQuasiQuoteEndPresent"
  ""
  "  , AlexM"
  "  , runAlexM"
  "  , alexSetInput"
  "  , alexSetNextCode"
  ""
  "  , AlexInput(..)"
  "  , aiLineL"
  "  , takeText"
  "  , countInputSpace"
  "  , extractDefineOrLetName"
  "  , dropUntilNL"
  "  , dropUntilUnescapedNL"
  "  , dropUntilNLOr"
  "  , dropUntilNLOrEither"
  "  , unsafeTextHeadAscii"
  "  , unsafeTextHeadOfTailAscii"
  "  , unsafeTextHead"
  "  , utf8BS"
  ""
  "  , asCodeL"
  "  , asCommentDepthL"
  "  , asQuasiquoterDepthL"
  "  , asIndentationSizeL"
  "  , asPreprocessorDepthL"
  "  , asLiterateLocL"
  "  , asHaveQQEndL"
  ""
  "    -- * Alex interface"
  "  , alexInputPrevChar"
  "  , alexGetByte"
  "  ) where"
  ""
  "import Control.DeepSeq"
  "import Control.Exception"
  "import Control.Monad.ST"
  "import Control.Monad.State.Strict"
  "import Control.Monad.Writer.Strict"
  ""
  "import Data.ByteString qualified as BS"
  "import Data.ByteString.Char8 qualified as C8"
  "import Data.ByteString.Internal qualified as BSI"
  "import Data.Char"
  "import Data.Int"
  "import Data.Text qualified as T"
  "import Data.Text.Encoding qualified as TE"
  "import Data.Vector.Unboxed qualified as U"
  "import Data.Vector.Unboxed.Mutable qualified as UM"
  "import Foreign.ForeignPtr"
  "import Foreign.Ptr"
  "import GHC.Base"
  "import GHC.Ptr"
  "import GHC.Word"
  "import Text.Printf"
  ""
  "import Haskell.Language.Lexer.Types (Context(..), AlexCode(..), LitMode(..), LitStyle(..), SrcPos(..), ServerToken(..), Pos(..), Line(..), increaseLine, Offset(..))"
  "import Haskell.Language.LexerSimple.LensBlaze"
  ""
  "data AlexState = AlexState"
  "  { asInput        :: {-# UNPACK #-} !AlexInput"
  "  , asIntStore     :: {-# UNPACK #-} !Word64"
  "    -- ^ Integer field that stores all the other useful fields for lexing."
  "  , asContextStack :: [Context]"
  "  } deriving (Show, Eq, Ord)"
  ""
  "{-# INLINE asIntStoreL #-}"
  "asIntStoreL :: Lens' AlexState Word64"
  "asIntStoreL = lens asIntStore (\\b s -> s { asIntStore = b })"
  ""
  "{-# INLINE maybeBoolToInt #-}"
  "-- | Encode 'Maybe Bool' as bit mask to store it within integer store."
  "maybeBoolToInt :: Maybe Bool -> Int"
  "maybeBoolToInt = \\case"
  "  Nothing    -> 0"
  "  Just False -> 1"
  "  Just True  -> 2"
  ""
  "{-# INLINE intToMaybeBool #-}"
  "-- | Decode 'Maybe Bool' from bit mask stored within integer store."
  "intToMaybeBool :: Int -> Maybe Bool"
  "intToMaybeBool = \\case"
  "  0 -> Nothing"
  "  1 -> Just False"
  "  2 -> Just True"
  "  x -> error $ \"Invalid integer representation of 'Maybe Bool': \" ++ show x"
  ""
  "{-# INLINE asCodeL              #-}"
  "{-# INLINE asCommentDepthL      #-}"
  "{-# INLINE asQuasiquoterDepthL  #-}"
  "{-# INLINE asIndentationSizeL   #-}"
  "{-# INLINE asPreprocessorDepthL #-}"
  "{-# INLINE asLiterateLocL       #-}"
  "{-# INLINE asHaveQQEndL         #-}"
  "-- | Current Alex state the lexer is in. E.g. comments, string, TH quasiquoter"
  "     -- or vanilla toplevel mode."
  "asCodeL :: Lens' AlexState AlexCode"
  "asCommentDepthL, asQuasiquoterDepthL, asIndentationSizeL :: Lens' AlexState Int16"
  "-- | How many directives deep are we."
  "asPreprocessorDepthL :: Lens' AlexState Int16"
  "-- | Whether we're in bird-style or latex-style literate environment"
  "asLiterateLocL :: Lens' AlexState (LitMode LitStyle)"
  "asHaveQQEndL   :: Lens' AlexState (Maybe Bool)"
  "asCodeL              = asIntStoreL . intL 0  0x000f"
  "asCommentDepthL      = asIntStoreL . intL 4  0x03ff"
  "asQuasiquoterDepthL  = asIntStoreL . intL 14 0x03ff"
  "asIndentationSizeL   = asIntStoreL . int16L  24"
  "asPreprocessorDepthL = asIntStoreL . int16L  40"
  "asLiterateLocL       = \\f -> asIntStoreL (intL 56 0x0003 (fmap litLocToInt    . f . intToLitLoc))"
  "asHaveQQEndL         = \\f -> asIntStoreL (intL 58 0x0003 (fmap maybeBoolToInt . f . intToMaybeBool))"
  ""
  "{-# INLINE litLocToInt #-}"
  "litLocToInt :: LitMode LitStyle -> Int"
  "litLocToInt = \\case"
  "  LitVanilla      -> 0"
  "  LitOutside      -> 1"
  "  LitInside Bird  -> 2"
  "  LitInside Latex -> 3"
  ""
  "{-# INLINE intToLitLoc #-}"
  "intToLitLoc :: Int -> LitMode LitStyle"
  "intToLitLoc = \\case"
  "  0 -> LitVanilla"
  "  1 -> LitOutside"
  "  2 -> LitInside Bird"
  "  3 -> LitInside Latex"
  "  x -> error $ \"Invalid literate location representation: \" ++ show x"
  ""
  "mkAlexState :: LitMode Void -> AlexCode -> AlexInput -> AlexState"
  "mkAlexState litLoc startCode input ="
  "  set asCodeL startCode $"
  "    set asLiterateLocL (vacuous litLoc) AlexState"
  "      { asInput        = input"
  "      , asIntStore     = 0"
  "      , asContextStack = []"
  "      }"
  ""
  "{-# INLINE alexEnterBirdLiterateEnv #-}"
  "alexEnterBirdLiterateEnv :: MonadState AlexState m => m ()"
  "alexEnterBirdLiterateEnv ="
  "  modify $ set asLiterateLocL (LitInside Bird)"
  ""
  "{-# INLINE alexEnterLiterateLatexEnv #-}"
  "alexEnterLiterateLatexEnv :: MonadState AlexState m => m ()"
  "alexEnterLiterateLatexEnv ="
  "  modify $ set asLiterateLocL (LitInside Latex)"
  ""
  "{-# INLINE alexExitLiterateEnv #-}"
  "alexExitLiterateEnv :: MonadState AlexState m => m ()"
  "alexExitLiterateEnv ="
  "  modify $ set asLiterateLocL LitOutside"
  ""
  "{-# INLINE pushContext #-}"
  "pushContext :: MonadState AlexState m => Context -> m ()"
  "pushContext ctx ="
  "  modify (\\s -> s { asContextStack = ctx : asContextStack s })"
  ""
  "{-# INLINE modifyCommentDepth #-}"
  "modifyCommentDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16"
  "modifyCommentDepth f = do"
  "  depth <- gets (view asCommentDepthL)"
  "  let !depth' = f depth"
  "  modify $ \\s -> set asCommentDepthL depth' s"
  "  return depth'"
  ""
  "{-# INLINE modifyQuasiquoterDepth #-}"
  "modifyQuasiquoterDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16"
  "modifyQuasiquoterDepth f = do"
  "  depth <- gets (view asQuasiquoterDepthL)"
  "  let !depth' = f depth"
  "  modify $ \\s -> set asQuasiquoterDepthL depth' s"
  "  return depth'"
  ""
  "{-# INLINE modifyPreprocessorDepth #-}"
  "modifyPreprocessorDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16"
  "modifyPreprocessorDepth f = do"
  "  depth <- gets (view asPreprocessorDepthL)"
  "  let !depth' = f depth"
  "  modify $ \\s -> set asPreprocessorDepthL depth' s"
  "  return depth'"
  ""
  "{-# INLINE alexSetInput #-}"
  "alexSetInput :: MonadState AlexState m => AlexInput -> m ()"
  "alexSetInput input = modify $ \\s -> s { asInput = input }"
  ""
  "{-# INLINE alexSetNextCode #-}"
  "alexSetNextCode :: MonadState AlexState m => AlexCode -> m ()"
  "alexSetNextCode code = modify $ set asCodeL code"
  ""
  "{-# INLINE addIndentationSize #-}"
  "addIndentationSize :: MonadState AlexState m => Int16 -> m ()"
  "addIndentationSize x ="
  "  modify (over asIndentationSizeL (+ x))"
  ""
  "data QQEndsState = QQEndsState"
  "  { qqessPresent  :: Int#"
  "  , qqessPrevChar :: Char#"
  "  }"
  ""
  "checkQuasiQuoteEndPresent :: Ptr Word8 -> Bool"
  "checkQuasiQuoteEndPresent"
  "  = (\\x -> isTrue# (qqessPresent x))"
  "  . utf8Foldl' combine (QQEndsState 0# '\\n'#)"
  "  where"
  "    combine :: QQEndsState -> Char# -> QQEndsState"
  "    combine QQEndsState{qqessPresent, qqessPrevChar} c# = QQEndsState"
  "      { qqessPresent      ="
  "          qqessPresent `orI#`"
  "            case (# qqessPrevChar, c# #) of"
  "              (# '|'#, ']'# #) -> 1#"
  "              (# _,    '⟧'# #) -> 1#"
  "              _                -> 0#"
  "      , qqessPrevChar = c#"
  "      }"
  ""
  "type AlexM = WriterT [(AlexInput, ServerToken)] (State AlexState)"
  ""
  "{-# INLINE runAlexM #-}"
  "runAlexM"
  "  :: Bool"
  "  -> LitMode Void"
  "  -> AlexCode"
  "  -> C8.ByteString"
  "  -> AlexM a"
  "  -> (a, [Pos ServerToken])"
  "runAlexM trackPrefixesAndOffsets litLoc startCode input action ="
  "  performIO $"
  "    withAlexInput input $ \\input' inputSize -> do"
  "      let (a, xs) = evalState (runWriterT action)"
  "                  $ mkAlexState litLoc startCode input'"
  "      if trackPrefixesAndOffsets"
  "      then do"
  "        let !ptr  = aiPtr input' `plusPtr` 1 -- Drop first newline"
  "            !size = inputSize - 1"
  "            !idx  = positionsIndex ptr size"
  "            res   ="
  "              map (\\(x, y) -> Pos (mkSrcPos idx ptr x) y) xs"
  "        pure $! res `deepseq` (a, res)"
  "      else do"
  "        -- Contents of 'xs' has been seq'ed so TokenVals in there should"
  "        -- have been forced and thus should not contain any references to the"
  "        -- original input bytestring. However, in GHC 9.0 it seems that"
  "        -- GHC does some transformation which results in some entries within 'xs'"
  "        -- being not fully evaluated and thus lead to an error since they get"
  "        -- forced outside of 'withForeignPtr' bounds. The call to 'evaluate' below"
  "        -- is intended to prevent such transformation from occuring."
  "        _ <- evaluate xs"
  "        pure (a, map (\\(x, y) -> Pos (mkSrcPosNoPrefix x) y) xs)"
  ""
  "mkSrcPosNoPrefix :: AlexInput -> SrcPos"
  "mkSrcPosNoPrefix input ="
  "  SrcPos { posLine   = view aiLineL input"
  "         , posOffset = Offset 0"
  "         , posPrefix = mempty"
  "         , posSuffix = mempty"
  "         }"
  ""
  "mkSrcPos :: U.Vector Int -> Ptr Word8 -> AlexInput -> SrcPos"
  "mkSrcPos bytesToCharsMap start (input@AlexInput {aiPtr}) ="
  "  SrcPos { posLine = view aiLineL input"
  "         , posOffset"
  "         , posPrefix"
  "         , posSuffix"
  "         }"
  "  where"
  "    lineLen   = view aiLineLengthL input"
  "    posPrefix = TE.decodeUtf8 $ bytesToUtf8BS lineLen $ plusPtr aiPtr $ negate lineLen"
  "    posSuffix = TE.decodeUtf8 $ regionToUtf8BS aiPtr $ dropUntilNL# aiPtr"
  "    posOffset = Offset $ U.unsafeIndex bytesToCharsMap $ minusPtr aiPtr start"
  ""
  ""
  "-- Vector mapping absolute offsets off a pointer into how many utf8 characters"
  "   -- were encoded since the pointer start."
  "positionsIndex :: Ptr Word8 -> Int -> U.Vector Int"
  "positionsIndex (Ptr start#) len ="
  "  U.create $ do"
  "    (vec :: UM.MVector s Int) <- UM.new len"
  "    let assignAfter :: Int -> Int -> Int -> ST s ()"
  "        assignAfter start n item = go' n start"
  "          where"
  "            go' :: Int -> Int -> ST s ()"
  "            go' 0 !i = UM.unsafeWrite vec i item"
  "            go' k !i = UM.unsafeWrite vec i item *> go' (k - 1) (i + 1)"
  "        go :: Int# -> Int -> ST s ()"
  "        go bytes# !nChars ="
  "          case utf8SizeChar# (start# `plusAddr#` bytes#) of"
  "            0#      -> pure ()"
  "            nBytes# -> do"
  "              assignAfter (I# bytes#) (I# nBytes#) nChars"
  "              go (bytes# +# nBytes#) $ nChars + 1"
  "    go 0# 0"
  "    pure vec"
  ""
  ""
  "data AlexInput = AlexInput"
  "  { aiPtr      :: {-# UNPACK #-} !(Ptr Word8)"
  "  , aiIntStore :: {-# UNPACK #-} !Word64"
  "    -- ^ Integer field that stores all the other useful fields for lexing."
  "  } deriving (Eq, Ord)"
  ""
  "instance Show AlexInput where"
  "  show AlexInput{aiPtr, aiIntStore} ="
  "    printf \"AlexInput 0x%08x 0x%08x\" ptr aiIntStore"
  "    where"
  "      ptr :: Word"
  "      ptr = fromIntegral $ ptrToWordPtr aiPtr"
  ""
  "{-# INLINE aiIntStoreL #-}"
  "aiIntStoreL :: Lens' AlexInput Word64"
  "aiIntStoreL = lens aiIntStore (\\b s -> s { aiIntStore = b })"
  ""
  "lineInt32L :: Lens' Int32 Line"
  "lineInt32L = lens (Line . fromIntegral) (\\(Line x) _ -> fromIntegral x)"
  ""
  "int2Int32L :: Lens' Int32 Int"
  "int2Int32L = lens fromIntegral (\\x _ -> fromIntegral x)"
  ""
  "{-# INLINE aiLineL       #-}"
  "{-# INLINE aiLineLengthL #-}"
  "-- | Current line in input stream."
  "aiLineL       :: Lens' AlexInput Line"
  "-- | Length of current line."
  "aiLineLengthL :: Lens' AlexInput Int"
  ""
  "aiLineL       = aiIntStoreL . int32L 0  . lineInt32L"
  "aiLineLengthL = aiIntStoreL . int32L 32 . int2Int32L"
  ""
  "{-# INLINE takeText #-}"
  "takeText :: AlexInput -> Int -> T.Text"
  "takeText AlexInput{aiPtr} len ="
  "  TE.decodeUtf8 $! utf8BS len aiPtr"
  ""
  "countInputSpace :: AlexInput -> Int -> Int"
  "countInputSpace AlexInput{aiPtr} len ="
  "  utf8FoldlBounded len inc 0 aiPtr"
  "  where"
  "    inc acc ' '#  = acc + 1"
  "    inc acc '\\t'# = acc + 8"
  "    inc acc c#    = case fixChar c# of"
  "      1## -> acc + 1"
  "      _   -> acc"
  ""
  "{-# INLINE performIO #-}"
  "performIO :: IO a -> a"
  "performIO = BSI.accursedUnutterablePerformIO"
  ""
  "{-# INLINE withAlexInput #-}"
  "withAlexInput :: C8.ByteString -> (AlexInput -> Int -> IO a) -> IO a"
  "withAlexInput s f ="
  "  case s' of"
  "    BSI.PS ptr offset len ->"
  "      withForeignPtr ptr $ \\ptr' -> do"
  "        let !input ="
  "             set aiLineL initLine $"
  "               AlexInput"
  "                 { aiPtr      = ptr' `plusPtr` offset"
  "                 , aiIntStore = 0"
  "                 }"
  "        f input $! len - offset"
  "  where"
  "    -- Line numbering starts from 0 because we're adding additional newline"
  "    -- at the beginning to simplify processing. Thus, line numbers in the"
  "    -- result are 1-based."
  "    initLine = Line 0"
  ""
  "    -- Add '\\0' at the end so that we'll find the end of stream (just"
  "    -- as in the old C days...)"
  "    s' = C8.cons '\\n' $ C8.snoc (C8.snoc (stripBOM s) '\\n') '\\0'"
  "    stripBOM :: C8.ByteString -> C8.ByteString"
  "    stripBOM xs"
  "        | \"\\xEF\\xBB\\xBF\" `C8.isPrefixOf` xs"
  "        = C8.drop 3 xs"
  "        | otherwise"
  "        = xs"
  ""
  "{-# INLINE extractDefineOrLetName #-}"
  "extractDefineOrLetName :: AlexInput -> Int -> T.Text"
  "extractDefineOrLetName AlexInput{aiPtr} n ="
  "  TE.decodeUtf8 $ regionToUtf8BS (Ptr start#) end"
  "  where"
  "    !end        = aiPtr `plusPtr` n"
  "    !(Ptr end#) = end"
  "    start#      = (goBack# (end# `plusAddr#` -1#)) `plusAddr#` 1#"
  ""
  "    goBack# :: Addr# -> Addr#"
  "    goBack# ptr# = case word8ToWord# (indexWord8OffAddr# ptr# 0#) of"
  "      0##  -> ptr#"
  "      9##  -> ptr# -- '\\n'"
  "      10## -> ptr# -- '\\n'"
  "      13## -> ptr# -- '\\r'"
  "      32## -> ptr# -- ' '"
  "      92## -> ptr# -- '\\\\'"
  "      _    -> goBack# (ptr# `plusAddr#` -1#)"
  ""
  "{-# INLINE dropUntilNL #-}"
  "dropUntilNL :: AlexInput -> AlexInput"
  "dropUntilNL input@AlexInput{aiPtr} ="
  "  input { aiPtr = dropUntilNL# aiPtr }"
  ""
  "{-# INLINE dropUntilUnescapedNL #-}"
  "dropUntilUnescapedNL :: AlexInput -> AlexInput"
  "dropUntilUnescapedNL input@AlexInput{aiPtr = start} ="
  "  case dropUntilUnescapedNL# start of"
  "    (# seenNewlines, end #) ->"
  "      over aiLineL (\\(Line n) -> Line (n + seenNewlines)) $"
  "        input { aiPtr = end }"
  ""
  "{-# INLINE dropUntilNLOr #-}"
  "dropUntilNLOr :: Word8 -> AlexInput -> AlexInput"
  "dropUntilNLOr w input@AlexInput{aiPtr} ="
  "  input { aiPtr = dropUntilNLOr# w aiPtr }"
  ""
  "{-# INLINE dropUntilNLOrEither #-}"
  "-- | Drop until either of two bytes."
  "dropUntilNLOrEither :: Word8 -> Word8 -> AlexInput -> AlexInput"
  "dropUntilNLOrEither w1 w2 input@AlexInput{aiPtr} ="
  "  input { aiPtr = dropUntilNLOrEither# w1 w2 aiPtr }"
  ""
  "-- Alex interface"
  ""
  "{-# INLINE alexInputPrevChar #-}"
  "alexInputPrevChar :: AlexInput -> Char"
  "alexInputPrevChar AlexInput{ aiPtr = Ptr ptr# } ="
  "  case base# `minusAddr#` start# of"
  "    0# -> C# (chr# ch0)"
  "    1# -> let !(# x, _ #) = readChar1# start# ch0 in C# x"
  "    2# -> let !(# x, _ #) = readChar2# start# ch0 in C# x"
  "    3# -> let !(# x, _ #) = readChar3# start# ch0 in C# x"
  "    _  -> '\\0' -- Invalid!"
  "  where"
  "    ch0 :: Int#"
  "    !ch0 = word2Int# (word8ToWord# (indexWord8OffAddr# start# 0#))"
  ""
  "    base# = findCharStart ptr# `plusAddr#` -1#"
  ""
  "    start# = findCharStart base#"
  ""
  "    findCharStart :: Addr# -> Addr#"
  "    findCharStart p#"
  "        | startsWith10# w#"
  "        = findCharStart (p# `plusAddr#` -1#)"
  "        | otherwise"
  "        = p#"
  "      where"
  "        w# = word2Int# (word8ToWord# (indexWord8OffAddr# p# 0#))"
  ""
  "{-# INLINE alexGetByte #-}"
  "alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)"
  "alexGetByte input@AlexInput{aiPtr} ="
  "  case nextChar aiPtr of"
  "    (# c#, n, cs #) ->"
  "      case fixChar c# of"
  "        0##  -> Nothing -- Abort on an unknown character"
  "        -- '\\n'"
  "        10## -> Just (10, input')"
  "          where"
  "            !input' ="
  "              over aiLineL increaseLine $"
  "                set aiLineLengthL 0 $"
  "                  input { aiPtr = cs }"
  "        c    -> Just (b, input')"
  "          where"
  "            !b     = W8# (wordToWord8# c)"
  "            !input' ="
  "              over aiLineLengthL (+ I# n) $"
  "                input { aiPtr = cs }"
  ""
  "-- Translate unicode character into special symbol we teached Alex to recognize."
  "{-# INLINE fixChar #-}"
  "fixChar :: Char# -> Word#"
  "fixChar = \\case"
  "  -- These should not be translated since Alex knows about them"
  "  '→'#    -> reservedSym"
  "  '∷'#    -> reservedSym"
  "  '⇒'#    -> reservedSym"
  "  '∀'#    -> reservedSym"
  "  '⦇'#    -> reservedSym"
  "  '⦈'#    -> reservedSym"
  "  '⟦'#    -> reservedSym"
  "  '⟧'#    -> reservedSym"
  "  '\\x00'# -> fullStop"
  "  '\\x01'# -> fullStop"
  "  '\\x02'# -> fullStop"
  "  '\\x03'# -> fullStop"
  "  '\\x04'# -> fullStop"
  "  '\\x05'# -> fullStop"
  "  '\\x06'# -> fullStop"
  "  '\\x07'# -> fullStop"
  "  '\\x08'# -> other"
  "  c# -> case ord# c# of"
  "    c2# | isTrue# (c2# <=# 0x7f#) ->"
  "          int2Word# c2# -- Plain ascii needs no fixing."
  "            | otherwise   ->"
  "              case generalCategory (C# c#) of"
  "                UppercaseLetter      -> upper"
  "                LowercaseLetter      -> lower"
  "                TitlecaseLetter      -> upper"
  "                ModifierLetter       -> suffix"
  "                OtherLetter          -> lower"
  "                NonSpacingMark       -> suffix"
  "                DecimalNumber        -> digit"
  "                OtherNumber          -> digit"
  "                Space                -> space"
  "                ConnectorPunctuation -> symbol"
  "                DashPunctuation      -> symbol"
  "                OtherPunctuation     -> symbol"
  "                MathSymbol           -> symbol"
  "                CurrencySymbol       -> symbol"
  "                ModifierSymbol       -> symbol"
  "                OtherSymbol          -> symbol"
  ""
  "                SpacingCombiningMark -> space"
  "                EnclosingMark        -> other"
  "                LetterNumber         -> symbol"
  "                OpenPunctuation      -> symbol"
  "                ClosePunctuation     -> symbol"
  "                InitialQuote         -> symbol"
  "                FinalQuote           -> symbol"
  "                LineSeparator        -> space"
  "                ParagraphSeparator   -> space"
  "                Control              -> other"
  "                Format               -> other"
  "                Surrogate            -> other"
  "                PrivateUse           -> other"
  "                NotAssigned          -> other"
  "  where"
  "    fullStop, space, upper, lower, symbol :: Word#"
  "    digit, suffix, reservedSym, other :: Word#"
  "    fullStop    = 0x00## -- Don't care about these"
  "    space       = 0x01##"
  "    upper       = 0x02##"
  "    lower       = 0x03##"
  "    symbol      = 0x04##"
  "    digit       = 0x05##"
  "    suffix      = 0x06##"
  "    reservedSym = 0x07##"
  "    other       = 0x08##"
  ""
  "{-# INLINE unsafeTextHeadAscii #-}"
  "unsafeTextHeadAscii :: Ptr Word8 -> Word8"
  "unsafeTextHeadAscii (Ptr ptr#) = W8# (indexWord8OffAddr# ptr# 0#)"
  ""
  "{-# INLINE unsafeTextHeadOfTailAscii #-}"
  "unsafeTextHeadOfTailAscii :: Ptr Word8 -> Word8"
  "unsafeTextHeadOfTailAscii (Ptr ptr#) = W8# (indexWord8OffAddr# ptr# 1#)"
  ""
  "{-# INLINE unsafeTextHead #-}"
  "unsafeTextHead :: Ptr Word8 -> Char"
  "unsafeTextHead x ="
  "  case nextChar x of"
  "    (# c#, _, _ #) -> C# c#"
  ""
  "{-# INLINE nextChar #-}"
  "nextChar :: Ptr Word8 -> (# Char#, Int#, Ptr Word8 #)"
  "nextChar (Ptr ptr#) ="
  "  case utf8DecodeChar# ptr# of"
  "    (# c#, nBytes# #) -> (# c#, nBytes#, Ptr (ptr# `plusAddr#` nBytes#) #)"
  ""
  "{-# INLINE dropUntilNL# #-}"
  "dropUntilNL# :: Ptr Word8 -> Ptr Word8"
  "dropUntilNL# (Ptr start#) = Ptr (go start#)"
  "  where"
  "    go :: Addr# -> Addr#"
  "    go ptr# = case word8ToWord# (indexWord8OffAddr# ptr# 0#) of"
  "      0##  -> ptr#"
  "      10## -> ptr# -- '\\n'"
  "      _    -> go (ptr# `plusAddr#` 1#)"
  ""
  "{-# INLINE dropUntilUnescapedNL# #-}"
  "dropUntilUnescapedNL# :: Ptr Word8 -> (# Int, Ptr Word8 #)"
  "dropUntilUnescapedNL# (Ptr start#) = go 0 start#"
  "  where"
  "    go :: Int -> Addr# -> (# Int, Ptr Word8 #)"
  "    go !n ptr# = case word8ToWord# (indexWord8OffAddr# ptr# 0#) of"
  "      0##  -> (# n, Ptr ptr# #)"
  "      -- '\\n'"
  "      10## -> (# n, Ptr ptr# #)"
  "      -- '\\\\'"
  "      92## ->"
  "        case word8ToWord# (indexWord8OffAddr# ptr# 1#) of"
  "          0##  -> (# n, Ptr (ptr# `plusAddr#` 1#) #)"
  "          -- '\\n'"
  "          10## -> go (n + 1) (ptr# `plusAddr#` 2#)"
  "          _    -> go n (ptr# `plusAddr#` 2#)"
  "      _    -> go n (ptr# `plusAddr#` 1#)"
  ""
  "{-# INLINE dropUntilNLOr# #-}"
  "dropUntilNLOr# :: Word8 -> Ptr Word8 -> Ptr Word8"
  "dropUntilNLOr# (W8# w#) (Ptr start#) = Ptr (go start#)"
  "  where"
  "    go :: Addr# -> Addr#"
  "    go ptr# = case word8ToWord# (indexWord8OffAddr# ptr# 0#) of"
  "      0##  -> ptr#"
  "      -- '\\n'"
  "      10## -> ptr#"
  "      c# | isTrue# (c# `eqWord#` word8ToWord# w#) -> ptr#"
  "           | otherwise                 -> go (ptr# `plusAddr#` 1#)"
  ""
  "{-# INLINE dropUntilNLOrEither# #-}"
  "dropUntilNLOrEither# :: Word8 -> Word8 -> Ptr Word8 -> Ptr Word8"
  "dropUntilNLOrEither# (W8# w1#) (W8# w2#) (Ptr start#) = Ptr (go start#)"
  "  where"
  "    go :: Addr# -> Addr#"
  "    go ptr# = case word8ToWord# (indexWord8OffAddr# ptr# 0#) of"
  "      0##  -> ptr#"
  "      -- '\\n'"
  "      10## -> ptr#"
  "      c# | isTrue# ((c# `eqWord#` word8ToWord# w1#) `orI#` (c# `eqWord#` word8ToWord# w2#))"
  "         -> ptr#"
  "           | otherwise"
  "           -> go (ptr# `plusAddr#` 1#)"
  ""
  "{-# INLINE utf8Foldl' #-}"
  "utf8Foldl' :: forall a. (a -> Char# -> a) -> a -> Ptr Word8 -> a"
  "utf8Foldl' f x0 (Ptr ptr#) ="
  "  go x0 ptr#"
  "  where"
  "    go :: a -> Addr# -> a"
  "    go !acc addr# ="
  "      case utf8DecodeChar# addr# of"
  "        (# _,  0#      #) -> acc"
  "        (# c#, nBytes# #) -> go (acc `f` c#) (addr# `plusAddr#` nBytes#)"
  ""
  "{-# INLINE utf8FoldlBounded #-}"
  "utf8FoldlBounded :: forall a. Int -> (a -> Char# -> a) -> a -> Ptr Word8 -> a"
  "utf8FoldlBounded (I# len#) f x0 (Ptr ptr#) ="
  "  go len# x0 ptr#"
  "  where"
  "    go :: Int#-> a -> Addr# -> a"
  "    go 0# !acc _     = acc"
  "    go n# !acc addr# ="
  "      case utf8DecodeChar# addr# of"
  "        (# _,  0#      #) -> acc"
  "        (# c#, nBytes# #) ->"
  "          go (n# -# 1#) (acc `f` c#) (addr# `plusAddr#` nBytes#)"
  ""
  "{-# INLINE utf8BS #-}"
  "utf8BS :: Int -> Ptr Word8 -> BS.ByteString"
  "utf8BS (I# nChars#) (Ptr start#) ="
  "  BSI.PS (performIO (newForeignPtr_ (Ptr start#))) 0 (I# (go nChars# 0#))"
  "  where"
  "    go :: Int# -> Int# -> Int#"
  "    go 0# bytes# = bytes#"
  "    go k# bytes# ="
  "      case utf8SizeChar# (start# `plusAddr#` bytes#)  of"
  "        0#      -> bytes#"
  "        nBytes# -> go (k# -# 1#) (bytes# +# nBytes#)"
  ""
  "{-# INLINE bytesToUtf8BS #-}"
  "bytesToUtf8BS :: Int -> Ptr Word8 -> BS.ByteString"
  "bytesToUtf8BS (I# nbytes#) (Ptr start#) ="
  "  BSI.PS (performIO (newForeignPtr_ (Ptr start#))) 0 (I# nbytes#)"
  ""
  "{-# INLINE regionToUtf8BS #-}"
  "regionToUtf8BS :: Ptr Word8 -> Ptr Word8 -> BS.ByteString"
  "regionToUtf8BS start end ="
  "  BSI.PS (performIO (newForeignPtr_ start)) 0 (minusPtr end start)"
  ""
  "{-# INLINE utf8DecodeChar# #-}"
  "utf8DecodeChar# :: Addr# -> (# Char#, Int# #)"
  "utf8DecodeChar# a# ="
  "  case word8ToWord# (indexWord8OffAddr# a# 0#) of"
  "    0## -> (# '\\0'#, 0# #)"
  "    x#  ->"
  "      let !ch0 = word2Int# x# in"
  "      if  | startsWith0# ch0     -> (# chr# ch0, 1# #)"
  "          | startsWith110# ch0   -> readChar1# a# ch0"
  "          | startsWith1110# ch0  -> readChar2# a# ch0"
  "          | startsWith11110# ch0 -> readChar3# a# ch0"
  "          | otherwise            -> invalid# 1#"
  ""
  "-- all invalid# sequences end up here:"
  "{-# INLINE invalid# #-}"
  "invalid# :: Int# -> (# Char#, Int# #)"
  "invalid# nBytes# = (# '\\8'#, nBytes# #)"
  "-- TODO: check whether following note from ghc applies to server's lexer:"
  "   -- '\\xFFFD' would be the usual replacement character, but"
  "      -- that's a valid symbol in Haskell, so will result in a"
  "         -- confusing parse error later on.  Instead we use '\\0' which"
  "            -- will signal a lexer error immediately."
  ""
  "{-# INLINE readChar1# #-}"
  "readChar1# :: Addr# -> Int# -> (# Char#, Int# #)"
  "readChar1# a# ch0 ="
  "  let !ch1 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 1#)) in"
  "  if noValidUtf8Cont# ch1 then invalid# 1# else"
  "    (# chr# (((ch0 `andI#` 0x3F#) `uncheckedIShiftL#` 6#) `orI#`"
  "              (ch1 `andI#` 0x7F#)),"
  "      2# #)"
  ""
  "{-# INLINE readChar2# #-}"
  "readChar2# :: Addr# -> Int# -> (# Char#, Int# #)"
  "readChar2# a# ch0 ="
  "  let !ch1 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 1#)) in"
  "  if noValidUtf8Cont# ch1 then invalid# 1# else"
  "    let !ch2 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 2#)) in"
  "    if noValidUtf8Cont# ch2 then invalid# 2# else"
  "      (# chr# (((ch0 `andI#` 0x1F#) `uncheckedIShiftL#` 12#) `orI#`"
  "                ((ch1 `andI#` 0x7F#) `uncheckedIShiftL#` 6#)  `orI#`"
  "                  (ch2 `andI#` 0x7F#)),"
  "        3# #)"
  ""
  "{-# INLINE readChar3# #-}"
  "readChar3# :: Addr# -> Int# -> (# Char#, Int# #)"
  "readChar3# a# ch0 ="
  "  let !ch1 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 1#)) in"
  "  if noValidUtf8Cont# ch1 then invalid# 1# else"
  "    let !ch2 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 2#)) in"
  "    if noValidUtf8Cont# ch2 then invalid# 2# else"
  "      let !ch3 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 3#)) in"
  "      if noValidUtf8Cont# ch3 then invalid# 3# else"
  "        (# chr# (((ch0 `andI#` 0x0F#) `uncheckedIShiftL#` 18#) `orI#`"
  "                  ((ch1 `andI#` 0x7F#) `uncheckedIShiftL#` 12#) `orI#`"
  "                    ((ch2 `andI#` 0x7F#) `uncheckedIShiftL#` 6#)  `orI#`"
  "                      (ch3 `andI#` 0x7F#)),"
  "          4# #)"
  ""
  "{-# INLINE noValidUtf8Cont# #-}"
  "noValidUtf8Cont# :: Int# -> Bool"
  "noValidUtf8Cont# x = isTrue# ((x <# 0x80#) `orI#` (x ># 0xBF#))"
  ""
  "{-# INLINE startsWith0# #-}"
  "startsWith0# :: Int# -> Bool"
  "startsWith0# x = isTrue# ((x `andI#` 0x80#) ==# 0#)"
  ""
  "{-# INLINE startsWith10# #-}"
  "startsWith10# :: Int# -> Bool"
  "startsWith10# x = isTrue# ((x `andI#` 0xC0#) ==# 0x80#)"
  ""
  "{-# INLINE startsWith110# #-}"
  "startsWith110# :: Int# -> Bool"
  "startsWith110# x = isTrue# ((x `andI#` 0xE0#) ==# 0xC0#)"
  ""
  "{-# INLINE startsWith1110# #-}"
  "startsWith1110# :: Int# -> Bool"
  "startsWith1110# x = isTrue# ((x `andI#` 0xF0#) ==# 0xE0#)"
  ""
  "{-# INLINE startsWith11110# #-}"
  "startsWith11110# :: Int# -> Bool"
  "startsWith11110# x = isTrue# ((x `andI#` 0xF8#) ==# 0xF0#)"
  ""
  "{-# INLINE utf8SizeChar# #-}"
  "utf8SizeChar# :: Addr# -> Int#"
  "utf8SizeChar# a# ="
  "  case word8ToWord# (indexWord8OffAddr# a# 0#) of"
  "    0## -> 0#"
  "    x#  ->"
  "      let !ch0 = word2Int# x# in"
  "      if  | startsWith0# ch0     -> 1#"
  "          | startsWith110# ch0   -> 2#"
  "          | startsWith1110# ch0  -> 3#"
  "          | startsWith11110# ch0 -> 4#"
  "          | otherwise            -> 1#"
  "_|_"))

(haskell-indentation-tests--test-treesitter-region
 :name haskell-indentation-tests--test-treesitter-region-3a
 :contents
 (tests-utils--multiline
  ""
  "foo arg"
  "  _|_( bar1"
  "    , bar2"
  "    , bar3"
  "    , bar4"
  "    , bar5"
  "  _||_)"
  "  = 1"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo arg"
  "  ( bar1"
  "  , bar2"
  "  , bar3"
  "  , bar4"
  "  , bar5"
  "  _|_)"
  "  = 1"
  ""))

(haskell-indentation-tests--test-treesitter-region
 :name haskell-indentation-tests--test-treesitter-region-3b
 :contents
 (tests-utils--multiline
  ""
  "foo arg"
  "  _||_( bar1"
  "    , bar2"
  "  , bar3"
  "    , bar4"
  "    , bar5"
  "  _|_)"
  "  = 1"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo arg"
  "  _|_( bar1"
  "  , bar2"
  "  , bar3"
  "  , bar4"
  "  , bar5"
  "  )"
  "  = 1"
  ""))

(haskell-indentation-tests--test-treesitter-region
 :name haskell-indentation-tests--test-treesitter-region-3c
 :contents
 (tests-utils--multiline
  ""
  "foo arg"
  "  _|_( bar1"
  "    , bar2"
  " , bar3"
  "    , bar4"
  "    , bar5"
  "  _||_)"
  "  = 1"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo arg"
  "  ( bar1"
  "  , bar2"
  "  , bar3"
  "  , bar4"
  "  , bar5"
  "  _|_)"
  "  = 1"
  ""))

(haskell-indentation-tests--test-treesitter-region
 :name haskell-indentation-tests--test-treesitter-region-3d
 :contents
 (tests-utils--multiline
  ""
  "foo arg"
  "  _||_( bar1"
  "    , bar2"
  "            , bar3"
  "    , bar4"
  "    , bar5"
  "  _|_)"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo arg"
  "  _|_( bar1"
  "  , bar2"
  "  , bar3"
  "  , bar4"
  "  , bar5"
  "  )"
  ""))

;; Test that indentation is confined to the selected region bounds.
(haskell-indentation-tests--test-treesitter-region
 :name haskell-indentation-tests--test-treesitter-region-4
 :contents
 (tests-utils--multiline
  ""
  "foo arg"
  "  _|_( bar1"
  "    , bar2"
  "    , bar3"
  "    , bar4"
  "    , bar5"
  "  _||_)"
  "  ( quux1"
  "    , quux2"
  "    , quux3"
  "    , quux4"
  "    , quux5"
  "    )"
  "  = bar1 + quux1"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo arg"
  "  ( bar1"
  "  , bar2"
  "  , bar3"
  "  , bar4"
  "  , bar5"
  "  _|_)"
  "  ( quux1"
  "    , quux2"
  "    , quux3"
  "    , quux4"
  "    , quux5"
  "    )"
  "  = bar1 + quux1"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-string-1a
 :contents
 (tests-utils--multiline
  "foo :: [String]"
  "foo ="
  "  [ \"bar\""
  "  , \"  {-   foo \\n\\"
  "   _|_\\\\n\\"
  "    \\-}\""
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo :: [String]"
  "foo ="
  "  [ \"bar\""
  "  , \"  {-   foo \\n\\"
  "    _|_\\\\n\\"
  "    \\-}\""
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-string-1b
 :contents
 (tests-utils--multiline
  "foo :: [String]"
  "foo ="
  "  [ bar :-> \"bar\""
  "  , foo :-> \"  {-   foo \\n\\"
  "   _|_\\\\n\\"
  "    \\-}\""
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo :: [String]"
  "foo ="
  "  [ bar :-> \"bar\""
  "  , foo :-> \"  {-   foo \\n\\"
  "    _|_\\\\n\\"
  "    \\-}\""
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-string-1c
 :contents
 (tests-utils--multiline
  "foo :: [String]"
  "foo ="
  "  [ bar :-> \"bar\""
  "  , foo :->"
  "     _|_\"  {-   foo \\n\\"
  "    \\\\n\\"
  "    \\-}\""
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo :: [String]"
  "foo ="
  "  [ bar :-> \"bar\""
  "  , foo :->"
  "      _|_\"  {-   foo \\n\\"
  "    \\\\n\\"
  "    \\-}\""
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-string-2a
 :contents
 (tests-utils--multiline
  "foo :: [String]"
  "foo ="
  "  [ \"bar\""
  "  , \"\"\""
  "    {-   foo"
  ""
  "    -}"
  "     _|_\"\"\""
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo :: [String]"
  "foo ="
  "  [ \"bar\""
  "  , \"\"\""
  "    {-   foo"
  ""
  "    -}"
  "    _|_\"\"\""
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-string-2b
 :contents
 (tests-utils--multiline
  "foo :: [String]"
  "foo ="
  "  [ \"bar\""
  "  , \"\"\""
  "     _|_{-   foo"
  ""
  "    -}"
  "    \"\"\""
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo :: [String]"
  "foo ="
  "  [ \"bar\""
  "  , \"\"\""
  "    _|_{-   foo"
  ""
  "    -}"
  "    \"\"\""
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-string-2c
 :contents
 (tests-utils--multiline
  "foo :: [String]"
  "foo ="
  "  [ \"bar\""
  "  , \"\"\""
  "    {-   foo"
  "_|_"
  "    -}"
  "    \"\"\""
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo :: [String]"
  "foo ="
  "  [ \"bar\""
  "  , \"\"\""
  "    {-   foo"
  "    _|_"
  "    -}"
  "    \"\"\""
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-string-2d
 :contents
 (tests-utils--multiline
  "foo :: [String]"
  "foo ="
  "  [ bar :-> \"bar\""
  "  , foo :-> \"\"\""
  "     _|_{-   foo"
  ""
  "    -}"
  "    \"\"\""
  "  ]")
 :expected-value
 (tests-utils--multiline
  "foo :: [String]"
  "foo ="
  "  [ bar :-> \"bar\""
  "  , foo :-> \"\"\""
  "    _|_{-   foo"
  ""
  "    -}"
  "    \"\"\""
  "  ]"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-string-2e
 :contents
 (tests-utils--multiline
  "foo :: (String, String)"
  "foo ="
  "  ( \"bar\""
  "  , \"\"\""
  "     _|_{-   foo"
  ""
  "    -}"
  "    \"\"\""
  "  )")
 :expected-value
 (tests-utils--multiline
  "foo :: (String, String)"
  "foo ="
  "  ( \"bar\""
  "  , \"\"\""
  "    _|_{-   foo"
  ""
  "    -}"
  "    \"\"\""
  "  )"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-string-2f
 :contents
 (tests-utils--multiline
  "foo :: (String, String)"
  "foo ="
  "  ( \"bar\""
  "  ,"
  "       _|_\"\"\""
  "    {-   foo"
  ""
  "    -}"
  "    \"\"\""
  "  )")
 :expected-value
 (tests-utils--multiline
  "foo :: (String, String)"
  "foo ="
  "  ( \"bar\""
  "  ,"
  "    _|_\"\"\""
  "    {-   foo"
  ""
  "    -}"
  "    \"\"\""
  "  )"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--quasi-quote-1a
 :contents
 (tests-utils--multiline
  ""
  "foo :: String"
  "foo ="
  "  [hereDoc|"
  "  foo"
  "  bar"
  "  baz"
  " _|_|]"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: String"
  "foo ="
  "  [hereDoc|"
  "  foo"
  "  bar"
  "  baz"
  "  _|_|]"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--quasi-quote-1b
 :contents
 (tests-utils--multiline
  ""
  "foo :: String"
  "foo ="
  "    _|_[hereDoc|"
  "  foo"
  "  bar"
  "  baz"
  "  |]"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: String"
  "foo ="
  "  _|_[hereDoc|"
  "  foo"
  "  bar"
  "  baz"
  "  |]"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--lambda-1a
 :contents
 (tests-utils--multiline
  ""
  "getDirectoryContentsWithFilterRecursive visitPred collectPred root ="
  "  listContentsRecFold'"
  "    Nothing"
  "    (\\_ _ (Relative path) basename ft symlink cons prependSubdir rest ->"
  "           _|_foo)"
  "    (Just root)"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "getDirectoryContentsWithFilterRecursive visitPred collectPred root ="
  "  listContentsRecFold'"
  "    Nothing"
  "    (\\_ _ (Relative path) basename ft symlink cons prependSubdir rest ->"
  "      _|_foo)"
  "    (Just root)"
  ""))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--lambda-1b
 :contents
 (tests-utils--multiline
  ""
  "getDirectoryContentsWithFilterRecursive visitPred collectPred root ="
  "  listContentsRecFold'"
  "    Nothing"
  "    (\\_ _ (Relative path) basename ft symlink cons prependSubdir rest ->"
  "           _|_if foo)"
  "    (Just root)"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "getDirectoryContentsWithFilterRecursive visitPred collectPred root ="
  "  listContentsRecFold'"
  "    Nothing"
  "    (\\_ _ (Relative path) basename ft symlink cons prependSubdir rest ->"
  "      _|_if foo)"
  "    (Just root)"
  "")
 ;; AST is incorrect so lambda construct is never fully parsed and thus cannot be indented.
 :expected-result :failed)

(provide 'haskell-indentation-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; haskell-tests.el ends here
