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
(require 'haskell-format-setup)
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
          expected-value)
  `(progn
     ,@(cl-loop
        for mode in '(haskell-ts-mode)
        collect
        `(ert-deftest ,(string->symbol (format "%s/%s" name mode)) ()
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
            ,(string->symbol (format "haskell-indentation-tests-%s" mode)))))))

(cl-defmacro haskell-indentation-tests--test-treesitter-region
    (&key name
          contents
          expected-value)
  `(progn
     ,@(cl-loop
        for mode in '(haskell-ts-mode)
        collect
        `(ert-deftest ,(string->symbol (format "%s/%s" name mode)) ()
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
                (if (re-search-forward "_||_" nil t)
                    (replace-match "")
                  (error "No _||_ marker for point position within contents:\n%s" ,contents))
                (when (save-excursion
                        (goto-char (point-min))
                        (re-search-forward "_||_" nil t))
                  (error "More than one occurrence of _||_ in source"))
                (setf end (point))
                (indent-region start end)))
            :contents ,contents
            :expected-value ,expected-value
            :initialisation (,mode)
            :suppress-cursor t
            :buffer-id
            ,(string->symbol (format "haskell-indentation-tests-%s" mode)))))))

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
 :name haskell-indentation-tests--test-treesitter-record-1
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
 :name haskell-indentation-tests--test-treesitter-field-update-1
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
  "          _|_foo bar baaz"
  "        }"
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
  "          _|_foo $ bar baaz"
  "        }"
  "  in bar"))

(haskell-indentation-tests--test-treesitter
 :name haskell-indentation-tests--test-treesitter-field-update-3
 :contents
 (tests-utils--multiline
  "foo x ="
  "  baz"
  "    { quux = bar"
  "        _|_{ frobnicate = baar $ baaz"
  "      }"
  "    }")
 :expected-value
 (tests-utils--multiline
  "foo x ="
  "  baz"
  "    { quux = bar"
  "      _|_{ frobnicate = baar $ baaz"
  "      }"
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

(haskell-indentation-tests--test-treesitter-region
 :name haskell-indentation-tests--test-treesitter-region-1
 :contents
 (tests-utils--multiline
  "foo = _|_["
  "  bar $ quux"
  "  , baz"
  " _||_]")
 :expected-value
 (tests-utils--multiline
  "_|_foo = ["
  "        bar $ quux"
  "      , baz"
  "      ]"))

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
  "  YYY :: a -> Foo2 a"))

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
  "bar1 _ _ = \"\"")
 )

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
  "bar1 _ _ = \"\"")
 )

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
  "bar1 _ _ = \"\"")
 )

(provide 'haskell-indentation-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; haskell-tests.el ends here