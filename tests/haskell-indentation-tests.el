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
          expected-value
          expected-result)
  `(progn
     ,@(cl-loop
        for mode in '(haskell-ts-mode)
        collect
        `(ert-deftest ,(string->symbol (format "%s/%s" name mode)) ()
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
 :name haskell-indentation-tests--test-treesitter-apply-14a
 :contents
 (tests-utils--multiline
  "foo :: Int -> Int"
  "foo x = bar + 1"
  "  where"
  "    bar"
  "      = decombobulate"
  "            _|_frobnicator"
  "      $ x"
  )
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
  "      $ x"
  )
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
 :name haskell-indentation-tests--test-treesitter-field-update-3b
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

(provide 'haskell-indentation-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; haskell-tests.el ends here
