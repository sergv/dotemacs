;; comment-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 19 April 2023
;; Description:

(require 'comment-util)

(require 'ert)
(require 'tests-utils)

(ert-deftest comment-tests/comment-region-1 ()
  (tests-utils--test-buffer-contents
      :action
      (let ((col (current-column-fixed)))
        (comment-util-comment-region--impl (point)
                                           (save-excursion
                                             (forward-line 1)
                                             (move-to-column col)
                                             (point))))
    :contents
    (tests-utils--multiline
     ""
     "foo bar = do"
     "  x <- quux bar"
     "  y <- _|_quux $ bar * 2"
     "  z <- quux $ bar * 3"
     "  pure $ x + y + z"
     "")
    :expected-value
    (tests-utils--multiline
     ""
     "foo bar = do"
     "  x <- quux bar"
     "  -- y <- _|_quux $ bar * 2"
     "  -- z <- quux $ bar * 3"
     "  pure $ x + y + z"
     "")
    :initialisation (haskell-mode)
    :buffer-id comment-tests-haskell))

(ert-deftest comment-tests/comment-lines-1 ()
  (tests-utils--test-buffer-contents
      :action
      (comment-util-comment-lines 2)
    :contents
    (tests-utils--multiline
     ""
     "foo bar = do"
     "  x <- quux bar"
     "  y <- _|_quux $ bar * 2"
     "  z <- quux $ bar * 3"
     "  pure $ x + y + z"
     "")
    :expected-value
    (tests-utils--multiline
     ""
     "foo bar = do"
     "  x <- quux bar"
     "  -- y <- quux $ bar * 2"
     "  -- z <- quux $ bar * 3"
     "_|_  pure $ x + y + z"
     "")
    :initialisation (haskell-mode)
    :buffer-id comment-tests-haskell))

(ert-deftest comment-tests/uncomment-region-1 ()
  (tests-utils--test-buffer-contents
      :action
      (comment-util-uncomment-region)
    :contents
    (tests-utils--multiline
     ""
     "foo bar = do"
     "  -- x <- quux bar"
     "  -- y <- _|_quux $ bar * 2"
     "  -- z <- quux $ bar * 3"
     "  pure $ x + y + z"
     "")
    :expected-value
    (tests-utils--multiline
     ""
     "foo bar = do"
     "  x <- quux bar"
     "  y <- _|_quux $ bar * 2"
     "  z <- quux $ bar * 3"
     "  pure $ x + y + z"
     "")
    :initialisation (haskell-mode)
    :buffer-id comment-tests-haskell))

(ert-deftest comment-tests/uncomment-region-2 ()
  (tests-utils--test-buffer-contents
      :action
      (comment-util-uncomment-region)
    :contents
    (tests-utils--multiline
     "-- _|_foo :: a -> a"
     "foo x = x"
     "")
    :expected-value
    (tests-utils--multiline
     "_|_foo :: a -> a"
     "foo x = x"
     "")
    :initialisation (haskell-mode)
    :buffer-id comment-tests-haskell))

(ert-deftest comment-tests/uncomment-region-simple-1 ()
  (tests-utils--test-buffer-contents
      :action
      (let ((col (current-column-fixed)))
        (comment-util-uncomment-region-simple (point)
                                              (save-excursion
                                                (forward-line 1)
                                                (move-to-column col)
                                                (point))))
    :contents
    (tests-utils--multiline
     ""
     "int foo(int bar) {"
     "  // int x = quux(bar);"
     "  // int y = _|_quux(bar * 2);"
     "  // int z = quux(bar * 3);"
     "  return x + y + z"
     "}"
     "")
    :expected-value
    (tests-utils--multiline
     ""
     "int foo(int bar) {"
     "  // int x = quux(bar);"
     "  int y = _|_quux(bar * 2);"
     "  int z = quux(bar * 3);"
     "  return x + y + z"
     "}"
     "")
    :initialisation
    (progn
      (c-mode)
      (setf c-basic-offset 2))
    :buffer-id comment-tests-c))

(provide 'comment-tests)

;; Local Variables:
;; End:

;; comment-tests.el ends here
