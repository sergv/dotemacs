;; rust-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 29 November 2019
;; Description:

(require 'common)
(require 'ert)
(require 'tests-utils)

(require 'rust-setup)
(require 'rust-smart-operators)

(defmacro rust-tests--test-buffer-contents (action contents expected-value)
  (declare (indent 1))
  `(tests-utils--test-buffer-contents
    :action ,action
    :contents (concat "\n\n" ,contents "\n\n")
    :expected-value (concat "\n\n" ,expected-value "\n\n")
    :initialisation (rust-mode)
    :buffer-id rust))

(ert-deftest rust-tests/rust-newline--expand-braced-block-1 ()
  (rust-tests--test-buffer-contents
   (rust-newline)
   (tests-utils--multiline
    ""
    "fn foo() {_|_}"
    "")
   (tests-utils--multiline
    ""
    "fn foo() {"
    "    _|_"
    "}"
    "")))

(ert-deftest rust-tests/rust-newline--expand-braced-block-2 ()
  (rust-tests--test-buffer-contents
   (rust-newline)
   (tests-utils--multiline
    ""
    "fn foo() {_|_     }"
    "")
   (tests-utils--multiline
    ""
    "fn foo() {"
    "    _|_"
    "}"
    "")))

(ert-deftest rust-tests/rust-newline--expand-braced-block-3 ()
  (rust-tests--test-buffer-contents
   (rust-newline)
   (tests-utils--multiline
    ""
    "fn foo() {             _|_}"
    "")
   (tests-utils--multiline
    ""
    "fn foo() {"
    "    _|_"
    "}"
    "")))

(ert-deftest rust-tests/rust-newline--expand-braced-block-4 ()
  (rust-tests--test-buffer-contents
   (rust-newline)
   (tests-utils--multiline
    ""
    "fn foo() {             _|_           }"
    "")
   (tests-utils--multiline
    ""
    "fn foo() {"
    "    _|_"
    "}"
    "")))


(ert-deftest rust-tests/rust-newline--duplication-of-commented-line-1 ()
  (rust-tests--test-buffer-contents
   (rust-newline)
   (tests-utils--multiline
    ""
    "// foobar _|_"
    ""
    "fn foo() {"
    "    let x = 2;"
    "    x + x"
    "}"
    "")
   (tests-utils--multiline
    ""
    "// foobar"
    "// _|_"
    ""
    "fn foo() {"
    "    let x = 2;"
    "    x + x"
    "}"
    "")))

(ert-deftest rust-tests/rust-newline--duplication-of-commented-line-2 ()
  (rust-tests--test-buffer-contents
   (rust-newline)
   (tests-utils--multiline
    ""
    "// foobar _|_ quux"
    ""
    "fn foo() {"
    "    let x = 2;"
    "    x + x"
    "}"
    "")
   (tests-utils--multiline
    ""
    "// foobar"
    "// _|_quux"
    ""
    "fn foo() {"
    "    let x = 2;"
    "    x + x"
    "}"
    "")))

(ert-deftest rust-tests/rust-newline--duplication-of-commented-line-3 ()
  (rust-tests--test-buffer-contents
   (rust-newline)
   (tests-utils--multiline
    ""
    "// foobar _|_ quux"
    "fn foo() {"
    "    let x = 2;"
    "    x + x"
    "}"
    "")
   (tests-utils--multiline
    ""
    "// foobar"
    "// _|_quux"
    "fn foo() {"
    "    let x = 2;"
    "    x + x"
    "}"
    "")))


(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-1 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1 +_|_2"
    "x = 1 + + _|_2"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-2 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1 +           _|_ 2"
    "x = 1 + +_|_ 2"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-3 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?/)
    "x = 1 +           _|_"
    "x = 1 +/ _|_"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-4 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1_|_"
    "x = 1 + _|_"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-5 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1 _|_"
    "x = 1 + _|_"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-6 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1  _|_"
    "x = 1  + _|_"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-7 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1  _|_ y"
    "x = 1  +_|_ y"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-8 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?=)
    "x = 1 ! _|_"
    "x = 1 != _|_"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-9 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?=)
    "x = 1 !_|_ y"
    "x = 1 !=_|_ y"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-10 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?-)
    "f(|x|_|_ g(x))"
    "f(|x| -_|_ g(x))"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-11 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?-)
    "f(|x| _|_ g(x))"
    "f(|x| -_|_ g(x))"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-12 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?-)
    "f(|x|  _|_ g(x))"
    "f(|x| -_|_ g(x))"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-13 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?-)
    "let mut groups_count = _|_;"
    "let mut groups_count = -_|_;"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-14 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "let mut groups_count = _|_;"
    "let mut groups_count = +_|_;"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-15 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?-)
    "let mut groups_count =_|_;"
    "let mut groups_count = -_|_;"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-16 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "let mut groups_count =_|_;"
    "let mut groups_count = +_|_;"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-17 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?=)
    "let mut groups_count +_|_;"
    "let mut groups_count += _|_;"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-18 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?=)
    "let mut groups_count + _|_;"
    "let mut groups_count += _|_;"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-19 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?=)
    "if foo = _|_"
    "if foo == _|_"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-20 ()
  (rust-tests--test-buffer-contents
      (progn
        (rust-smart-operators--insert-char-surrounding-with-spaces ?=)
        (rust-smart-operators--insert-char-surrounding-with-spaces ?=))
    "if x_|_ { 0 } else { 1 }"
    "if x ==_|_ { 0 } else { 1 }"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-21 ()
  (rust-tests--test-buffer-contents
      (progn
        (rust-smart-operators--insert-char-surrounding-with-spaces ?=)
        (rust-smart-operators--insert-char-surrounding-with-spaces ?=))
    "let is_contiguous = idx_|_"
    "let is_contiguous = idx == _|_"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-22 ()
  (rust-tests--test-buffer-contents
   (rust-smart-operators--insert-char-surrounding-with-spaces ?-)
    "Sign::Minus => _|_x,"
    "Sign::Minus => - _|_x,"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-23 ()
  (rust-tests--test-buffer-contents
   (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "Sign::Minus => _|_x,"
    "Sign::Minus => + _|_x,"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-24 ()
  (rust-tests--test-buffer-contents
   (rust-smart-operators--insert-char-surrounding-with-spaces ?/)
    "Sign::Minus => _|_x,"
    "Sign::Minus => / _|_x,"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-+-1 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "foo +_|_ bar"
    "foo + +_|_ bar"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-+-2 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "foo +_|_bar"
    "foo + + _|_bar"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-+-3 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "foo +    _|_bar"
    "foo + + _|_bar"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-+-4 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "foo +    _|_  bar"
    "foo + +_|_  bar"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-*-1 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?*)
    "foo *_|_ bar"
    "foo * *_|_ bar"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-*-2 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?*)
    "foo *_|_bar"
    "foo * *_|_bar"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-*-3 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?*)
    "foo *    _|_bar"
    "foo *    *_|_bar"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-*-4 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?*)
    "foo *    _|_  bar"
    "foo *    *_|_  bar"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-*-5 ()
  (rust-tests--test-buffer-contents
      (progn
        (rust-smart-operators--insert-char-surrounding-with-spaces ?&)
        (rust-smart-operators--insert-char-surrounding-with-spaces ?*))
    "foo * _|_bar"
    "foo * &*_|_bar"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-*-6 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?*)
    "heatmap[_|_idx2 as usize]"
    "heatmap[*_|_idx2 as usize]"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator->-1 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?>)
    "f(|x| -_|_ g(x))"
    "f(|x| ->_|_ g(x))"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator->-2 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?>)
    "f(|x| -   _|_ g(x))"
    "f(|x| ->_|_ g(x))"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator->-3 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?>)
    "f(|x| -   _|_g(x))"
    "f(|x| -> _|_g(x))"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator->-4 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?>)
    "f(|x| -_|_g(x))"
    "f(|x| -> _|_g(x))"))

(ert-deftest rust-tests/rust-smart-operators--equals-1 ()
  (rust-tests--test-buffer-contents
   (rust-smart-operators--insert-char-surrounding-with-spaces ?=)
   "let bands: Vec<&mut [u8]>_|_"
   "let bands: Vec<&mut [u8]> = _|_"))

(ert-deftest rust-tests/rust-smart-operators--equals-2 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?=)
    "let bands: Vec<&mut [u8]> _|_"
    "let bands: Vec<&mut [u8]> = _|_"))

(ert-deftest rust-tests/rust-smart-operators--equals-3 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?=)
    "for x in 1.._|_"
    "for x in 1..=_|_"))

(ert-deftest rust-tests/rust-smart-operators--equals-4 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?=)
    "for x in 1..   _|_"
    "for x in 1..=_|_"))

(ert-deftest rust-tests/rust-smart-operators--two-ampersands-1 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\&)
    "x &_|_"
    "x && _|_"))

(ert-deftest rust-tests/rust-smart-operators--two-ampersands-2 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\&)
    "x &_|_ y"
    "x &&_|_ y"))

(ert-deftest rust-tests/rust-smart-operators--pipe-1 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\|)
    "crossbeam::scope(_|_"
    "crossbeam::scope(|_|_"))

(ert-deftest rust-tests/rust-smart-operators--pipe-2 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\|)
    "crossbeam::scope(|spawner_|_"
    "crossbeam::scope(|spawner| _|_"))

(ert-deftest rust-tests/rust-smart-operators--pipe-3 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\|)
    "crossbeam::scope(|spawner_|_)"
    "crossbeam::scope(|spawner|_|_)"))

(ert-deftest rust-tests/rust-smart-operators--pipe-4 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\|)
    "crossbeam::scope(move |spawner_|_)"
    "crossbeam::scope(move |spawner|_|_)"))

(ert-deftest rust-tests/rust-smart-operators--pipe-5 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\|)
    "crossbeam::scope(move  |spawner_|_)"
    "crossbeam::scope(move  |spawner|_|_)"))

(ert-deftest rust-tests/rust-smart-operators--pipe-6 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\|)
    "crossbeam::scope( move  |spawner_|_)"
    "crossbeam::scope( move  |spawner|_|_)"))

(ert-deftest rust-tests/rust-smart-operators--pipe-7 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\|)
    "crossbeam::scope(  move  |spawner_|_)"
    "crossbeam::scope(  move  |spawner|_|_)"))

(ert-deftest rust-tests/rust-smart-operators--pipe-8 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\|)
    "crossbeam::scope( |spawner_|_)"
    "crossbeam::scope( |spawner|_|_)"))

(ert-deftest rust-tests/rust-smart-operators--pipe-9 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\|)
    "crossbeam::scope(  |spawner_|_)"
    "crossbeam::scope(  |spawner|_|_)"))

(ert-deftest rust-tests/rust-smart-operators--pipe-10 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\|)
    "crossbeam::scope(move |spawner_|_"
    "crossbeam::scope(move |spawner| _|_"))

(ert-deftest rust-tests/rust-smart-operators--pipe-11 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\|)
    "crossbeam::scope(  move  |spawner_|_"
    "crossbeam::scope(  move  |spawner| _|_"))

(ert-deftest rust-tests/rust-smart-operators--pipe-12 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\|)
    "crossbeam::scope(move_|_"
    "crossbeam::scope(move |_|_"))

(ert-deftest rust-tests/rust-smart-operators--pipe-13 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\|)
    "crossbeam::scope(move _|_"
    "crossbeam::scope(move |_|_"))

(ert-deftest rust-tests/rust-smart-operators--fat-arrow-1 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\>)
    "Foo(x) =_|_"
    "Foo(x) => _|_"))

(ert-deftest rust-tests/rust-smart-operators--fat-arrow-2 ()
  (rust-tests--test-buffer-contents
      (progn
        (rust-smart-operators--insert-char-surrounding-with-spaces ?\=)
        (rust-smart-operators--insert-char-surrounding-with-spaces ?\>))
    "Foo(x)_|_"
    "Foo(x) => _|_"))

(ert-deftest rust-tests/rust-smart-operators--arrow-1 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\>)
    "fn foo(x: i32) -_|_"
    "fn foo(x: i32) -> _|_"))

(ert-deftest rust-tests/rust-smart-operators--arrow-2 ()
  (rust-tests--test-buffer-contents
      (progn
        (rust-smart-operators--insert-char-surrounding-with-spaces ?\-)
        (rust-smart-operators--insert-char-surrounding-with-spaces ?\>))
    "foo(x: i32)_|_"
    "foo(x: i32) -> _|_"))

(ert-deftest rust-tests/rust-smart-operators--ampersand-1 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\&)
    "let foo: &Foo = _|_"
    "let foo: &Foo = &_|_"))

(ert-deftest rust-tests/rust-smart-operators--ampersand-2 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\&)
    "let foo: &Foo =_|_"
    "let foo: &Foo = &_|_"))

(ert-deftest rust-tests/rust-smart-operators--ampersand-3 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\&)
    "let foo: &Foo = _|_foobar"
    "let foo: &Foo = &_|_foobar"))

(ert-deftest rust-tests/rust-smart-operators--ampersand-4 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\&)
    "a -> _|_b"
    "a -> &_|_b"))

(ert-deftest rust-tests/rust-smart-operators--ampersand-5 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\&)
    "let x: &[_|_]"
    "let x: &[&_|_]"))

(ert-deftest rust-tests/rust-smart-operators--ampersand-6 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\&)
    "Vec<_|_"
    "Vec<&_|_"))

(ert-deftest rust-tests/rust-smart-operators--ampersand-7 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\&)
    "fn parse(self) -> Result<_|_str> {"
    "fn parse(self) -> Result<&_|_str> {"))

(ert-deftest rust-tests/rust-smart-operators--asterisk-1 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\*)
    "let foo: &Foo = _|_foobar"
    "let foo: &Foo = *_|_foobar"))

(ert-deftest rust-tests/rust-smart-operators--asterisk-2 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\*)
    "x + _|_y"
    "x + *_|_y"))

(ert-deftest rust-tests/rust-smart-operators--asterisk-3 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\*)
    "use crate::common::_|_;"
    "use crate::common::*_|_;"))

(ert-deftest rust-tests/rust-smart-operators--asterisk-4 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\*)
    "use crate::common::_|_"
    "use crate::common::*_|_"))

(ert-deftest rust-tests/rust-smart-operators--asterisk-5 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\*)
    "while i < level.len() && _|_level[i].1 == &Tree::Empty {"
    "while i < level.len() && *_|_level[i].1 == &Tree::Empty {"))

(ert-deftest rust-tests/rust-smart-operators--asterisk-6 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\*)
    "fn parse(self) -> Result<_|_int> {"
    "fn parse(self) -> Result<*_|_int> {"))

(ert-deftest rust-tests/rust-smart-operators--gt-1 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\>)
    (tests-utils--multiline
     ""
     "let x: Vec<Box<i32_|_"
     "")
    (tests-utils--multiline
     ""
     "let x: Vec<Box<i32>_|_"
     "")))

(ert-deftest rust-tests/rust-smart-operators--gt-2 ()
  (rust-tests--test-buffer-contents
      (progn
        (rust-smart-operators--insert-char-surrounding-with-spaces ?\>)
        (rust-smart-operators--insert-char-surrounding-with-spaces ?\>))
    (tests-utils--multiline
     ""
     "let x: Vec<Box<i32_|_ = vec![1, 2, 3];"
     "")
    (tests-utils--multiline
     ""
     "let x: Vec<Box<i32>>_|_ = vec![1, 2, 3];"
     "")))

(ert-deftest rust-tests/rust-smart-operators--gt-3 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\>)
    (tests-utils--multiline
     ""
     "let x: Vec<Box<i32> _|_ = vec![1, 2, 3];"
     "")
    (tests-utils--multiline
     ""
     "let x: Vec<Box<i32>>_|_ = vec![1, 2, 3];"
     "")))

(ert-deftest rust-tests/rust-smart-operators--slash-1 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?/)
    (tests-utils--multiline
     ""
     "_|_"
     "fn foo(x: i32) -> i32 {"
     "   x + 1"
     "}"
     "")
    (tests-utils--multiline
     ""
     "/ _|_"
     "fn foo(x: i32) -> i32 {"
     "   x + 1"
     "}"
     "")))

(ert-deftest rust-tests/rust-smart-operators--slash-2 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?/)
    (tests-utils--multiline
     ""
     "/ _|_"
     "fn foo(x: i32) -> i32 {"
     "   x + 1"
     "}"
     "")
    (tests-utils--multiline
     ""
     "// _|_"
     "fn foo(x: i32) -> i32 {"
     "   x + 1"
     "}"
     "")))

(ert-deftest rust-tests/rust-smart-operators--slash-3 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?/)
    (tests-utils--multiline
     ""
     "// _|_"
     "fn foo(x: i32) -> i32 {"
     "   x + 1"
     "}"
     "")
    (tests-utils--multiline
     ""
     "/// _|_"
     "fn foo(x: i32) -> i32 {"
     "   x + 1"
     "}"
     "")))

(ert-deftest rust-tests/rust-smart-operators--slash-4 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?\/)
    (tests-utils--multiline
     ""
     "_|_// Test"
     "fn foo(x: i32) -> i32 {"
     "    x * 2"
     "}"
     "")
    (tests-utils--multiline
     ""
     "/_|_// Test"
     "fn foo(x: i32) -> i32 {"
     "    x * 2"
     "}"
     "")))

(ert-deftest rust-tests/rust-smart-operators--comma-1 ()
  (rust-tests--test-buffer-contents
      (smart-operators-comma)
    (tests-utils--multiline
     ""
     "fn foo(x: i32) -> char {"
     "   '_|_'"
     "}"
     "")
    (tests-utils--multiline
     ""
     "fn foo(x: i32) -> char {"
     "   ',_|_'"
     "}"
     "")))

;; ;; Impossible to support?
;; (ert-deftest rust-tests/rust-smart-operators--comma-2 ()
;;   (rust-tests--test-buffer-contents
;;    (smart-operators-comma)
;;    (tests-utils--multiline
;;     ""
;;     "fn append(&mut self, payload: A)"
;;     "    where"
;;     "    A: Clone + for<'b> Monoid<'static_|_'b>"
;;     "{"
;;     "")
;;    (tests-utils--multiline
;;     ""
;;     "fn append(&mut self, payload: A)"
;;     "    where"
;;     "    A: Clone + for<'b> Monoid<'static, _|_'b>"
;;     "{"
;;     "")))

(ert-deftest rust-tests/paredit-splice-sexp-killing-backward-1 ()
  (rust-tests--test-buffer-contents
      (paredit-splice-sexp-killing-backward)
    "foo = ({ foo: _|_ a, b})"
    "foo = (_|_a, b)"))

(ert-deftest rust-tests/paredit-splice-sexp-killing-backward-2 ()
  (rust-tests--test-buffer-contents
      (paredit-splice-sexp-killing-backward)
    (tests-utils--multiline
     ""
     "if ulen <= rest1.len() {"
     "    Ok(((_|_&rest2[..ulen]), &rest2[ulen..]))"
     "} else {"
     "    Err(ParseError::PrematureStringEnd(mk_string(input)))"
     "}"
     "")
    (tests-utils--multiline
     ""
     "if ulen <= rest1.len() {"
     "    Ok((_|_&rest2[..ulen], &rest2[ulen..]))"
     "} else {"
     "    Err(ParseError::PrematureStringEnd(mk_string(input)))"
     "}"
     "")))

;; (ert-deftest rust-tests/paredit-splice-sexp-killing-backward-3 ()
;;   (rust-tests--test-buffer-contents
;;       (paredit-splice-sexp-killing-backward)
;;     (tests-utils--multiline
;;      ""
;;      "if ulen <= rest1.len() {"
;;      "    Ok(((&_|_rest2[..ulen]), &rest2[ulen..]))"
;;      "} else {"
;;      "    Err(ParseError::PrematureStringEnd(mk_string(input)))"
;;      "}"
;;      "")
;;     (tests-utils--multiline
;;      ""
;;      "if ulen <= rest1.len() {"
;;      "    Ok((_|_rest2[..ulen], &rest2[ulen..]))"
;;      "} else {"
;;      "    Err(ParseError::PrematureStringEnd(mk_string(input)))"
;;      "}"
;;      "")))

(provide 'rust-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; rust-tests.el ends here
