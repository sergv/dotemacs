;; nix-indentation-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  Wednesday,  1 July 2026
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'cl))

(require 'nix-ts-mode)

(require 'common)
(require 'ert)
(require 'search)
(require 'tests-utils)
(require 'treesit-setup)

(cl-defmacro nix-indentation-tests--test-treesitter
    (&key name
          contents        ;; Buffer text before indent.
          expected-value  ;; Buffer text after indent.
          expected-result ;; Whether the test should fail or succeed.
          )
  (cl-assert (symbolp name))
  `(ert-deftest ,name ()
     :expected-result ,(or expected-result :passed) ;;:failed
     (tests-utils--test-buffer-contents
      :action (treesit-indent)
      :contents ,contents
      :expected-value ,expected-value
      :initialisation (nix-ts-mode)
      :buffer-id
      ,(string->symbol "nix-indentation-tests"))))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--let-comment-after-in-1
 :contents
 (tests-utils--multiline
  "let foo = bar;"
  "in"
  "    _|_# foo"
  "{"
  "  bar = 1;"
  "}")
 :expected-value
 (tests-utils--multiline
  "let foo = bar;"
  "in"
  "_|_# foo"
  "{"
  "  bar = 1;"
  "}"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--let-in-binding-1
 :contents
 (tests-utils--multiline
  "let"
  "      _|_foo = bar;"
  "in"
  "{"
  "  bar = 1;"
  "}")
 :expected-value
 (tests-utils--multiline
  "let"
  "  _|_foo = bar;"
  "in"
  "{"
  "  bar = 1;"
  "}"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--let-in-binding-2
 :contents
 (tests-utils--multiline
  "let foo ="
  "                _|_bar;"
  "in"
  "{"
  "  bar = 1;"
  "}")
 :expected-value
 (tests-utils--multiline
  "let foo ="
  "      _|_bar;"
  "in"
  "{"
  "  bar = 1;"
  "}"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--let-in-binding-3
 :contents
 (tests-utils--multiline
  "let bar = pkgs.fetchgit {"
  "           _|_foo = 1;"
  "      inherit rev sha256;"
  "    };"
  "in 2")
 :expected-value
 (tests-utils--multiline
  "let bar = pkgs.fetchgit {"
  "      _|_foo = 1;"
  "      inherit rev sha256;"
  "    };"
  "in 2"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--let-in-binding-4
 :contents
 (tests-utils--multiline
  "let bar ="
  "      pkgs.fetchgit {"
  "           _|_foo = 1;"
  "        inherit rev sha256;"
  "      };"
  "in 2")
 :expected-value
 (tests-utils--multiline
  "let bar ="
  "      pkgs.fetchgit {"
  "        _|_foo = 1;"
  "        inherit rev sha256;"
  "      };"
  "in 2"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--let-expression-1a
 :contents
 (tests-utils--multiline
  "let"
  "  x = let"
  "    _|_a = b;"
  "  in"
  "  c"
  "  ;"
  "in z")
 :expected-value
 (tests-utils--multiline
  "let"
  "  x = let"
  "        _|_a = b;"
  "  in"
  "  c"
  "  ;"
  "in z"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--let-expression-1b
 :contents
 (tests-utils--multiline
  "let"
  "  x = let"
  "        a = b;"
  "  _|_in"
  "  c"
  "  ;"
  "in z")
 :expected-value
 (tests-utils--multiline
  "let"
  "  x = let"
  "        a = b;"
  "      _|_in"
  "  c"
  "  ;"
  "in z"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--let-expression-1c
 :contents
 (tests-utils--multiline
  "let"
  "  x = let"
  "        a = b;"
  "      in"
  "  _|_c"
  "  ;"
  "in z")
 :expected-value
 (tests-utils--multiline
  "let"
  "  x = let"
  "        a = b;"
  "      in"
  "      _|_c"
  "  ;"
  "in z"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--let-expression-1d
 :contents
 (tests-utils--multiline
  "let"
  "  x = let"
  "        a = b;"
  "      in"
  "      c"
  "           _|_;"
  "in z")
 :expected-value
 (tests-utils--multiline
  "let"
  "  x = let"
  "        a = b;"
  "      in"
  "      c"
  "    _|_;"
  "in z"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--let-expression-2
 :contents
 (tests-utils--multiline
  "let"
  "      _|_# test"
  "  x = 1;"
  "in z")
 :expected-value
 (tests-utils--multiline
  "let"
  "  _|_# test"
  "  x = 1;"
  "in z"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--let-in-comment-after-binding-1
 :contents
 (tests-utils--multiline
  "let"
  "  foo = bar;"
  "            _|_# foo"
  "in"
  "{"
  "  bar = 1;"
  "}")
 :expected-value
 (tests-utils--multiline
  "let"
  "  foo = bar;"
  "  _|_# foo"
  "in"
  "{"
  "  bar = 1;"
  "}"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--let-in-comment-after-binding-2
 :contents
 (tests-utils--multiline
  "let foo = bar;"
  "            _|_# foo"
  "in"
  "{"
  "  bar = 1;"
  "}")
 :expected-value
 (tests-utils--multiline
  "let foo = bar;"
  "    _|_# foo"
  "in"
  "{"
  "  bar = 1;"
  "}"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--let-in-comment-after-binding-3
 :contents
 (tests-utils--multiline
  "let foo = bar;"
  "            _|_# foo"
  "    baz = 2;"
  "in"
  "{"
  "  bar = 1;"
  "}")
 :expected-value
 (tests-utils--multiline
  "let foo = bar;"
  "    _|_# foo"
  "    baz = 2;"
  "in"
  "{"
  "  bar = 1;"
  "}"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--lambda-body-1
 :contents
 (tests-utils--multiline
  "let"
  "  foo = x:"
  "          _|_y;"
  "in foo")
 :expected-value
 (tests-utils--multiline
  "let"
  "  foo = x:"
  "    _|_y;"
  "in foo"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--if-then-else-1a
 :contents
 (tests-utils--multiline
  "pkgs.runCommand (\"wrapped-\" + source)"
  "  {"
  "    nativeBuildInputs = [];"
  "  }"
  "  ''"
  "    mkdir -p \"$out/bin\""
  "    ${if builtins.isList dests"
  "          _|_then builtins.concatStringsSep \"\\n\" (builtins.map f dests)"
  "      else f dests}"
  "  ''")
 :expected-value
 (tests-utils--multiline
  "pkgs.runCommand (\"wrapped-\" + source)"
  "  {"
  "    nativeBuildInputs = [];"
  "  }"
  "  ''"
  "    mkdir -p \"$out/bin\""
  "    ${if builtins.isList dests"
  "      _|_then builtins.concatStringsSep \"\\n\" (builtins.map f dests)"
  "      else f dests}"
  "  ''"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--if-then-else-1b
 :contents
 (tests-utils--multiline
  "pkgs.runCommand (\"wrapped-\" + source)"
  "  {"
  "    nativeBuildInputs = [];"
  "  }"
  "  ''"
  "    mkdir -p \"$out/bin\""
  "    ${if builtins.isList dests"
  "        then builtins.concatStringsSep \"\\n\" (builtins.map f dests)"
  "                _|_else f dests}"
  "  ''")
 :expected-value
 (tests-utils--multiline
  "pkgs.runCommand (\"wrapped-\" + source)"
  "  {"
  "    nativeBuildInputs = [];"
  "  }"
  "  ''"
  "    mkdir -p \"$out/bin\""
  "    ${if builtins.isList dests"
  "        then builtins.concatStringsSep \"\\n\" (builtins.map f dests)"
  "        _|_else f dests}"
  "  ''"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--if-then-else-2a
 :contents
 (tests-utils--multiline
  "''"
  "${if hasDocs"
  "    then"
  "             _|_1"
  "  else"
  "  2}"
  "''")
 :expected-value
 (tests-utils--multiline
  "''"
  "${if hasDocs"
  "    then"
  "      _|_1"
  "  else"
  "  2}"
  "''"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--if-then-else-2b
 :contents
 (tests-utils--multiline
  "''"
  "${if hasDocs"
  "  then"
  "             1"
  "    else"
  "                _|_2}"
  "''")
 :expected-value
 (tests-utils--multiline
  "''"
  "${if hasDocs"
  "  then"
  "             1"
  "    else"
  "      _|_2}"
  "''"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--if-then-else-2c
 :contents
 (tests-utils--multiline
  "''"
  "${if"
  "               _|_hasDocs"
  "  then"
  "             1"
  "    else"
  "                2}"
  "''")
 :expected-value
 (tests-utils--multiline
  "''"
  "${if"
  "    _|_hasDocs"
  "  then"
  "             1"
  "    else"
  "                2}"
  "''"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--rec-attrset-1
 :contents
 (tests-utils--multiline
  "rec {"
  "  hello ="
  "    world;"
  ""
  "       _|_yes ="
  "    \"no\";"
  ""
  "  pi ="
  "    3;"
  "}")
 :expected-value
 (tests-utils--multiline
  "rec {"
  "  hello ="
  "    world;"
  ""
  "  _|_yes ="
  "    \"no\";"
  ""
  "  pi ="
  "    3;"
  "}"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--attrset-1
 :contents
 (tests-utils--multiline
  "{"
  "  hello ="
  "    world;"
  ""
  "       _|_yes ="
  "    \"no\";"
  ""
  "  pi ="
  "    3;"
  "}")
 :expected-value
 (tests-utils--multiline
  "{"
  "  hello ="
  "    world;"
  ""
  "  _|_yes ="
  "    \"no\";"
  ""
  "  pi ="
  "    3;"
  "}"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--attrset-2
 :contents
 (tests-utils--multiline
  "let foo = 1;"
  "in"
  "{"
  "  hello ="
  "    world;"
  ""
  "       _|_yes ="
  "    \"no\";"
  ""
  "  pi ="
  "    3;"
  "}")
 :expected-value
 (tests-utils--multiline
  "let foo = 1;"
  "in"
  "{"
  "  hello ="
  "    world;"
  ""
  "  _|_yes ="
  "    \"no\";"
  ""
  "  pi ="
  "    3;"
  "}"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--attrset-3a
 :contents
 (tests-utils--multiline
  "let foo = 1;"
  "in {"
  "  hello ="
  "    world;"
  ""
  "       _|_yes ="
  "    \"no\";"
  ""
  "  pi ="
  "    3;"
  "}")
 :expected-value
 (tests-utils--multiline
  "let foo = 1;"
  "in {"
  "  hello ="
  "    world;"
  ""
  "  _|_yes ="
  "    \"no\";"
  ""
  "  pi ="
  "    3;"
  "}"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--attrset-3b
 :contents
 (tests-utils--multiline
  "let foo = 1;"
  "in {"
  "           _|_hello ="
  "    world;"
  ""
  "  yes ="
  "    \"no\";"
  ""
  "  pi ="
  "    3;"
  "}")
 :expected-value
 (tests-utils--multiline
  "let foo = 1;"
  "in {"
  "  _|_hello ="
  "    world;"
  ""
  "  yes ="
  "    \"no\";"
  ""
  "  pi ="
  "    3;"
  "}"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--attrset-4a
 :contents
 (tests-utils--multiline
  "(if pkgs-cross-win == null"
  " then {}"
  " else {"
  "       _|_cross-win = ghc-win;"
  "     })")
 :expected-value
 (tests-utils--multiline
  "(if pkgs-cross-win == null"
  " then {}"
  " else {"
  "   _|_cross-win = ghc-win;"
  "     })"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--attrset-4b
 :contents
 (tests-utils--multiline
  "(if pkgs-cross-win == null"
  " then {}"
  " else {"
  "       cross-win = ghc-win;"
  "     _|_})")
 :expected-value
 (tests-utils--multiline
  "(if pkgs-cross-win == null"
  " then {}"
  " else {"
  "       cross-win = ghc-win;"
  " _|_})"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--misc-1a
 :contents
 (tests-utils--multiline
  "{"
  "  foo = ("
  "  _|_z:"
  "  x: {"
  "    y = x + z;"
  "  }"
  ");"
  "}")
 :expected-value
 (tests-utils--multiline
  "{"
  "  foo = ("
  "    _|_z:"
  "  x: {"
  "    y = x + z;"
  "  }"
  ");"
  "}"))

(nix-indentation-tests--test-treesitter
 :name nix-indentation-tests--misc-1b
 :contents
 (tests-utils--multiline
  "{"
  "  foo = ("
  "  z:"
  "  x: {"
  "    y = x + z;"
  "  }"
  "_|_);"
  "}")
 :expected-value
 (tests-utils--multiline
  "{"
  "  foo = ("
  "  z:"
  "  x: {"
  "    y = x + z;"
  "  }"
  "  _|_);"
  "}"))

(provide 'nix-indentation-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; nix-indentation-tests.el ends here
