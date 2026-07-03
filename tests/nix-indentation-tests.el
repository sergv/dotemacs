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

(provide 'nix-indentation-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; nix-indentation-tests.el ends here
