;; indentation-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 14 September 2018
;; Description:

(require 'indentation)

(require 'ert)
(require 'tests-utils)

(ert-deftest indentation-tests/indent-relative-forward-1 ()
  (tests-utils--test-buffer-contents
      :action
      (progn
        (indent-relative-forward))
    :contents
    (tests-utils--multiline
     ""
     "foo bar ="
     "_|_"
     "")
    :expected-value
    (tests-utils--multiline
     ""
     "foo bar ="
     "  _|_"
     "")))

(ert-deftest indentation-tests/indent-relative-forward-2 ()
  (tests-utils--test-buffer-contents
      :action
      (progn
        (indent-relative-forward)
        (indent-relative-forward))
    :contents
    (tests-utils--multiline
     ""
     "foo bar ="
     "_|_"
     "")
    :expected-value
    (tests-utils--multiline
     ""
     "foo bar ="
     "    _|_"
     "")))

(ert-deftest indentation-tests/indent-relative-forward-3 ()
  (tests-utils--test-buffer-contents
      :action
      (progn
        (indent-relative-forward)
        (indent-relative-forward)
        (indent-relative-forward))
    :contents
    (tests-utils--multiline
     ""
     "foo bar ="
     "_|_"
     "")
    :expected-value
    (tests-utils--multiline
     ""
     "foo bar ="
     "        _|_"
     "")))

(ert-deftest indentation-tests/indent-relative-forward-4 ()
  (tests-utils--test-buffer-contents
      :action
      (progn
        (indent-relative-forward)
        (indent-relative-forward)
        (indent-relative-forward)
        (indent-relative-forward))
    :contents
    (tests-utils--multiline
     ""
     "foo bar ="
     "_|_"
     "")
    :expected-value
    (tests-utils--multiline
     ""
     "foo bar ="
     "          _|_"
     "")))

(ert-deftest indentation-tests/indent-relative-forward-5 ()
  (tests-utils--test-buffer-contents
      :action
      (progn
        (indent-relative-forward))
    :contents
    (tests-utils--multiline
     "frob = foo 0 where"
     "  foo bar ="
     "_|_"
     "")
    :expected-value
    (tests-utils--multiline
     "frob = foo 0 where"
     "  foo bar ="
     "  _|_"
     "")))

(ert-deftest indentation-tests/indent-relative-forward-6 ()
  (tests-utils--test-buffer-contents
      :action
      (progn
        (indent-relative-forward)
        (indent-relative-forward))
    :contents
    (tests-utils--multiline
     "frob = foo 0 where"
     "  foo bar ="
     "_|_"
     "")
    :expected-value
    (tests-utils--multiline
     "frob = foo 0 where"
     "  foo bar ="
     "    _|_"
     "")))

(provide 'indentation-tests)

;; Local Variables:
;; End:

;; indentation-tests.el ends here
