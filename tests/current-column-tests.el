;; current-column-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 10 December 2023
;; Description:

(require 'current-column-fixed)

(require 'tests-utils)

(ert-deftest current-column-tests/move-to-column-fixed-1 ()
  (tests-utils--test-buffer-contents
      :action
      (move-to-column-fixed 10)
    :contents
    (tests-utils--multiline
     ""
     "_|_"
     "")
    :expected-value
    (tests-utils--multiline
     ""
     "_|_"
     "")
    :initialisation (text-mode)
    :buffer-id current-column-text))

(ert-deftest current-column-tests/move-to-column-fixed-2 ()
  (tests-utils--test-buffer-contents
      :action
      (move-to-column-fixed 10 t)
    :contents
    (tests-utils--multiline
     ""
     "_|_"
     "")
    :expected-value
    (tests-utils--multiline
     ""
     "          _|_"
     "")
    :initialisation (text-mode)
    :buffer-id nil))

(ert-deftest current-column-tests/move-to-column-fixed-3 ()
  (tests-utils--test-buffer-contents
      :action
      (progn
        (indent-tabs-mode -1)
        (move-to-column-fixed 5 t))
    :contents
    (tests-utils--multiline
     ""
     " _|_"
     "")
    :expected-value
    (tests-utils--multiline
     ""
     "     _|_"
     "")
    :initialisation (text-mode)
    :buffer-id nil))

(provide 'current-column-tests)

;; Local Variables:
;; End:

;; current-column-tests.el ends here
