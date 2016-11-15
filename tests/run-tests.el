;; run-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 14 November 2016
;; Description:

;; (add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path "/home/sergey/emacs/tests")

(load "common-tests")
(load "datastructures-tests")
(load "eproj-tests")
(load "git-tests")
(load "haskell-tests")
(load "persistent-sessions-tests")
(load "persistent-store-tests")

(ert ".*")

;; Local Variables:
;; End:

;; run-tests.el ends here
