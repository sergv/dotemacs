;; more-haskell.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 30 November 2013
;; Description:

(require 'common)

(defun concatMap (f xs)
  (mapcan f xs))

(provide 'more-haskell)

;; Local Variables:
;; End:

;; more-haskell.el ends here
