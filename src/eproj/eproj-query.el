;; eproj-query.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 29 January 2018
;; Description:

(require 'eproj)

;; Predefined queries

;;;###autoload
(defun eproj-query/haskell/indent-offset (proj)
  (declare (pure t))
  (awhen (and proj
              (eproj-project/query-aux-info (eproj-project/aux-info proj)
                language-specific
                haskell-mode
                indent-offset))
    (assert (integerp it)
            nil
            "haskell-offset in .eproj-info must be an integer, but got %s"
            it)
    it))

;;;###autoload
(defun eproj-query/haskell/disable-intero? (proj)
  (declare (pure t))
  (awhen (and proj
              (eproj-project/query-aux-info (eproj-project/aux-info proj)
                language-specific
                haskell-mode
                disable-intero?))
    (assert (booleanp it)
            nil
            "disable-intero? in .eproj-info must be a boolean, but got %s"
            it)
    it))

;;;###autoload
(defun eproj-query/general/disable-flycheck? (proj)
  (declare (pure t))
  (awhen (and proj
              (eproj-project/query-aux-info (eproj-project/aux-info proj)
                general
                disable-flycheck?))
    (assert (booleanp it)
            nil
            "disable-flycheck? in .eproj-info must be a boolean, but got %s"
            it)
    it))

(provide 'eproj-query)

;; Local Variables:
;; End:

;; eproj-query.el ends here
