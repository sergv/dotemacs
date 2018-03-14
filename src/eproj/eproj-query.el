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
    (cl-assert (integerp it) nil
               "language-specific.haskell-mode.indent-offset in .eproj-info must be an integer, but got %s"
               it)
    it))

;;;###autoload
(defun eproj-query/haskell/enable-intero? (proj)
  (declare (pure t))
  (awhen (and proj
              (eproj-project/query-aux-info (eproj-project/aux-info proj)
                language-specific
                haskell-mode
                enable-intero?))
    (cl-assert (booleanp it) nil
               "language-specific.haskell-mode.enable-intero? in .eproj-info must be a boolean, but got %s"
               it)
    it))

;;;###autoload
(defun eproj-query/general/enable-flycheck? (proj)
  (declare (pure t))
  (awhen (and proj
              (eproj-project/query-aux-info (eproj-project/aux-info proj)
                general
                enable-flycheck?))
    (cl-assert (booleanp it) nil
               "general.enable-flycheck? in .eproj-info must be a boolean, but got %s"
               it)
    it))

(provide 'eproj-query)

;; Local Variables:
;; End:

;; eproj-query.el ends here
