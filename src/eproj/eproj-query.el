;; eproj-query.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 29 January 2018
;; Description:

(require 'eproj)

;; Predefined queries

;;;###autoload
(defun eproj-query/haskell/indent-offset (proj &optional default)
  (declare (pure t) (side-effect-free nil))
  (if-let ((p proj)
           (entry (eproj-project/query-aux-info-seq (eproj-project/aux-info p)
                    language-specific
                    haskell-mode
                    indent-offset)))
      (let ((res (car entry)))
        (cl-assert (integerp res) nil
                   "language-specific.haskell-mode.indent-offset in .eproj-info must be an integer, but got %s"
                   it)
        res)
    default))

;;;###autoload
(defun eproj-query/haskell/enable-intero? (proj default)
  (declare (pure t) (side-effect-free nil))
  (if-let ((p proj)
           (entry (eproj-project/query-aux-info-seq (eproj-project/aux-info p)
                    language-specific
                    haskell-mode
                    enable-intero?)))
      (let ((res (car entry)))
        (cl-assert (booleanp res) nil
                   "language-specific.haskell-mode.enable-intero? in .eproj-info must be a boolean, but got %s"
                   it)
        res)
    default))

;;;###autoload
(defun eproj-query/general/enable-flycheck? (proj default)
  (declare (pure t) (side-effect-free nil))
  (if-let ((p proj)
           (entry (eproj-project/query-aux-info-seq (eproj-project/aux-info p)
                    general
                    enable-flycheck?)))
      (let ((res (car entry)))
        (cl-assert (booleanp res) nil
                   "general.enable-flycheck? in .eproj-info must be a boolean, but got %s"
                   it)
        res)
    default))

(provide 'eproj-query)

;; Local Variables:
;; End:

;; eproj-query.el ends here
