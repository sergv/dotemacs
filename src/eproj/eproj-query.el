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
  (eproj-query/any-mode/indent-offset proj 'haskell-mode default))

;;;###autoload
(defun eproj-query/any-mode/indent-offset (proj mode &optional default)
  (declare (pure t) (side-effect-free nil))
  (cl-assert (symbolp mode) "Mode must be a symbol")
  (if-let ((p proj)
           (entry (eproj-project/query-aux-info-entry (eproj-project/aux-info p)
                    'language-specific
                    mode
                    'indent-offset)))
      (let ((res (car entry)))
        (cl-assert (integerp res) nil
                   "language-specific.%s.indent-offset entry in .eproj-info of %s must be an integer, but got %s"
                   mode
                   (eproj-project/root proj)
                   res)
        res)
    default))

;;;###autoload
(defun eproj-query/flycheck-checker (proj mode default)
  (declare (pure t) (side-effect-free nil))
  (cl-assert (symbolp mode))
  (let ((effective-mode (eproj/resolve-synonym-modes mode)))
    (if-let ((p proj)
             (entry (eproj-project/query-aux-info-entry (eproj-project/aux-info p)
                      'flycheck-checker
                      effective-mode)))
        (let ((res (car entry)))
          (cl-assert (symbolp res) nil
                     "flycheck-checker.%s entry in .eproj-info of %s must be a boolean, but got %s"
                     effective-mode
                     (eproj-project/root proj)
                     res)
          res)
      default)))

;;;###autoload
(defun eproj-query/flycheck-disabled-checkers (proj mode default)
  (declare (pure t) (side-effect-free nil))
  (cl-assert (symbolp mode))
  (let ((effective-mode (eproj/resolve-synonym-modes mode)))
    (if-let ((p proj)
             (entry (eproj-project/query-aux-info-entry (eproj-project/aux-info p)
                      'flycheck-disabled-checkers
                      effective-mode)))
        (let ((res entry))
          (cl-assert (and (listp res)
                          (-all-p #'symbolp res))
                     nil
                     "flycheck-disabled-checkers.%s entry in .eproj-info of %s must be a list of symbols, but got %s"
                     effective-mode
                     (eproj-project/root proj)
                     res)
          res)
      default)))

;;;###autoload
(defun eproj-query/local-variables (proj mode default)
  (declare (pure t) (side-effect-free nil))
  (let ((effective-mode (eproj/resolve-synonym-modes mode)))
    (if-let ((p proj)
             (entry (eproj-project/query-aux-info-entry (eproj-project/aux-info p)
                      'local-variables
                      effective-mode)))
        (let ((res (cdr entry)))
          (cl-assert (--all? (and (symbolp (car it))
                                  (null (cddr it)))
                             res)
                     nil
                     "local-variables.%s entry in .eproj-info of %s must be an associative list of (<var-name-symbol> <value>) lists, but got %s"
                     effective-mode
                     (eproj-project/root proj)
                     res)
          res)
      default)))

(provide 'eproj-query)

;; Local Variables:
;; End:

;; eproj-query.el ends here
