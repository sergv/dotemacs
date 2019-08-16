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
           (entry (eproj-project/query-aux-info-entry (eproj-project/aux-info p)
                    'language-specific
                    'haskell-mode
                    'indent-offset)))
      (let ((res (car entry)))
        (cl-assert (integerp res) nil
                   "language-specific.haskell-mode.indent-offset entry in .eproj-info of %s must be an integer, but got %s"
                   (eproj-project/root proj)
                   res)
        res)
    default))

;;;###autoload
(defun eproj-query/flycheck-checker (proj mode default)
  (declare (pure t) (side-effect-free nil))
  (cl-assert (symbolp mode))
  (if-let ((p proj)
           (entry (eproj-project/query-aux-info-entry (eproj-project/aux-info p)
                    'flycheck-checker
                    mode)))
      (let ((res (car entry)))
        (cl-assert (symbolp res) nil
                   "flycheck-checker.%s entry in .eproj-info of %s must be a boolean, but got %s"
                   mode
                   (eproj-project/root proj)
                   res)
        res)
    default))

;;;###autoload
(defun eproj-query/flycheck-disabled-checkers (proj mode default)
  (declare (pure t) (side-effect-free nil))
  (cl-assert (symbolp mode))
  (if-let ((p proj)
           (entry (eproj-project/query-aux-info-entry (eproj-project/aux-info p)
                    'flycheck-disabled-checkers
                    mode)))
      (let ((res entry))
        (cl-assert (and (listp res)
                        (-all-p #'symbolp res))
                   nil
                   "flycheck-disabled-checkers.%s entry in .eproj-info of %s must be a list of symbols, but got %s"
                   mode
                   (eproj-project/root proj)
                   res)
        res)
    default))

;;;###autoload
(defun eproj-query/intero-auto-install (proj default)
  (declare (pure t) (side-effect-free nil))
  (if-let ((p proj)
           (entry (eproj-project/query-aux-info-entry (eproj-project/aux-info p)
                    'intero-auto-install)))
      (let ((res (car entry)))
        (cl-assert (or (eq res 'nil)
                       (eq res 't))
                   nil
                   "intero-auto-install entry in .eproj-info of %s must be a boolean (either 't or 'nil), but got %s"
                   (eproj-project/root proj)
                   res)
        res)
    default))

;;;###autoload
(defun eproj-query/dante-package-name (proj default)
  (declare (pure t) (side-effect-free nil))
  (if-let ((p proj)
           (entry (eproj-project/query-aux-info-entry (eproj-project/aux-info p)
                    'dante-package-name)))
      (let ((res (car entry)))
        (cl-assert (stringp res)
                   nil
                   "dante-package-name entry in .eproj-info of %s must be a string, but got %s"
                   (eproj-project/root proj)
                   res)
        res)
    default))

(provide 'eproj-query)

;; Local Variables:
;; End:

;; eproj-query.el ends here
