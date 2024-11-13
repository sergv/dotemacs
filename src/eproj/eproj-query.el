;; eproj-query.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 29 January 2018
;; Description:

(require 'eproj)

;; Predefined queries

;;;###autoload
(defun eproj-query/build-dir (proj &optional default)
  (declare (pure t) (side-effect-free nil))
  (if-let ((p proj)
           (entry (eproj-project/query-aux-info-entry (eproj-project/aux-info p)
                    'build-dir)))
      (let ((res (car entry)))
        (unless (or (stringp res)
                    (null res))
          (error "build-dir entry in .eproj-info of %s must be a string or nil, but got %s"
                 (eproj-project/root proj)
                 res))
        res)
    default))

;;;###autoload
(defun eproj-query/fold-build-dir (proj if-undef if-def)
  (if proj
      (let* ((undef '#:undef)
             (dir (eproj-query/build-dir proj undef)))
        (if (eq dir undef)
            (funcall if-undef)
          (funcall if-def dir)))
    (funcall if-undef)))

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
        (unless (integerp res)
          (error "language-specific.%s.indent-offset entry in .eproj-info of %s must be an integer, but got %s"
                 mode
                 (eproj-project/root proj)
                 res))
        res)
    default))

;;;###autoload
(defun eproj-query/java/indent-tab (proj &optional default)
  (declare (pure t) (side-effect-free nil))
  (eproj-query/any-mode/indent-tab proj 'java-mode default))

;;;###autoload
(defun eproj-query/any-mode/indent-tab (proj mode &optional default)
  (declare (pure t) (side-effect-free nil))
  (cl-assert (symbolp mode) "Mode must be a symbol")
  (if-let ((p proj)
           (entry (eproj-project/query-aux-info-entry (eproj-project/aux-info p)
                    'language-specific
                    mode
                    'indent-tab)))
      (let ((res (car entry)))
        (unless (booleanp res)
          (error "language-specific.%s.indent-tab entry in .eproj-info of %s must be a boolean, but got %s"
                 mode
                 (eproj-project/root proj)
                 res))
        res)
    default))

;;;###autoload
(defun eproj-query/java/indent-style (proj &optional default)
  (declare (pure t) (side-effect-free nil))
  (eproj-query/any-mode/indent-style proj 'java-mode default))

;;;###autoload
(defun eproj-query/any-mode/indent-style (proj mode &optional default)
  "For now only allows styles registerted in ‘c-style-alist’ and thus only has
sense for modes where cc’s indentation engine works."
  (declare (pure t) (side-effect-free nil))
  (cl-assert (symbolp mode) "Mode must be a symbol")
  (if-let ((p proj)
           (entry (eproj-project/query-aux-info-entry (eproj-project/aux-info p)
                    'language-specific
                    mode
                    'indent-style)))
      (let ((res (car entry)))
        (unless (stringp res)
          (error "language-specific.%s.indent-style entry in .eproj-info of %s must be a string, but got %s"
                 mode
                 (eproj-project/root proj)
                 res))
        (unless (assoc res c-style-alist)
          (error "language-specific.%s.indent-style entry in .eproj-info of %s specifies unknown indentation style: %s"
                 mode
                 (eproj-project/root proj)
                 res))
        res)
    default))

;;;###autoload
(defun eproj-query/checker (proj mode default)
  (declare (pure t) (side-effect-free nil))
  (cl-assert (symbolp mode))
  (let ((effective-mode (eproj/resolve-synonym-modes mode)))
    (if-let ((p proj)
             (entry (eproj-project/query-aux-info-entry (eproj-project/aux-info p)
                      'checker
                      effective-mode)))
        (let ((res (car entry)))
          (unless (symbolp res)
            (error "checker.%s entry in .eproj-info of %s must be a symbol, but got %s"
                   effective-mode
                   (eproj-project/root proj)
                   res))
          res)
      default)))

;;;###autoload
(defun eproj-query/disabled-checkers (proj mode default)
  (declare (pure t) (side-effect-free nil))
  (cl-assert (symbolp mode))
  (let ((effective-mode (eproj/resolve-synonym-modes mode)))
    (if-let ((p proj)
             (entry (eproj-project/query-aux-info-entry (eproj-project/aux-info p)
                      'disabled-checkers
                      effective-mode)))
        (let ((res entry))
          (unless (and (listp res)
                       (-all-p #'symbolp res))
            (error "disabled-checkers.%s entry in .eproj-info of %s must be a list of symbols, but got %s"
                   effective-mode
                   (eproj-project/root proj)
                   res))
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
          (unless (--all? (and (symbolp (car it))
                               (null (cddr it)))
                          res)
            (error "local-variables.%s entry in .eproj-info of %s must be an associative list of (<var-name-symbol> <value>) lists, but got %s"
                   effective-mode
                   (eproj-project/root proj)
                   res))
          res)
      default)))

(provide 'eproj-query)

;; Local Variables:
;; End:

;; eproj-query.el ends here
