;; haskell-cabal-components.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  6 October 2025
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'subr-x)
  (require 'macro-util)
  (require 'set-up-platform)
  (require 'treesit-utils)
  (require 'trie))

(cl-defstruct (cabal-component
               (:conc-name cabal-component/))
  type
  name
  main-file
  module-list
  source-dirs
  build-dir)

(defun cabal-component-get-cabal-target (component)
  (concat (cabal-component/type component) ":" (cabal-component/name component)))

(defun parse-cabal-component (entry)
  "Turn

(<component type> <component name> <main file> <module list> <source dirs> <build-dir>)

into

‘cabal-component’ struct."
  (let ((type (car entry))
        (name (cadr entry))
        (main-file (caddr entry))
        (module-list (cadddr entry))
        (source-dirs (car (cddddr entry)))
        (build-dir (cadr (cddddr entry))))
    (cl-assert (stringp type))
    (cl-assert (stringp name))
    (cl-assert (or (string= type "lib")
                   (string= type "flib")
                   (stringp main-file)))
    (cl-assert (listp module-list))
    (cl-assert (-all? #'listp module-list))
    (cl-assert (-all? (lambda (xs) (-all? #'stringp xs)) module-list))
    (cl-assert (listp source-dirs))
    (cl-assert (-all? #'stringp source-dirs))
    (cl-assert (stringp build-dir))
    (make-cabal-component
     :type type
     :name name
     :main-file main-file
     :module-list module-list
     :source-dirs source-dirs
     :build-dir build-dir)))

(provide 'haskell-cabal-components)

;; Local Variables:
;; End:

;; haskell-cabal-components.el ends here
