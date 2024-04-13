;; attrap-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 13 April 2024
;; Description:

(require 'attrap)

(require 'common)
(require 'ert)
(require 'tests-utils)
(require 'haskell-autoload)
(require 'haskell-setup)
(require 'flycheck)

(cl-defmacro attrap-tests--test-buffer-contents-multi
    (&key
     name
     flycheck-errors
     action
     contents
     expected-value
     (modes '(haskell-mode haskell-ts-mode)))
  `(progn
     ,@(cl-loop
        for mode in modes
        append
        (cl-loop
         for content-entry in contents
         collect
         (cl-destructuring-bind (subname content) content-entry
           `(ert-deftest ,(string->symbol (concat (format "%s/%s" name mode)
                                                  (when subname
                                                    (format "/%s" subname))))
                ()
              (tests-utils--test-buffer-contents
               :action ,action
               :contents ,content
               :expected-value ,expected-value
               :initialisation
               (,mode)
               :post-content-initialisation
               (dolist (err ,flycheck-errors)
                 (flycheck-add-overlay err))
               :buffer-id ,(string->symbol (format "attrap-tests-%s" mode)))))))))

(cl-defmacro attrap-tests--test-buffer-contents-one
    (&key
     name
     flycheck-errors
     action
     contents
     expected-value
     (modes '(haskell-mode haskell-ts-mode)))
  `(attrap-tests--test-buffer-contents-multi
     :name ,name
     :flycheck-errors ,flycheck-errors
     :action ,action
     :contents ,(list (list nil contents))
     :expected-value ,expected-value
     :modes ,modes))

(defun attrap-tests--run-attrap ()
  (cl-letf
      ;; Set up artificial checker because flycheck is disabled and
      ;; it’s best to not enable it for temporary buffers if we can
      ;; help it.
      (((symbol-function 'flycheck-get-checker-for-buffer)
        (lambda ()
          'haskell-dante)))
    (attrap-flycheck (point))))

(attrap-tests--test-buffer-contents-one
 :name attrap/haskell-dante/delete-module-import-1
 :flycheck-errors
 (list
  (let ((linecol (save-excursion
                   (re-search-forward "_|_")
                   (flycheck-line-column-at-pos (point)))))
    (flycheck-error-new
     :line (car linecol)
     :column (cdr linecol)
     :buffer (current-buffer)
     :checker 'haskell-dante
     :message
     (tests-utils--multiline
      "warning: [GHC-66111] [-Wunused-imports]"
      "    The import of ‘Foo.Bar.Baz’ is redundant"
      "      except perhaps to import instances from ‘Foo.Bar.Baz’"
      "    To import instances alone, use: import Foo.Bar.Baz()")
     :level 'warning
     :id nil
     :group nil)))
 :action
 (attrap-tests--run-attrap)
 :contents
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Foo.Bar.Baz"
  ""
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_"
  ""))

(attrap-tests--test-buffer-contents-multi
 :name attrap/haskell-dante/delete-import-1
 :flycheck-errors
 (list
  (let ((linecol (save-excursion
                   (re-search-forward "_|_")
                   (flycheck-line-column-at-pos (point)))))
    (flycheck-error-new
     :line (car linecol)
     :column (cdr linecol)
     :buffer (current-buffer)
     :checker 'haskell-dante
     :message
     (tests-utils--multiline
      "warning: [GHC-38856] [-Wunused-imports]"
      "    The import of ‘depPkgName, unPackageName’"
      "    from module ‘Distribution.Package’ is redundant")
     :level 'warning
     :id nil
     :group nil)))
 :action
 (attrap-tests--run-attrap)
 :contents
 ((a
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Distribution.Package (unPackageName, depPkgName, PackageName)"
    ""
    ""))
  (b
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Distribution.Package (unPackageName, PackageName, depPkgName)"
    ""
    ""))
  (c
   (tests-utils--multiline
    ""
    "import Quux"
    "_|_import Distribution.Package (PackageName, depPkgName,unPackageName)"
    ""
    "")))
 :expected-value
 (tests-utils--multiline
  ""
  "import Quux"
  "_|_import Distribution.Package (PackageName)"
  ""))

(provide 'attrap-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; attrap-tests.el ends here
