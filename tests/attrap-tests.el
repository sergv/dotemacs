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

(cl-defmacro attrap-tests--test-buffer-contents*
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
        collect
        `(ert-deftest ,(string->symbol (format "%s/%s" name mode)) ()
           (tests-utils--test-buffer-contents
            :action ,action
            :contents ,contents
            :expected-value ,expected-value
            :initialisation
            (,mode)
            :post-content-initialisation
            (dolist (err ,flycheck-errors)
              (flycheck-add-overlay err))
            :buffer-id ,(string->symbol (format "attrap-tests-%s" mode)))))))

(defun attrap-tests--run-attrap ()
  (cl-letf
      ;; Set up artificial checker because flycheck is disabled and
      ;; it’s best to not enable it for temporary buffers if we can
      ;; help it.
      (((symbol-function 'flycheck-get-checker-for-buffer)
        (lambda ()
          'haskell-dante)))
    (attrap-flycheck (point))))

(attrap-tests--test-buffer-contents*
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

(provide 'attrap-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; attrap-tests.el ends here
