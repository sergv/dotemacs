;; vim-internals-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 31 May 2024
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 'dash)
(require 'tests-utils)

(require 'vim)
(require 'vim-motions)
(require 'vim-search)
(require 'ert)

(require 'vim-tests)

(defmacro vim-internals-tests--test-fresh-buffer-contents-init (init action contents expected-value)
  (declare (indent 2))
  `(tests-utils--test-buffer-contents
    :action ,action
    :contents ,contents
    :expected-value ,expected-value
    :initialisation ,init
    ;; Don’t reuse buffer to start out in fresh environment each time and don’t
    ;; share things like last cmd events, etc.
    :buffer-id nil))

(defmacro vim-internals-tests--multiple-equivalent-start-positions
    (name action starts result)
  (declare (indent 1))
  `(progn
     ,@(cl-loop
        for start in starts
        append
        (cl-loop
         for mode-and-init in vim-tests--all-known-modes-and-init
         collect
         `(ert-deftest ,(string->symbol (format "%s-%s-%s" name (car mode-and-init) (car start))) ()
            (vim-internals-tests--test-fresh-buffer-contents-init
                (progn
                  ,@(cdr mode-and-init))
              ,action
              ,(cadr start)
              ,result))))))

(vim-internals-tests--multiple-equivalent-start-positions
    vim-internals-tests--substitute-1
  (vim--do-substitute
   (vim-make-motion :begin (point-min)
                    :end (point-max)
                    :has-begin t
                    :type 'linewise)
   (vim-make-pattern
    :regex "foo"
    :whole-line t
    :case-fold 'sensitive)
   "decombobulate"
   nil
   nil)
  ((a
    (tests-utils--multiline
     "_|_foo"
     "bar"
     "baz"
     "quux"))
   (b
    (tests-utils--multiline
     "foo_|_"
     "bar"
     "baz"
     "quux"))
   (c
    (tests-utils--multiline
     "foo"
     "_|_bar"
     "baz"
     "quux"))
   (d
    (tests-utils--multiline
     "foo"
     "bar"
     "baz"
     "_|_quux"))
   (e
    (tests-utils--multiline
     "foo"
     "bar"
     "baz"
     "quux_|_")))
  (tests-utils--multiline
   "decombobulate_|_"
   "bar"
   "baz"
   "quux"))

(vim-internals-tests--multiple-equivalent-start-positions
    vim-internals-tests--substitute-2
  (vim--do-substitute
   (vim-make-motion :begin (point-min)
                    :end (point-max)
                    :has-begin t
                    :type 'linewise)
   (vim-make-pattern
    :regex "foo"
    :whole-line t
    :case-fold 'sensitive)
   "decombobulate"
   nil
   nil)
  ((a
    (tests-utils--multiline
     "_|_foo foo"
     "bar"
     "baz"
     "quux"))
   (b
    (tests-utils--multiline
     "foo_|_ foo"
     "bar"
     "baz"
     "quux"))
   (c
    (tests-utils--multiline
     "foo foo"
     "_|_bar"
     "baz"
     "quux"))
   (d
    (tests-utils--multiline
     "foo foo"
     "bar"
     "baz"
     "_|_quux"))
   (e
    (tests-utils--multiline
     "foo foo"
     "bar"
     "baz"
     "quux_|_")))
  (tests-utils--multiline
   "decombobulate decombobulate_|_"
   "bar"
   "baz"
   "quux"))

(vim-internals-tests--multiple-equivalent-start-positions
    vim-internals-tests--substitute-3
  (vim--do-substitute
   (vim-make-motion :begin (point-min)
                    :end (point-max)
                    :has-begin t
                    :type 'linewise)
   (vim-make-pattern
    :regex "foo"
    :whole-line t
    :case-fold 'sensitive)
   "decombobulate"
   nil
   nil)
  ((a
    (tests-utils--multiline
     "_|_foo foo"
     "bar"
     "baz"
     "quux"
     "foo"))
   (b
    (tests-utils--multiline
     "foo_|_ foo"
     "bar"
     "baz"
     "quux"
     "foo"))
   (c
    (tests-utils--multiline
     "foo foo"
     "_|_bar"
     "baz"
     "quux"
     "foo"))
   (d
    (tests-utils--multiline
     "foo foo"
     "bar"
     "baz"
     "_|_quux"
     "foo"))
   (e
    (tests-utils--multiline
     "foo foo"
     "bar"
     "baz"
     "quux_|_"
     "foo")
    (f
     (tests-utils--multiline
      "foo foo"
      "bar"
      "baz"
      "quux"
      "_|_foo"))
    (g
     (tests-utils--multiline
      "foo foo"
      "bar"
      "baz"
      "quux"
      "foo_|_"))))
  (tests-utils--multiline
   "decombobulate decombobulate"
   "bar"
   "baz"
   "quux"
   "decombobulate_|_"))

(vim-internals-tests--multiple-equivalent-start-positions
    vim-internals-tests--substitute-4
  (vim--do-substitute
   (vim-make-motion :begin (point-min)
                    :end (point-max)
                    :has-begin t
                    :type 'linewise)
   (vim-make-pattern
    :regex "foo"
    :whole-line nil
    :case-fold 'sensitive)
   "decombobulate"
   nil
   nil)
  ((a
    (tests-utils--multiline
     "_|_foo foo"
     "bar"
     "baz"
     "quux"))
   (b
    (tests-utils--multiline
     "foo_|_ foo"
     "bar"
     "baz"
     "quux"))
   (c
    (tests-utils--multiline
     "foo foo"
     "_|_bar"
     "baz"
     "quux"))
   (d
    (tests-utils--multiline
     "foo foo"
     "bar"
     "baz"
     "_|_quux"))
   (e
    (tests-utils--multiline
     "foo foo"
     "bar"
     "baz"
     "quux_|_")))
  (tests-utils--multiline
   "decombobulate_|_ foo"
   "bar"
   "baz"
   "quux"))

(vim-internals-tests--multiple-equivalent-start-positions
    vim-internals-tests--substitute-5
  (vim--do-substitute
   (vim-make-motion :begin (point-min)
                    :end (point-max)
                    :has-begin t
                    :type 'linewise)
   (vim-make-pattern
    :regex "foo"
    :whole-line nil
    :case-fold 'sensitive)
   "decombobulate"
   nil
   nil)
  ((a
    (tests-utils--multiline
     "_|_foo foo"
     "bar"
     "baz"
     "quux"
     "foo"))
   (b
    (tests-utils--multiline
     "foo_|_ foo"
     "bar"
     "baz"
     "quux"
     "foo"))
   (c
    (tests-utils--multiline
     "foo foo"
     "_|_bar"
     "baz"
     "quux"
     "foo"))
   (d
    (tests-utils--multiline
     "foo foo"
     "bar"
     "baz"
     "_|_quux"
     "foo"))
   (e
    (tests-utils--multiline
     "foo foo"
     "bar"
     "baz"
     "quux_|_"
     "foo"))
   (f
    (tests-utils--multiline
     "foo foo"
     "bar"
     "baz"
     "quux"
     "_|_foo"))
   (g
    (tests-utils--multiline
     "foo foo"
     "bar"
     "baz"
     "quux"
     "foo_|_")))
  (tests-utils--multiline
   "decombobulate foo"
   "bar"
   "baz"
   "quux"
   "decombobulate_|_"))

(provide 'vim-internals-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; vim-internals-tests.el ends here
