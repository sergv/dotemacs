;; set-up-environment-variables.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 27 July 2012
;; Description:

(require 'more-scheme)
(require 'cl)

(for-each
 (lambda (entry)
   (destructuring-bind (variable value)
       entry
     (setenv variable value)))
 '(("PATH"
    "/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin/:/home/sergey/.cabal/bin:/home/sergey/projects/lisp/local/bin:/home/sergey/projects/python/pypy/bin:/home/sergey/projects/scheme/local/bin")
   ("LD_LIBRARY_PATH"
    "/home/sergey/projects/python/webcam/collect-data/local/lib/:/usr/local/lib:/usr/lib:/home/sergey/projects/lisp/local/lib:/home/sergey/projects/scheme/local/lib")
   ("PYTHONPATH"
    "/home/sergey/projects/python/modules/:/home/sergey/projects/python/webcam/collect-data/local/lib/python2.7/site-packages:/home/sergey/projects/python/theano/local/lib/python2.7/site-packages/")
   ("INFOPATH"
    "/usr/share/info:/usr/local/share/info:/home/sergey/projects/lisp/local/share/info:/home/sergey/projects/scheme/local/share/info")
   ("MANPATH"
    "/usr/share/man:/usr/local/share/man:/home/sergey/projects/lisp/local/share/man:/home/sergey/projects/scheme/local/share/man")
   ("PKG_CONFIG_PATH"
    "/usr/lib/pkgconfig:/usr/share/pkgconfig:/usr/local/lib/pkgconfig:/usr/local/share/pkgconfig:/home/sergey/projects/scheme/local/lib/pkgconfig/:/home/sergey/projects/scheme/local/share/pkgconfig")
   ("EDITOR"
    "emacs")))

(defun env-var-into-list (env-var list)
  (for-each
   (lambda (dir)
     (add-to-list list dir))
   (split-string (getenv env-var)
                 ":")))

(env-var-into-list "PATH" 'exec-path)


(provide 'set-up-environment-variables)

;; Local Variables:
;; lexical-binding: t
;; End:

;; set-up-environment-variables.el ends here
