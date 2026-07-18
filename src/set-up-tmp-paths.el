;; set-up-tmp-paths.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 22 March 2021
;; Description:

(eval-when-compile
  (require 'subr-x)
  (require 'set-up-platform))

(require 'set-up-paths)

(defconst +tmp-path+
  (make-temp-name
   (fold-platform-os-type
    "/tmp/emacs-tmp-"
    (or (when-let (tmpdir (or (getenv "TMPDIR")
                              (getenv "TEMP")))
          (concat tmpdir "\\emacs-tmp-"))
        (concat +prog-data-path+ "/tmp"))))
  "Path to temporary directory specific to current Emacs session.
Other Emacs invocations will not be able to share anything in
here. Contents of this directory may be removed on system
restarts.")

(make-directory +tmp-path+ t)

(setf temporary-file-directory +tmp-path+
      small-temporary-file-directory +tmp-path+
      tramp-auto-save-directory (concat +tmp-path+ "/tramp"))

(defun clean-tmp-path-on-emacs-exit ()
  "Delete `+tmp-path+' directory on emacs exit."
  (ignore-errors
    (delete-directory +tmp-path+
                      t   ;; recurse
                      nil ;; don't just move to trash
                      )))
(add-hook 'kill-emacs-hook #'clean-tmp-path-on-emacs-exit)


(defalias 'strip-trailing-slash 'directory-file-name)

(defmacro add-to-load-path (&rest items)
  (declare (indent 0))
  `(setf load-path
         ,(cl-reduce (lambda (item acc)
                       `(cons ,item ,acc))
                     items
                     :initial-value 'load-path
                     :from-end t)))

(defun find-elisp-dirs (root &optional ignored-dirs-re)
  "Recursively find directories containing elisp files starting at ROOT. Omit
directories whose absolute path matches IGNORED-DIR-RE."
  (let ((standard-ignored-dirs-re
         (rx
          (or (or "CVS"
                  ".svn"
                  ".git"
                  ".hg"
                  ".bzr"
                  "_darcs"
                  ".cask")
              (seq (or "dist"
                       "dist-newstyle"
                       ".cabal-sandbox"
                       ".stack-work")
                   (* not-newline))))))
    (let ((dirs nil))
      (letrec ((collect-dirs
                (lambda (path)
                  (when
                      (and (file-directory-p path)
                           (not
                            (string-match-p standard-ignored-dirs-re
                                            (file-name-nondirectory (strip-trailing-slash path)))))
                    (let ((has-elisp-files? nil))
                      (dolist (p (directory-files path
                                                  t ;; produce full names
                                                  directory-files-no-dot-files-regexp
                                                  t ;; don't sort
                                                  ))
                        (if (file-regular-p p)
                            (when (string-match-p ".*\\.\\(elc?\\|emacs\\)$" p)
                              (setf has-elisp-files? t))
                          (funcall collect-dirs p)))
                      (when (and has-elisp-files?
                                 (or (null ignored-dirs-re)
                                     (not (string-match-p ignored-dirs-re
                                                          path))))
                        (push path dirs)))))))
        (funcall collect-dirs root)
        dirs))))

(defun nconc-after (skip-pred xs ys)
  (let* ((tmp xs)
         (prev nil)
         (result tmp))
    (while (and tmp
                (funcall skip-pred (car tmp)))
      (setf prev tmp
            tmp (cdr tmp)))
    (if prev
        (progn
          (setcdr prev (nconc ys tmp))
          result)
      (nconc ys tmp))))

(defmacro add-to-load-path-recursively (root &optional ignored-dirs-re)
  (declare (indent 1))
  (let* ((prefix-len (length +emacs-config-path+))
         (dirs (mapcar (lambda (dir)
                         (substring dir (+ prefix-len 1)))
                       (find-elisp-dirs (concat +emacs-config-path+ root) (eval ignored-dirs-re)))))
    `(setf load-path
           (nconc-after
            (lambda (x)
              (or (string-prefix-p +emacs-config-path+ x)
                  (string-prefix-p +emacs-compiled-path+ x)))
            load-path
            (mapcar (lambda (dir)
                      (directory-file-name (concat +emacs-config-path+ "/" dir)))
                    (list ,@dirs))))))

(add-to-load-path-recursively "/src")

(add-to-load-path-recursively
    "/third-party"
  ;; Ignored third-party dirs.
  (rx
   (or (seq bow (or "tests" "doc" "examples" ".cask" ".stack-work.*") eol)
       (seq (* anything) "/test" (? "s"))
       "auctex/tests"
       "auctex/style"
       "clojure-mode/test"
       "company-mode/test"
       "dash.el/dev"
       (seq "f.el/" (or "bin" "test"))
       "flycheck-haskell/test"
       (seq "flycheck/" (or ".cask" "test"))
       "ivy/targets"
       "groovy-mode/test"
       (seq "haskell-mode/" (or "doc/gifcasts" "tests" "tests/compat"))
       "ht/test"
       "js2-mode/tests"
       "kotlin-ts-mode/test"
       "lua-mode/test"
       "lsp-mode/test"
       "magit/t"
       "markdown-mode/tests"
       "markdown-mode/scripts"
       "nix-ts-mode/test"
       (seq "org-mode/" (or "mk" "testing"))
       "pkg-info/test"
       "s.el/dev"
       "treepy.el/test"
       "transient/test")))

(when-windows
 (push (concat +emacs-config-path+ "/native/fakecygpty") load-path))

;; (push (concat +emacs-compiled-path+ "/lib") load-path )
;;
;; ;; Shadow everything else
;; (push (concat +emacs-compiled-path+ "/compiled") load-path)
;; (unless (equal (getenv "EMACS_SKIP_ELC") "1")
;;   (push (concat +emacs-compiled-path+ "/compiled/elc") load-path))

(add-to-list 'exec-path +execs-path+)

(provide 'set-up-tmp-paths)

;; Local Variables:
;; End:

;; set-up-tmp-paths.el ends here
