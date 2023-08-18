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

(when-emacs-version (and (<= 25 it) (<= it 27))
  (setf ediff-temp-file-prefix (concat +tmp-path+ "/ediff")))

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

(defmacro add-to-load-path-recursively (root &optional ignored-dirs-re)
  (let* ((prefix-len (length +emacs-config-path+))
         (dirs (mapcar (lambda (dir)
                         `(concat +emacs-config-path+ ,(concat "/" dir)))
                       (mapcar (lambda (dir)
                                 (substring dir (+ prefix-len 1)))
                               (find-elisp-dirs (concat +emacs-config-path+ root) (eval ignored-dirs-re))))))
    `(add-to-load-path ,@dirs)))

(defvar set-up-paths--ignored-third-party-el-dirs-re
  (rx
   (or (seq bow (or "tests" "doc" "examples" ".cask" ".stack-work.*") eol)
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
       "lua-mode/test"
       "magit/t"
       "markdown-mode/tests"
       "mmm-mode/tests"
       (seq "org-mode/" (or "mk" "testing"))
       "s.el/dev")))

(add-to-load-path-recursively "/src")

(add-to-load-path-recursively
 "/third-party"
 set-up-paths--ignored-third-party-el-dirs-re)

(add-to-load-path (concat +emacs-config-path+ "/native/fakecygpty"))

;; this must go to the end in order to give files in /src dir a chance
;; (add-to-list 'load-path +bytecode-lib+ t)

(add-to-list 'load-path (concat +emacs-config-path+ "/lib"))

;; Shadow everything else
(push (concat +emacs-config-path+ "/compiled") load-path)

(add-to-list 'exec-path +execs-path+)


(provide 'set-up-tmp-paths)

;; Local Variables:
;; End:

;; set-up-tmp-paths.el ends here
