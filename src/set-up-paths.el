;; set-up-paths.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  6 November 2012
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'set-up-platform)
(require 'set-up-environment-variables)

(defconst +emacs-config-path+
  (find-if (lambda (p) (and (file-accessible-directory-p p)))
           (list
            (concat (platform-dependent-root) "/emacs")
            (concat (platform-dependent-root) "/.emacs.d")))
  "Path to root for emacs configuration.")

(when (or (null +emacs-config-path+)
          (not (stringp +emacs-config-path+)))
  (error "No accessible directory found for +emacs-config-path+"))

(defconst +emacs-standalone-path+
  (concat +emacs-config-path+ "/standalone")
  "Path to directory with standalone packages.")

(defconst +prog-data-path+
  (concat +emacs-config-path+ "/prog-data")
  "Path to directory with programs's auxiliary files.")

(defconst +execs-path+
  ;; note: execs are somewhat external to emacs, so it's
  ;; reasonable for them to be somewhere outside
  (find-if (lambda (p) (and p (file-directory-p p)))
           (list
            (concat +emacs-config-path+ "/../execs")
            (concat +emacs-config-path+ "/execs")
            (let ((emacs-exec (executable-find "emacs")))
              (when emacs-exec
                (file-name-directory emacs-exec)))))
  "Path to directory with programs executables files.")

(defconst +tmp-path+ (make-temp-name (if (platform-os-type? 'windows)
                                         (concat +prog-data-path+ "/tmp")
                                       "/tmp/emacs-tmp-"))
  "Path to temporary directory, contents of which may be removed on
system restars.")

(defconst +tmp-global-path+ temporary-file-directory
  "Path to temporary files that are visible across different emacs instances.")

(make-directory +tmp-path+ t)

(setf temporary-file-directory +tmp-path+
      small-temporary-file-directory +tmp-path+
      tramp-auto-save-directory (concat +prog-data-path+ "/tramp"))

(defun remove-tmp-path ()
  "Delete `+tmp-path+' directory on emacs exit."
  (ignore-errors
    (delete-directory +tmp-path+
                      t   ;; recurse
                      nil ;; don't just move to trash
                      )))
(add-hook 'kill-emacs-hook #'remove-tmp-path)

(defalias 'strip-trailing-slash 'directory-file-name)

(defmacro add-to-load-path (&rest items)
  (declare (indent 0))
  `(setf load-path
         ,(reduce (lambda (item acc)
                    `(cons ,item ,acc))
                  items
                  :initial-value 'load-path
                  :from-end t)))

(defun find-elisp-dirs (root &optional ignored-dirs-re)
  "Recursively find directories containing elisp files starting at ROOT. Omit
directories whose absolute path matches IGNORED-DIR-RE."
  (let ((ignored-dirs
         '("CVS" ".svn" ".git" ".hg" ".bzr" "_darcs"
           ".cabal-sandbox" "dist" ".stack-work"
           ".cask")))
    (let ((dirs nil))
      (letrec ((collect-dirs
                (lambda (path)
                  (when
                      (and (file-directory-p path)
                           (not
                            (member (file-name-nondirectory (strip-trailing-slash path))
                                    ignored-dirs)))
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
       "ghc-mod/doc/presentation/auto"
       (seq "ghc-mod/.stack-work" (* any))
       (seq "haskell-mode/" (or "doc/gifcasts" "tests" "tests/compat"))
       "js2-mode/tests"
       "lua-mode/test"
       "magit/t"
       "markdown-mode/tests"
       "mmm-mode/tests"
       (seq "org-mode/" (or "mk" "testing"))
       "s.el/dev"
       "smartparens/test"
       "smartparens/tests")))

(add-to-load-path-recursively "/src")

(add-to-load-path-recursively
 "/third-party"
 set-up-paths--ignored-third-party-el-dirs-re)

;; this must go to the end in order to give files in /src dir a chance
;; (add-to-list 'load-path +bytecode-lib+ t)

(add-to-list 'exec-path +execs-path+)

(provide 'set-up-paths)

;; Local Variables:
;; End:

;; set-up-paths.el ends here
