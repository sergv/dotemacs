;; set-up-paths.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  6 November 2012
;; Description:

(require 'set-up-platform)
(require 'set-up-environment-variables)

(eval-when-compile
 (require 'cl))


(defconst +emacs-config-path+
  (concat (platform-dependent-root) "/emacs")
  "Path to folder which is the root for emacs configuration.")

(defconst +emacs-standalone-path+
  (concat +emacs-config-path+ "/standalone")
  "Path to folder which is the root for emacs configuration.")

(defconst +emacs-documentation-path+
  (concat +emacs-config-path+ "/doc")
  "Path to folder which is the root for emacs configuration.")

(defconst +prog-data-path+
  (concat +emacs-config-path+ "/prog-data")
  "Path to folder where programs can store their auxiliary files")

(defconst +bytecode-lib+
  (concat +emacs-config-path+ "/lib")
  "Path to *.elc files")

(defconst +color-themes-path+
  (concat +prog-data-path+ "/themes")
  "Path to color themes")

(defconst +slime-path+
  (concat +emacs-standalone-path+ "/slime" ;;"-2012-01-15"
          ))

(defconst +tmp-path+
  (concat +prog-data-path+ "/tmp")
  "Path to temporary directory, contents of which may be removed on
system restars.")


(defun %emacs-boot--strip-trailing-slash (path)
  (if (char-equal ?\/ (aref path (1- (length path))))
    (subseq path 0 -1)
    path))

(defun %emacs-boot--get-directory-contents (dir)
  (remove-if #'(lambda (x) (member (file-name-nondirectory x) '("." "..")))
             (directory-files dir t)))

(defun* %emacs-boot--find-rec-special (path
                                       &key
                                       (filep #'(lambda (p) t))
                                       (dirp  #'(lambda (p) nil))
                                       (do-not-visitp
                                           #'(lambda (p)
                                               (member* (file-name-nondirectory (%emacs-boot--strip-trailing-slash p))
                                                        '("SCCS" "RCS" "CVS" "MCVS" ".svn"
                                                          ".git" ".hg" ".bzr" "_MTN" "_darcs"
                                                          "{arch}")
                                                        :test #'string=))))
  "Collect files and/or directories under PATH recursively.

Collect files and directories which satisfy FILEP and
DIRP respectively in directories which don't satisfy DO-NOT-VISITP.
By default, version-control specific directories are omitted, e.g. .git etc."
  (when (stringp filep)
    (setf filep
          (let ((regular-expression filep))
            #'(lambda (p) (string-match-p regular-expression p)))))
  (when (stringp dirp)
    (setf dirp
          (let ((regular-expression dirp))
            #'(lambda (p) (string-match-p regular-expression p)))))
  (when (stringp do-not-visitp)
    (setf do-not-visitp
          (let ((regular-expression do-no-visitp))
            #'(lambda (p) (string-match-p regular-expression p)))))

  (letrec ((collect-rec
             (lambda (path accum)
               (cond
                 ((and (file-directory-p path)
                       (not (funcall do-not-visitp path)))
                  (reduce #'(lambda (acc p)
                              (funcall collect-rec p acc))
                          (%emacs-boot--get-directory-contents path)
                          :initial-value (if (funcall dirp path)
                                           (cons path accum)
                                           accum)))
                 ((funcall filep path)
                  (cons path accum))
                 (t
                  accum)))))
    (funcall collect-rec path nil)))

(setf load-path
      (remove-duplicates
       (append
        (%emacs-boot--find-rec-special (concat +emacs-config-path+ "/src")
                                       :filep #'(lambda (x) nil)
                                       :dirp #'(lambda (x) t))
        (%emacs-boot--find-rec-special (concat +emacs-config-path+ "/third-party")
                                       :filep #'(lambda (x) nil)
                                       :dirp #'(lambda (x) t))
        load-path)
       :test #'string=))

;; this must go to the end in order to give files in /src dir a chance
(add-to-list 'load-path +bytecode-lib+ t)
(add-to-list 'load-path +color-themes-path+)


(provide 'set-up-paths)

;; Local Variables:
;; End:

;; set-up-paths.el ends here
