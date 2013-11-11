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

(when (null +emacs-config-path+)
  (error "No accessible directory found for +emacs-config-path+"))

(defconst +emacs-standalone-path+
  (concat +emacs-config-path+ "/standalone")
  "Path to directory with standalone packages.")

(defconst +emacs-standalone-elc-path+
  (concat +emacs-standalone-path+ "/local")
  "Path to directory with compiled files of standalone packages.")

(defconst +emacs-documentation-path+
  (concat +emacs-config-path+ "/doc")
  "Path to directory with offline documentation.")

(defconst +prog-data-path+
  (concat +emacs-config-path+ "/prog-data")
  "Path to directory with programs's auxiliary files.")

(defconst +execs-path+
  (concat +emacs-config-path+ "/execs")
  "Path to directory with programs executables files.")

(defconst +color-themes-path+
  (concat +prog-data-path+ "/themes")
  "Path to color themes")

(defconst +slime-path+
  (concat +emacs-standalone-path+ "/slime" ;;"-2012-01-15"
          ))

(defconst +tmp-path+ (make-temp-name (if (platform-os-type? 'windows)
                                       (concat +prog-data-path+ "/tmp")
                                       "/tmp/emacs-tmp-"))
  "Path to temporary directory, contents of which may be removed on
system restars.")

(make-directory +tmp-path+ t)

(setf temporary-file-directory +tmp-path+
      small-temporary-file-directory +tmp-path+
      tramp-auto-save-directory (concat +prog-data-path+ "/tramp"))


(defalias 'strip-trailing-slash 'directory-file-name)

(defun %emacs-boot--get-directory-contents (dir)
  (remove-if (lambda (x) (member (file-name-nondirectory x) '("." "..")))
             (directory-files dir t)))

(defun* %emacs-boot--find-rec-special
    (path
     &key
     (filep (lambda (p) t))
     (dirp  (lambda (p) nil))
     (do-not-visitp
      (lambda (p)
        (member* (file-name-nondirectory (strip-trailing-slash p))
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
            (lambda (p) (string-match-p regular-expression p)))))
  (when (stringp dirp)
    (setf dirp
          (let ((regular-expression dirp))
            (lambda (p) (string-match-p regular-expression p)))))
  (when (stringp do-not-visitp)
    (setf do-not-visitp
          (let ((regular-expression do-no-visitp))
            (lambda (p) (string-match-p regular-expression p)))))

  (letrec ((collect-rec
            (lambda (path accum)
              (cond
                ((and (file-directory-p path)
                      (not (funcall do-not-visitp path)))
                 (reduce (lambda (acc p)
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
                                       :filep (lambda (x) nil)
                                       :dirp (lambda (x) t))
        (%emacs-boot--find-rec-special (concat +emacs-config-path+ "/third-party")
                                       :filep (lambda (x) nil)
                                       :dirp (lambda (x) t))
        load-path)
       :test #'string=))

;; this must go to the end in order to give files in /src dir a chance
;; (add-to-list 'load-path +bytecode-lib+ t)
(add-to-list 'load-path +color-themes-path+)


(provide 'set-up-paths)

;; Local Variables:
;; End:

;; set-up-paths.el ends here
