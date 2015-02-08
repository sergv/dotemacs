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

(defconst +emacs-documentation-path+
  (concat +emacs-config-path+ "/doc")
  "Path to directory with offline documentation.")

(defconst +prog-data-path+
  (concat +emacs-config-path+ "/prog-data")
  "Path to directory with programs's auxiliary files.")

(defconst +execs-path+
  ;; note: execs are somewhat external to emacs, so it's
  ;; reasonable for them to be somewhere outside
  (find-if #'file-directory-p
   (list
    (concat +emacs-config-path+ "/../execs")
    (concat +emacs-config-path+ "/execs")
    (file-name-directory (executable-find "emacs"))))
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

(defun remove-tmp-path ()
  "Delete `+tmp-path+' directory on emacs exit."
  (ignore-errors
    (delete-directory +tmp-path+
                      t   ;; recurse
                      nil ;; don't just move to trash
                      )))
(add-hook 'kill-emacs-hook #'remove-tmp-path)

(defalias 'strip-trailing-slash 'directory-file-name)

(let ((%emacs-boot--find-rec-special
        (lambda (path)
          (let ((%emacs-boot--get-directory-contents
               (lambda (dir)
                 (remove-if (lambda (x) (member (file-name-nondirectory x) '("." "..")))
                            (directory-files dir t))))
              (do-not-visitp
               (lambda (p)
                 (member* (file-name-nondirectory (strip-trailing-slash p))
                          '("SCCS" "RCS" "CVS" "MCVS" ".svn"
                            ".git" ".hg" ".bzr" "_MTN" "_darcs"
                            "{arch}")
                          :test #'string=))))
          (letrec ((collect-rec
                    (lambda (path accum)
                      (cond
                        ((and (file-directory-p path)
                              (not (funcall do-not-visitp path)))
                         (reduce (lambda (acc p)
                                   (funcall collect-rec p acc))
                                 (funcall %emacs-boot--get-directory-contents path)
                                 :initial-value (cons path accum)))
                        (t
                         accum)))))
            (funcall collect-rec path nil))))))
  (setf load-path
        (remove-duplicates
         (append
          (funcall %emacs-boot--find-rec-special
                   (concat +emacs-config-path+ "/src"))
          (funcall %emacs-boot--find-rec-special
                   (concat +emacs-config-path+ "/third-party"))
          load-path)
         :test #'string=)))

;; this must go to the end in order to give files in /src dir a chance
;; (add-to-list 'load-path +bytecode-lib+ t)
(add-to-list 'load-path +color-themes-path+)

(provide 'set-up-paths)

;; Local Variables:
;; End:

;; set-up-paths.el ends here
