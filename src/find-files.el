;; find-files.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 22 February 2016
;; Description:

(require 'dash)

(defun* find-rec-do (path
                     &key
                     (filep (lambda (p) t))
                     (dirp  (lambda (p) nil))
                     do-not-visitp
                     ;; (lambda (p)
                     ;;   (version-control-directory?
                     ;;    (file-name-nondirectory p)))

                     (file-action #'ignore)
                     (directory-action #'ignore)
                     (do-not-sort-directory-files t))
  "Call FILE-ACTION on every matching file and DIRECTORY-ACTION on every matching
folder."
  (letrec ((go
            (lambda (path)
              (cond
                ((file-directory-p path)
                 (unless (funcall do-not-visitp path)
                   (mapc go (directory-files path
                                             t ;; produce full names
                                             directory-files-no-dot-files-regexp
                                             do-not-sort-directory-files))))
                ((funcall filep path)
                 (funcall file-action path))
                (t nil)))))
    (funcall go path)))

(defun* find-rec (path
                  &key
                  (filep (lambda (p) t))
                  (dirp  (lambda (p) nil))
                  do-not-visitp)
  "Collect files and/or directories under PATH recursively.

Collect files and directories which satisfy FILEP and
DIRP respectively in directories which don't satisfy DO-NOT-VISITP.
By default, version-control specific directories are omitted, e.g. .git etc.

All predicates are called with full absolute paths."
  (let* ((accum nil)
         (record-path (lambda (path) (push path accum))))
    (find-rec-do
     path
     :filep filep
     :dirp dirp
     :do-not-visitp do-not-visitp
     :file-action record-path
     :directory-action record-path)
    accum))


(defvar find-files/find-program-type
  (or (and (not (platform-os-type? 'windows))
           (executable-find "find")
           'find)
      (and (executable-find "busybox")
           'busybox))
  "Type of find program that `find-files/find-program-executable' refers to.
Valid values are:
'find        - vanilla gnu find
'cygwin-find - cygwin version of gnu find, requires special quoting due to
               Windows and libcygwin*.dll
'busybox     - find as found in busybox, does not have as many options as gnu
               find.")

(defvar find-files/find-program-executable
  (pcase find-files/find-program-type
    ((or `find `cygwin-find) "find")
    (`busybox "busybox")))

(defun* find-rec* (&key
                   root
                   extensions-globs
                   ignored-extensions-globs
                   ignored-files-absolute-regexps
                   ignored-directories
                   ignored-directory-prefixes)
  "Optimized `find-rec' that can use system's find executable to speed up
search.

EXTENSIONS-GLOBS - list of globs that match file extensions to search for."
  (when (null extensions-globs)
    (error "no extensions globs for project %s" root))
  (let ((ignored-dirs-globs
         (nconc (-map (lambda (dir) (concat "*/" dir))
                      ignored-directories)
                (-map (lambda (dir) (concat "*/" dir "*"))
                      ignored-directory-prefixes))))
    (if find-files/find-program-type
      (let* ((ignored-dirs
              (-map (lambda (dir-glob) (list "-ipath" dir-glob))
                    ignored-dirs-globs))
             (ignored-files
              (append
               (-map (lambda (glob)
                       (list "-iname" glob))
                     ignored-extensions-globs)
               (-map (pcase find-files/find-program-type
                       ((or `find `cygwin-find)
                        (lambda (re) (list "-iregex" re)))
                       (`busybox
                        (lambda (re) (list "-regex" re)))
                       (_
                        (error "find-files/find-program-type has invalid value: %s"
                               find-files/find-program-type)))
                     ignored-files-absolute-regexps)))
             (exts (-map (lambda (glob) (list "-iname" glob))
                         extensions-globs))
             (find-cmd (or find-files/find-program-executable
                           (error "find-files/find-program-type has invalid value: %s"
                                  find-files/find-program-type)))
             (cmd
              (-flatten
               (list (when (eq 'busybox find-files/find-program-type)
                       "find")
                     "-L"
                     (when (memq find-files/find-program-type '(find cygwin-find))
                       "-O3")
                     root
                     (when (memq find-files/find-program-type '(find cygwin-find))
                       '("-regextype" "emacs"))
                     (when ignored-dirs
                       (list
                        "-type" "d"
                        "("
                        (-interpose "-o" ignored-dirs)
                        ")"
                        "-prune"
                        "-o"))
                     (when ignored-files
                       (list
                        "-type" "f"
                        "("
                        (-interpose "-o" ignored-files)
                        ")"
                        "-prune"
                        "-o"))
                     "-type" "f"
                     "("
                     (-interpose "-o" exts)
                     ")"
                     "-print")))
             (w32-quote-process-args
              (if (boundp 'w32-quote-process-args)
                (pcase find-files/find-program-type
                  (`cygwin-find ?\\)
                  (_ w32-quote-process-args))
                nil)))
        (with-temp-buffer
          (with-disabled-undo
           (with-inhibited-modification-hooks
            (apply #'call-process
                   find-cmd
                   nil
                   t
                   nil
                   cmd)
            (split-into-lines
             (buffer-substring-no-properties (point-min)
                                             (point-max)))))))
      (let* ((ext-re (globs-to-regexp extensions-globs))
             (ignored-files-re (globs-to-regexp ignored-extensions-globs))
             (ignored-files-absolute-re
              (mk-regexp-from-alts ignored-files-absolute-regexps))
             (ignored-files-all-re
              (mk-regexp-from-alts
               (remq nil
                     (list ignored-files-re
                           ignored-files-absolute-re))))
             (ignored-dirs-re
              (globs-to-regexp ignored-dirs-globs)))
        (find-rec root
                  :filep
                  (if ignored-files-all-re
                    (lambda (path)
                      (and (string-match-p ext-re path)
                           (not (string-match-p ignored-files-all-re path))))
                    (lambda (path)
                      (string-match-p ext-re path)))
                  :do-not-visitp
                  (lambda (path)
                    (string-match-p ignored-dirs-re
                                    (file-name-nondirectory path))))))))

(provide 'find-files)

;; Local Variables:
;; End:

;; find-files.el ends here
