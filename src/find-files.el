;; find-files.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 22 February 2016
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'set-up-platform)
  (require 'macro-util)

  (defvar use-foreign-libraries?))

(require 'common)
(require 'dash)

;;;###autoload
(cl-defun find-rec-do (path
                       &key
                       (filep (lambda (_) t))
                       do-not-visitp
                       (file-action #'ignore)
                       (do-not-sort-directory-files t))
  "Call FILE-ACTION on every matching file."
  (declare (pure nil) (side-effect-free nil))
  (letrec ((go
            (lambda (path)
              (cond
                ((file-directory-p path)
                 (when (or (null do-not-visitp)
                           (not (funcall do-not-visitp path)))
                   (mapc go (directory-files path
                                             t ;; produce full names
                                             directory-files-no-dot-files-regexp
                                             do-not-sort-directory-files))))
                ((funcall filep path)
                 (funcall file-action path))
                (t nil)))))
    (funcall go path)))

;;;###autoload
(cl-defun find-rec (path
                    &key
                    (filep (lambda (_) t))
                    do-not-visitp)
  "Collect files and/or directories under PATH recursively.

Collect files and directories which satisfy FILEP and
DIRP respectively in directories which don't satisfy DO-NOT-VISITP.
By default, version-control specific directories are omitted, e.g. .git etc.

All predicates are called with full absolute paths."
  (declare (pure nil) (side-effect-free nil))
  (let* ((accum nil)
         (record-path (lambda (path) (push path accum))))
    (find-rec-do
     path
     :filep filep
     :do-not-visitp do-not-visitp
     :file-action record-path)
    accum))


(defvar find-files/find-program-type
  (or (fold-platform-os-type
       (and (executable-find "find")
            'find)
       nil)
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

(defvar find-rec-backend
  (cond
    (use-foreign-libraries?
     'native)
    (find-files/find-program-type
     'executable)
    (t
     'elisp))
  "Control implementation of `find-rec*'. Valid values: 'native,
  'executable and 'elisp.")

;;;###autoload
(cl-defun find-rec* (&key
                     root
                     globs-to-find
                     ignored-extensions-globs
                     ignored-files-globs
                     ignored-absolute-dirs
                     ignored-directories
                     ignored-directory-prefixes)
  "Optimized `find-rec' that can use system's find executable to speed up
search.

EXTENSIONS-GLOBS - list of globs that match file extensions to search for."
  (declare (pure nil) (side-effect-free nil))
  (when (null globs-to-find)
    (error "No globs to search for under %s" root))
  (cl-assert (listp ignored-directories))
  (cl-assert (listp ignored-directory-prefixes))
  (funcall (pcase find-rec-backend
             (`native
              #'find-rec--rust-native-impl)
             (`executable
              #'find-rec--find-executable-impl)
             (`elisp
              #'find-rec--elisp-impl)
             (invalid
              (error "Invalid find-rec-type: %s" invalid)))
           root
           globs-to-find
           ignored-extensions-globs
           ignored-files-globs
           ignored-absolute-dirs
           ignored-directories
           ignored-directory-prefixes))

(defun find-rec--rust-native-impl (root
                                   globs-to-find
                                   ignored-extensions-globs
                                   ignored-files-globs
                                   ignored-absolute-dirs
                                   ignored-directories
                                   ignored-directory-prefixes)
  "A version of `find-rec' that uses ffi to do the search."
  (declare (pure nil) (side-effect-free nil))
  (let ((ignored-files
         (append ignored-extensions-globs
                 ignored-files-globs)))
    (cl-assert (fboundp #'rust-native-find-rec))
    (let* ((results
           (rust-native-find-rec
            (list root)
            globs-to-find
            ignored-files
            ignored-directories
            ignored-directory-prefixes
            ignored-absolute-dirs))
           (errs (cdr results)))
      (when (< 0 (length errs))
        (dolist (err errs)
          (message "Error during file search: %s" err)))
      (car results))))

(defun find-rec--find-executable-impl (root
                                       globs-to-find
                                       ignored-extensions-globs
                                       ignored-files-globs
                                       ignored-absolute-dirs
                                       ignored-directories
                                       ignored-directory-prefixes)
  "A version of `find-rec' that uses system's find executable to do the search."
  (declare (pure nil) (side-effect-free nil))
  (when (null globs-to-find)
    (error "No globs to search for under %s" root))
  (let* ((ignored-dirs-globs
          (nconc (--map (concat "*/" (strip-trailing-slash it))
                        ignored-directories)
                 (--map (concat "*/" it "*")
                        ignored-directory-prefixes)
                 (--map (strip-trailing-slash it)
                        ignored-absolute-dirs)))
         (ignored-dirs
          (-map (lambda (dir-glob)
                  (list "-path" dir-glob))
                ignored-dirs-globs))
         (ignored-files
          (append
           (-map (lambda (glob)
                   (list (fold-platform-os-type "-name" "-iname") glob))
                 ignored-extensions-globs)
           (pcase find-files/find-program-type
             ((or `find `cygwin-find `busybox)
              (--map (list (fold-platform-os-type "-path" "-ipath") it)
                     ignored-files-globs))
             (_
              (error "find-files/find-program-type has invalid value: %s"
                     find-files/find-program-type)))))
         (to-find (-map (lambda (glob) (list "-name" glob))
                        globs-to-find))
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
                 (-interpose "-o" to-find)
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
                                         (point-max))))))))

(defun find-rec--elisp-impl (root
                             globs-to-find
                             ignored-extensions-globs
                             ignored-files-globs
                             ignored-absolute-dirs
                             ignored-directories
                             ignored-directory-prefixes)
  "An implementation of `find-rec' that does everything via elisp. Thus, it
does not depend on external executables nor native extensions and is used
as a fallback if those are not available."
  (declare (pure nil) (side-effect-free nil))
  (when (null globs-to-find)
    (error "No globs to search for under %s" root))
  (let* ((re-to-find (globs-to-regexp globs-to-find))
         (ignored-files-re (globs-to-regexp ignored-extensions-globs))
         (ignored-files-abs-re (globs-to-regexp ignored-files-globs))
         (ignored-dirs-re
          (globs-to-regexp
           (append (-map #'strip-trailing-slash ignored-directories)
                   (--map (concat it "*") ignored-directory-prefixes))))
         (ignored-absolute-dirs-re
          (when ignored-absolute-dirs
            (concat "\\`\\(?:"
                    (mk-regexp-from-alts ignored-absolute-dirs)
                    "\\)\\(?:/.*\\)?\\'"))))
    (find-rec root
              :filep
              (cond
                ((and ignored-files-re
                      ignored-files-abs-re)
                 (lambda (abs-path)
                   (let ((fname (file-name-nondirectory abs-path)))
                     (and (let ((case-fold-search t))
                            (string-match-p re-to-find fname))
                          (let ((case-fold-search nil))
                            (and (not (string-match-p ignored-files-re fname))
                                 (not (string-match-p ignored-files-abs-re abs-path))))))))
                (ignored-files-abs-re
                 (lambda (abs-path)
                   (let ((fname (file-name-nondirectory abs-path)))
                     (and (let ((case-fold-search t))
                            (string-match-p re-to-find fname))
                          (let ((case-fold-search nil))
                            (not (string-match-p ignored-files-abs-re abs-path)))))))
                (ignored-files-re
                 (lambda (abs-path)
                   (let ((fname (file-name-nondirectory abs-path)))
                     (and (let ((case-fold-search t))
                            (string-match-p re-to-find fname))
                          (let ((case-fold-search nil))
                            (not (string-match-p ignored-files-re fname)))))))
                (t
                 (lambda (abs-path)
                   (let ((fname (file-name-nondirectory abs-path)))
                     (let ((case-fold-search t))
                       (string-match-p re-to-find fname))))))
              :do-not-visitp
              (cond
                ((and ignored-absolute-dirs-re
                      ignored-dirs-re)
                 (lambda (path)
                   (let ((case-fold-search nil))
                     (or (string-match-p ignored-dirs-re
                                         (file-name-nondirectory path))
                         (string-match-p ignored-absolute-dirs-re
                                         path)))))
                (ignored-absolute-dirs-re
                 (lambda (path)
                   (let ((case-fold-search nil))
                     (string-match-p ignored-absolute-dirs-re
                                     path))))
                (ignored-dirs-re
                 (lambda (path)
                   (let ((case-fold-search nil))
                     (string-match-p ignored-dirs-re
                                     (file-name-nondirectory path)))))
                (t
                 #'ignore)))))

(provide 'find-files)

;; Local Variables:
;; End:

;; find-files.el ends here
