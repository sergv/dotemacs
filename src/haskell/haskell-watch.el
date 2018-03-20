;; haskell-watch.el --- -*- lexical-binding: t; -*-

;; Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  8 March 2018
;; Description:

(require 'datastructures)

(cl-defstruct (haskell-watched-project
               (:conc-name haskell-watched-project/))
  project-root            ;; String - absolute file path.
  registered-buffers      ;; Hash table from buffers to `t'.
  watched-files           ;; Hash table of *.cabal, stack*.yaml and
                          ;; package.yaml files under project-root to `t'.
  registered-file-watches ;; Bimap of identifiers returned by
                          ;; `file-notify-add-watch' to absolute file
                          ;; names being watched over.
  intero-needs-restart    ;; Whether we need to call `intero-restart'
                          ;; next time we'll be performing checks.
  )

(defvar-local haskell-watch--project-root nil)

(defvar haskell-watch--known-projects (make-hash-table :test #'equal)
  "Hash table from absolute project root file paths to
`haskell-watched-project' structures.")

(defvar haskell-watch--registered-file-watchers (make-hash-table :test #'equal)
  "Hash table from identifiers returned by
`file-notify-add-watch' to obsolute directory paths for project
roots (i.e. valid and existing keys within
`haskell-watch--known-projects').")


(defun haskell-watch--get-project-root-for-path (file)
  "Obtain root of a Haskell project that FILE is part of."
  (cl-assert (file-name-absolute-p file))
  (let* ((build-dir-regexp
          (rx bos
              (or "cabal.project"
                  ".cabal.sandbox"
                  (seq "stack" (* nonl) ".yaml"))
              eos))
         (proj-file-regexp
          (rx bos
              (or (seq (+ nonl) ".cabal")
                  "package.yaml")
              eos))
         (search-start (file-name-directory file))
         (lookup-with
          (lambda (filename-regexp)
            (let ((get-files
                   (lambda (dir-name)
                     (directory-files dir-name
                                      nil ;; Relative names.
                                      filename-regexp
                                      t ;; Do not sort.
                                      ))))
              (locate-dominating-file search-start get-files)))))
    (or (funcall lookup-with build-dir-regexp)
        (funcall lookup-with proj-file-regexp))))

(defun haskell-watch-get-project-root ()
  "Get absolute project root for current buffer."
  (unless buffer-file-name
    (error "Haskell watch does not work for temporary buffers: %s"
           (current-buffer)))
  (unless haskell-watch--project-root
    (setf haskell-watch--project-root
          (haskell-watch--get-project-root-for-path buffer-file-name)))
  haskell-watch--project-root)

(defun haskell-watch--find-watched-files (root)
  "Find files under ROOT directory to watch for."
  (cl-assert (file-accessible-directory-p root))
  (seq-sort #'string<
            (find-rec*
             :root root
             :extensions-globs '("*.cabal" "package.yaml" "stack*.yaml")
             :ignored-directories +ignored-directories+
             :ignored-directory-prefixes +ignored-directory-prefixes+)))


(defun haskell-watch--construct-project-for-root (root)
  "Make up a `haskell-watched-project' structure for a project at ROOT."
  (setf root (expand-file-name root))
  (cl-assert (file-name-absolute-p root))
  (cl-assert (file-accessible-directory-p root))
  (let ((watched-files (haskell-watch--find-watched-files root))
        (registered-file-watches (bimap-empty)))
    (unless watched-files
      (error "Failed to find any files to watch for in project %s" root))
    (let ((proj
           (make-haskell-watched-project
            :project-root root
            :registered-buffers (make-hash-table :test #'equal) ;; Hash table from buffers to `t'.
            :watched-files (make-hash-table :test #'equal)
            :registered-file-watches registered-file-watches
            :intero-needs-restart nil)))
      (puthash root proj haskell-watch--known-projects)
      (dolist (file watched-files)
        (haskell-watch--watch-file! proj file))
      proj)))

(defun haskell-watch--watch-file! (proj file)
  (let ((descriptor (file-notify-add-watch file '(change)
                                           #'haskell-watch--on-watched-file-changed)))
    (puthash file t (haskell-watched-project/watched-files proj))
    (bimap-insert descriptor file (haskell-watched-project/registered-file-watches proj))
    (cl-assert (gethash (haskell-watched-project/project-root proj)
                        haskell-watch--known-projects)
               nil
               "Project at %s is not registered in `haskell-watch--known-projects'."
               (haskell-watched-project/project-root proj))
    ;; Register in the global table of descriptors.
    (puthash descriptor (haskell-watched-project/project-root proj) haskell-watch--registered-file-watchers)))

(defun haskell-watch--unwatch-file (proj file)
  (remhash file (haskell-watched-project/watched-files proj))
  (let ((descriptor (bimap-lookup-reverse file
                                          (haskell-watched-project/registered-file-watches proj))))
    (bimap-delete file (haskell-watched-project/registered-file-watches proj))
    (remhash descriptor haskell-watch--registered-file-watchers)))

(defun haskell-watch--on-watched-file-changed (event)
  (let* ((descriptor (car event))
         (action (cadr event))
         (rest (cddr event))
         (file (car rest))
         (new-file (cadr-safe rest))
         (proj-root (gethash descriptor haskell-watch--registered-file-watchers)))
    (cl-assert proj-root
               nil
               "File descriptor %s over file %s is not registered within `haskell-watch--registered-file-watchers'"
               descriptor
               file)
    (let ((proj (gethash proj-root haskell-watch--known-projects)))
      (cl-assert proj-root
                 nil
                 "No project with root %s"
                 proj-root)
      (pcase action
        ('changed
         (haskell-watch--mark-project-as-dirty proj))
        ((or 'deleted 'stopped)
         (haskell-watch--unwatch-file proj file))
        ((or 'created 'attribute-changed)
         ;; It was not present but now is - just continue watching over it.
         )
        ('renamed
         (cl-assert (not (null new-file)))
         (haskell-watch--watch-file! proj new-file)
         ;; unwatch old file
         (file-notify-rm-watch descriptor))))))

(defun haskell-watch--subscribe-buffer-for-config-file-updates (buf proj)
  "Subscribe BUF to be marked dirty whenever config files (e.g. cabal files) of
project PROJ change."
  (puthash buf t (haskell-watched-project/registered-buffers proj)))


(defvar-local haskell-watch--buffer-project-is-dirty? nil
  "Whether project of current buffer had it's configuration (e.g. cabal file)
modified and we should reconfigure the project.")

(defvar-local haskell-watch--project-for-buffer nil
  "Project this buffer is assigned to.")

;; TODO for 2018-03-13: make flycheck's state dirty in the target buffers,
;; so that when
(defun haskell-watch--mark-project-as-dirty (proj)
  (setf (haskell-watched-project/intero-needs-restart proj) t)
  (maphash (lambda (buf _ignored)
             (declare (ignore _ignored))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq-local haskell-watch--buffer-project-is-dirty? t)
                 (setq-local haskell-watch--project-for-buffer proj)
                 ;; Schedule a check next time we visit the buffer.
                 (flycheck-buffer-automatically
                  nil
                  t ;; Force a deferred check.
                  ))))
           (haskell-watched-project/registered-buffers proj)))

(defun haskell-watch--refresh-config-if-needed ()
  (when haskell-watch--buffer-project-is-dirty?
    (cond
      ((and intero-mode
            (haskell-watched-project/intero-needs-restart haskell-watch--project-for-buffer))
       (intero-restart)
       (setf (haskell-watched-project/intero-needs-restart haskell-watch--project-for-buffer) nil))
      (flycheck-mode
       (flycheck-haskell-configure)))
    (setf haskell-watch--buffer-project-is-dirty? nil)))

(add-hook 'flycheck-before-syntax-check-hook
          #'haskell-watch--refresh-config-if-needed)


(defun haskell-watch-get-project (root)
  (aif (gethash root haskell-watch--known-projects)
      it
    (let ((proj (haskell-watch--construct-project-for-root root)))
      (puthash root proj haskell-watch--known-projects)
      proj)))

;;;###autoload
(defun haskell-watch-register-current-buffer! ()
  (when (buffer-file-name)
    (haskell-watch--subscribe-buffer-for-config-file-updates
     (current-buffer)
     (let ((root (haskell-watch-get-project-root)))
       (if root
           (haskell-watch-get-project root)
         (error "Failed to find project root for buffer %s" (current-buffer)))))))

(provide 'haskell-watch)

;; Local Variables:
;; End:

;; haskell-watch.el ends here
