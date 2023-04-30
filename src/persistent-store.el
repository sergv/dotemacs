;; persistent-store.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'subr-x))

(require 'common)
(require 'set-up-paths)

;;; database stores all info in single file, but facilities
;;; for dealing with multiple files are provided

(defvar persistent-store-content nil
  "Current contents of the database. A hash table with :test type being 'eq.")

(defvar persistent-store-store-file
  (path-concat +prog-data-path+ "persistent-store")
  "Filename of database store file")

(defvar persistent-store-backup-file
  (concat persistent-store-store-file ".bak")
  "Filename of backup database store file")

(defvar persistent-store-loaded-content nil
  "Contents of `persistent-store-store-file' than was used to set up
`persistent-store-content'.")


(defun persistent-store-init ()
  "Initialize database."
  (unless (persistent-store-initialized?)
    (add-hook 'kill-emacs-hook #'persistent-store-flush-database)
    (persistent-store-load-contents)))

(defsubst persistent-store-initialized? ()
  (not (null persistent-store-content)))

;;;###autoload
(defun persistent-store-put (key value)
  "Store entry in database."
  (when (persistent-store-initialized?)
    (puthash key value persistent-store-content)))

;;;###autoload
(defun persistent-store-get (key &optional default)
  "Retrieve entry from database."
  (gethash key persistent-store-content default))

(defsetf persistent-store-get (key) (value)
  `(setf (gethash ,key persistent-store-content) ,value))

(defsubst persistent-store-remove (key)
  "Remove entry from database."
  (remhash key persistent-store-content))

(defun persistent-store-load-file (filename)
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun persistent-store-load-contents ()
  "Load database contents from file."
  (let* ((new-file-content (persistent-store-load-file persistent-store-store-file))
         (new-content
          (condition-case nil
              (persistent-store-read-contents-from-string new-file-content)
            (error
             (error "Failed to read persistent-store's contents from file %s"
                    persistent-store-store-file)))))
    (setf persistent-store-loaded-content new-file-content
          persistent-store-content new-content)))

(defun persistent-store-read-contents-from-string (str)
  (condition-case nil
      (let ((tbl (make-hash-table :test #'eq)))
        (dolist (entry (read str))
          (puthash (car entry) (cdr entry) tbl))
        tbl)
    (error
     (error "Failed to read persistent-store's contents from string"))))

(defun persistent-store-write-contents-to-file (contents filename)
  (with-temp-buffer
    (goto-char (point-min))
    (let ((print-level nil)
          (print-length nil))
      ;; (print new-content (current-buffer))
      (pp contents (current-buffer))
      (insert "\n\n;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; coding: utf-8
;; mode: emacs-lisp
;; End:"))
    (write-region (point-min) (point-max) filename)))

(defvar persistent-store-merge-handlers nil
  "Alist of (symbol-key . merge-function) pairs, where symbol-key
is the key used in `persistent-store-get' and merge-function is
function of two arguments that takes old and new entry and
should produce merged entry or nil in case it fails to do so.")

(defun persistent-store-try-merging-contents (old-contents new-contents)
  "Try to merge two database contents into one. Return nil if it cannot be
performed for some field."
  (let ((done nil)
        (failed nil)
        (result nil))
    (while (and (not done)
                old-contents
                new-contents)
      (let* ((old-entry       (car old-contents))
             (old-entry-key   (car old-entry))
             (old-entry-value (cdr old-entry))
             (new-entry       (car new-contents))
             (new-entry-key   (car new-entry))
             (new-entry-value (cdr new-entry)))
        (cond
          ((eq old-entry-key new-entry-key)
           (cond
             ((and (hash-table-p old-entry-value)
                   (hash-table-p new-entry-value)
                   (equal (hash-table->alist old-entry-value)
                          (hash-table->alist new-entry-value)))
              (setf result
                    (cons old-entry result)))
             ((equal old-entry-value new-entry-value)
              (setf result
                    (cons old-entry result)))
             (t
              (if-let (merge-handler (cdr-safe
                                      (assoc old-entry-key
                                             persistent-store-merge-handlers)))
                  (if-let (merged-entry (funcall merge-handler
                                                 old-entry
                                                 new-entry))
                      (setf result
                            (cons merged-entry
                                  result))
                    ;; Abort if merge-handler failed to merge entries
                    (setf done t
                          failed t
                          result nil))
                ;; Abort if entries are different and there's no suitable
                ;; merge handler.
                (setf done t
                      failed t
                      result nil)))))
          ((persistent-store-symbol< old-entry-key new-entry-key)
           ;; Store entries in reverse order because we're going to do
           ;; nreverse at the end.
           (setf result
                 (cons new-entry
                       (cons old-entry
                             result))))
          (t
           ;; Store entries in reverse order because we're going to do
           ;; nreverse at the end.
           (setf result
                 (cons old-entry
                       (cons new-entry
                             result))))))
      (setf old-contents (cdr-safe old-contents)
            new-contents (cdr-safe new-contents)))
    (unless failed
      (cond
        ((null? new-contents)
         (setf result (append (reverse old-contents) result)))
        ((null? old-contents)
         (setf result (append (reverse new-contents) result)))))
    (nreverse result)))

(defun persistent-store-symbol< (x y)
  (string< (symbol->string x)
           (symbol->string y)))

(defun persistent-store-flush-database ()
  "Flush db contents to file."
  (let ((new-content nil)
        (current-content-str
         (persistent-store-load-file persistent-store-store-file))
        (entries-sort-pred
         (lambda (x y)
           (persistent-store-symbol< (car x) (car y)))))
    (maphash (lambda (key value)
               ;; store nil values too
               (push (cons key value) new-content))
             persistent-store-content)

    (setf new-content (-sort entries-sort-pred
                             new-content))
    (if (string= current-content-str
                 persistent-store-loaded-content)
        ;; file was not changed since we loaded data from it
        (persistent-store-write-contents-to-file new-content persistent-store-store-file)
      ;; file was changed since we loaded data from it
      (let ((merged-content
             (persistent-store-try-merging-contents
              (sort
               (hash-table->alist
                (persistent-store-read-contents-from-string current-content-str))
               entries-sort-pred)
              new-content)))
        (if merged-content
            (persistent-store-write-contents-to-file
             merged-content
             persistent-store-store-file)
          (let ((done nil))
            (while (not done)
              (let ((ch nil))
                (while (not (memq ch '(?y ?n ?d ?b ?h ?\? ?Y ?N ?D ?B ?H 7 27 ?q ?Q)))
                  (setf ch (read-key (format "Store file changed since last load, store anyway? [?hyYnNdDbB]: "))))
                (cond
                  ((or (= ch 7)  ;; C-g, abort
                       (= ch 27) ;; <escape>
                       (char=? ch ?q)
                       (char=? ch ?Q))
                   (error "Store file not saved, *your data may get lost*"))
                  ((or (char=? ch ?y)
                       (char=? ch ?Y))
                   (persistent-store-write-contents-to-file
                    new-content
                    persistent-store-store-file)
                   (setf persistent-store-loaded-content
                         (buffer-substring-no-properties (point-min) (point-max))
                         done t))
                  ((and
                    (or (char=? ch ?n)
                        (char=? ch ?N))
                    (y-or-n-p "Store file not saved, *your data may get lost*. Really continue?"))
                   (setf done t)
                   ;; unwind stack, prevent emacs exit if any
                   ;; by all means, let the user know, that he may loose data!
                   (message "Store file not saved, *your data may get lost*")
                   (sit-for 0.5))
                  ((or (char=? ch ?d)
                       (char=? ch ?D))
                   (ediff-diff-texts-recursive-edit
                    persistent-store-loaded-content
                    current-content-str
                    :a-buf-name "Loaded contents (original file)"
                    :b-buf-name "Current file contents")
                   (sit-for 0.1))
                  ((or (char=? ch ?b)
                       (char=? ch ?B))
                   (persistent-store-write-contents-to-file
                    new-content
                    (read-file-name "Backup file name: "
                                    (file-name-directory persistent-store-store-file)
                                    persistent-store-backup-file))
                   (setf done t))
                  ((or (char=? ch ?h)
                       (char=? ch ?H)
                       (char=? ch ?\?))
                   (read-key
                    (join-lines
                     '("yY  - write your data to store file, nevermind that someone wrote something there"
                       "nN  - do not write data, *your data may get lost*"
                       "dD  - view diff between contents of store file loaded by you and current one"
                       "bB  - write to backup file"
                       "?hH - show this message"
                       ""
                       "Press any key to continue")
                     "\n"))
                   (sit-for 0.1)))))))))))

(defun persistent-store-debug-print-content ()
  (interactive)
  (if (= 0 (persistent-store-database-size))
      (message "database is empty")
    (let ((counter 0))
      (maphash (lambda (key value)
                 (message "#%d: %S:%S" counter key value)
                 (cl-incf counter))
               persistent-store-content))))

(defsubst persistent-store-database-size ()
  "Return number of entries in database."
  (hash-table-size persistent-store-content))

(provide 'persistent-store)

;; Local Variables:
;; End:

;; persistent-store.el ends here
