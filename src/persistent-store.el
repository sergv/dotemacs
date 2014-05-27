;; persistent-store.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile (require 'cl))

(require 'common)

;;; database stores all info in single file, but facilities
;;; for dealing with multiple files are provided

(defparameter persistent-store-content nil ;; (make-hash-table :test #'equal :size 1024)
  "Current contents of the database")

(defparameter persistent-store-store-file
  (path-concat +prog-data-path+ "persistent-store")
  "Filename of database store file")

(defparameter persistent-store-backup-file
  (concat persistent-store-store-file ".bak")
  "Filename of backup database store file")

(defparameter persistent-store-flush-hook nil
  "Functions that will be called before flush of contents to disc will take place")

(defparameter persistent-store-loaded-content nil
  "Contents of `persistent-store-store-file' than was used to set up
`persistent-store-content'.")


(defun persistent-store-init ()
  "Initialize database."
  (unless persistent-store-content
    (add-hook 'kill-emacs-hook #'persistent-store-flush-database)
    (setf persistent-store-content (make-hash-table :test #'equal :size 1024))
    (persistent-store-load-contents)))

(defsubst persistent-store-put (key value)
  "Store entry in database."
  (puthash key value persistent-store-content))

(defsubst persistent-store-get (key &optional default)
  "Retrieve entry from database."
  (gethash key persistent-store-content default))

(defsetf persistent-store-get (key) (value)
  `(setf (gethash ,key persistent-store-content) ,value))

(defsubst persistent-store-remove (key)
  "Remove entry from database."
  (remhash key persistent-store-content))

;; (defun persistent-store-query (pred)
;;   "Return list of values for whose keys PRED returns t."
;;   (let ((result nil))
;;     (maphash (lambda (key value)
;;                (if (funcall pred key)
;;                  (push value result)))
;;              persistent-store-content)
;;     result))

(defun persistent-store-load-file (filename)
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun persistent-store-flush-database ()
  "Flush db contents to file."
  (run-hooks persistent-store-flush-hook)
  (let ((content-list nil)
        (current-file-content
         (persistent-store-load-file persistent-store-store-file)))
    (maphash (lambda (key value)
               ;; store nil values too
               (push (cons key value) content-list))
             persistent-store-content)

    (setf content-list (sort (copy-list content-list)
                             (lambda (a b)
                               (string< (symbol->string (car a))
                                        (symbol->string (car b))))))
    (with-temp-buffer
      (goto-char (point-min))
      (let ((print-level nil)
            (print-length nil))
        ;; (print content-list (current-buffer))
        (pp content-list (current-buffer)))

      (if (string= current-file-content
                   persistent-store-loaded-content)
        ;; file was not changed since we loaded data from it
        (write-region (point-min) (point-max) persistent-store-store-file)
        ;; file was changed since we loaded data from it
        (let ((done nil))
          (while (not done)
            (let ((ch nil))
              (while (not (member ch '(?y ?n ?d ?b ?h ?\? ?Y ?N ?D ?B ?H 7 27 ?q ?Q)))
                (setf ch (read-key (format "Store file changed since last load, store anyway? [?hyYnNdDbB]: "))))
              (cond
                ((or (= ch 7)  ;; C-g, abort
                     (= ch 27) ;; <escape>
                     (char=? ch ?q)
                     (char=? ch ?Q))
                 (error "Store file not saved, *your data may get lost*"))
                ((or (char=? ch ?y)
                     (char=? ch ?Y))
                 (write-region (point-min) (point-max) persistent-store-store-file)
                 (setf persistent-store-loaded-content
                       (buffer-substring-no-properties (point-min) (point-max))
                       done t))
                ((or (char=? ch ?n)
                     (char=? ch ?N))
                 (setf done t)
                 ;; unwind stack, prevent emacs exit if any
                 ;; by all means, let the user know, that he may loose data!
                 (message "Store file not saved, *your data may get lost*")
                 (sit-for 0.5))
                ((or (char=? ch ?d)
                     (char=? ch ?D))
                 (ediff-diff-texts-recursive-edit
                  persistent-store-loaded-content
                  current-file-content
                  :a-buf-name "Loaded contents (original file)"
                  :b-buf-name "Current file contents")
                 (sit-for 0.1))
                ((or (char=? ch ?b)
                     (char=? ch ?B))
                 (write-region (point-min) (point-max)
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
                 (sit-for 0.1))))))))))

(defun persistent-store-load-contents ()
  "Load database contents from file."
  (setf persistent-store-loaded-content
        (persistent-store-load-file persistent-store-store-file))
  (mapc (lambda (entry)
          (puthash (car entry) (cdr entry) persistent-store-content))
        (read persistent-store-loaded-content)))


(defun persistent-store-debug-print-content ()
  (interactive)
  (if (equal 0 (persistent-store-database-size))
    (message "database is empty")
    (let ((counter 0))
      (maphash (lambda (key value)
                 (message "#%d: %S:%S" counter key value)
                 (incf counter))
               persistent-store-content))))

(defun persistent-store-database-size ()
  "Return number of entries in database."
  (let ((count 0))
    (maphash (lambda (key value)
               (incf count))
             persistent-store-content)
    count))


(provide 'persistent-store)

;; Local Variables:
;; End:

;; persistent-store.el ends here
