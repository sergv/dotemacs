(require 'common)
(require 'cl)

;;; database stores all info in single file, but facilities
;;; for dealing with multiple files are provided

(defvar persistent-store-content nil ;; (make-hash-table :test #'equal :size 1024)
  "Current content of the database")

(defvar persistent-store-store-file
  (path-concat +prog-data-path+ "persistent-store")
  "Filename of database store file")

(defvar persistent-store-flush-hook nil
  "Functions that will be called before flush of content to disc will take place")

(defun persistent-store-init ()
  "Initialize database."
  (unless persistent-store-content
    (add-hook 'kill-emacs-hook #'persistent-store-flush-database)
    (setf persistent-store-content (make-hash-table :test #'equal :size 1024))
    (persistent-store-load-content persistent-store-store-file)))

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
;;     (maphash #'(lambda (key value)
;;                  (if (funcall pred key)
;;                      (push value result)))
;;              persistent-store-content)
;;     result))

(defun persistent-store-flush-database ()
  "Function to flush db content to file."
  (run-hooks persistent-store-flush-hook)
  (let ((content-list nil))
    (maphash #'(lambda (key value)
                 ;; store nil values too
                 (push (cons key value) content-list))
             persistent-store-content)
    (persistent-store-write-alist persistent-store-store-file content-list)))

(defun persistent-store-load-content (filename)
  "Load database content from file."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (goto-char (point-min))
    (mapc #'(lambda (entry)
              (puthash (car entry) (cdr entry) persistent-store-content))
          (read (current-buffer)))))

(defun persistent-store-write-alist (filename content-list)
  "Write content in form of list to file."
  (with-temp-buffer
    (goto-char (point-min))
    (let ((print-level nil)
          (print-length nil))
      (print content-list (current-buffer)))
    (write-region (point-min) (point-max) filename)))


(defun persistent-store-debug-print-content ()
  (interactive)
  (if (equal 0 (persistent-store-database-size))
    (message "database is empty")
    (let ((counter 0))
      (maphash #'(lambda (key value)
                   (message "#%d: %S:%S" counter key value)
                   (incf counter))
               persistent-store-content))))

(defun persistent-store-database-size ()
  "Return number of entries in database."
  (let ((count 0))
    (maphash #'(lambda (key value)
                 (incf count))
             persistent-store-content)
    count))

(provide 'persistent-store)

