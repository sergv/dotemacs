;; tests-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 14 September 2018
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 'common)
(require 'ert)

(defun tests-utils--multiline (&rest lines)
  (mapconcat #'identity lines "\n"))

(defvar test-utils--temp-buffers nil
  "Alist from buffer-id (symbol) to actual buffer.")

(cl-defmacro tests-utils--with-temp-buffer (&key action contents initialisation buffer-id)
  (declare (indent 1))
  `(save-match-data
     (let ((buf (cdr-safe (assq ',buffer-id test-utils--temp-buffers))))
       (unless buf
         (setf buf
               (get-buffer-create ,(concat " tests-utils-temp-buffer-" (symbol-name buffer-id)))
               test-utils--temp-buffers (cons (cons ',buffer-id  buf) test-utils--temp-buffers))
         (with-current-buffer buf
           ,initialisation))
       (with-current-buffer buf
         (erase-buffer)
         (insert ,contents)
         (goto-char (point-min))
         (if (re-search-forward "_|_" nil t)
             (replace-match "")
           (error "No _|_ marker for point position within contents:\n%s" ,contents))
         (font-lock-fontify-buffer)
         ,action))))

(cl-defmacro tests-utils--test-buffer-contents (&key action contents expected-value initialisation buffer-id)
  (declare (indent 2))
  `(tests-utils--with-temp-buffer
    :initialisation ,initialisation
    :action
    (progn
      ,action
      (insert "_|_")
      (let ((actual-contents
             (buffer-substring-no-properties (point-min) (point-max)))
            (expected-contents ,expected-value))
        (unless (string-match-p "_|_" expected-contents)
          (error "Expected buffer contents does not provide point position with _|_"))
        (should (equal (split-into-lines actual-contents)
                       (split-into-lines expected-contents)))))
    :contents ,contents
    :buffer-id ,buffer-id))

(cl-defmacro tests-utils--test-result (&key action expected-value contents)
  `(tests-utils--with-temp-buffer
    :action (should (equal ,action ,expected-value))
    :contents ,contents))

(defmacro tests-utils--test-evaluate (action contents)
  (declare (indent 1))
  `(tests-utils--with-temp-buffer
    :action ,action
    :contents ,contents))


(provide 'tests-utils)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; tests-utils.el ends here
