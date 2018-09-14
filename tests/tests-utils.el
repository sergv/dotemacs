;; tests-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 14 September 2018
;; Description:

(require 'common)
(require 'ert)

(defun tests-utils--multiline (&rest lines)
  (mapconcat #'identity lines "\n"))

(defvar tests-utils--temp-buffer nil)

(defmacro* tests-utils--with-temp-buffer (&key action contents initialisation)
  (declare (indent 1))
  `(save-match-data
     (unless tests-utils--temp-buffer
       (setf tests-utils--temp-buffer (get-buffer-create " tests-utils-temp-buffer"))
       (with-current-buffer tests-utils--temp-buffer
         ,initialisation))
     (with-current-buffer tests-utils--temp-buffer
       (erase-buffer)
       (insert ,contents)
       (goto-char (point-min))
       (if (re-search-forward "_|_" nil t)
           (replace-match "")
         (error "No _|_ marker for point position within contents:\n%s" ,contents))
       (font-lock-fontify-buffer)
       ,action)))

(defmacro* tests-utils--test-buffer-contents (&key action contents expected-value initialisation)
  (declare (indent 2))
  `(tests-utils--with-temp-buffer
    :initialisation ,initialisation
    :action
    (progn
      ,action
      (insert "_|_")
      (let ((actual-contents
             (buffer-substring-no-properties (point-min) (point-max)))
            (expected-contents
             ,expected-value))
        (unless (string-match-p "_|_" expected-contents)
          (error "Expected buffer contents does not provide point position with _|_"))
        (should (equal (split-into-lines actual-contents)
                       (split-into-lines expected-contents)))))
    :contents
    ,contents))

(defmacro* tests-utils--test-result (&key action expected-value contents)
  `(tests-utils--with-temp-buffer
    :action (should (equal ,action ,expected-value))
    :contents ,contents))

(defmacro tests-utils--test-evaluate (action contents expected-value)
  (declare (indent 1))
  `(tests-utils--with-temp-buffer
    :action ,action
    :contents ,contents))


(provide 'tests-utils)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; tests-utils.el ends here
