;; tests-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 14 September 2018
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 'common)
(require 'el-patch)
(require 'ert)

(defun tests-utils--multiline (&rest lines)
  (mapconcat #'identity lines "\n"))

(defvar test-utils--temp-buffers nil
  "Alist from buffer-id (symbol) to actual buffer.")

(cl-defmacro tests-utils--with-temp-buffer (&key action contents initialisation post-content-initialisation buffer-id suppress-cursor)
  (declare (indent nil))
  `(save-match-data
     (let ((buf ,@(when buffer-id
                    (list `(cdr-safe (assq ',buffer-id test-utils--temp-buffers))))))
       ,@(when buffer-id
           (list
            `(unless buf
               (setf buf
                     (get-buffer-create ,(concat " tests-utils-temp-buffer-" (symbol-name buffer-id)))
                     test-utils--temp-buffers (cons (cons ',buffer-id  buf) test-utils--temp-buffers))
               (with-current-buffer buf
                 ,initialisation))))
       (,@(if buffer-id
              '(with-current-buffer buf)
            `(with-temp-buffer
               ,initialisation))
        ;; This is required for functions like ‘execute-kbd-macro’ that will
        ;; take action only in the buffer of currently selected window.
        (set-window-buffer (selected-window) (current-buffer))
        (erase-buffer)
        (insert ,contents)
        (goto-char (point-min))
        (save-match-data
          (save-excursion
            ,post-content-initialisation))
        ,(unless suppress-cursor
           `(progn
              (if (re-search-forward "_|_" nil t)
                  (replace-match "")
                (error "No _|_ marker for point position within contents:\n%s" ,contents))
              (when (save-excursion
                      (goto-char (point-min))
                      (re-search-forward "_|_" nil t))
                (error "More than one occurrence of _|_ in source"))))
        (font-lock-fontify-buffer)
        (unwind-protect
            ,action
          (when (and vim-active-mode
                     (not (eq vim-active-mode 'vim-normal-mode)))
            (vim-activate-mode #'vim-normal-mode)))))))

;; Nasty overrides to print actual string I want to see instead of prettyprinting
;; lisp object that ERT insists on.
(el-patch-defun ert--pp-with-indentation-and-newline (object)
  "Pretty-print OBJECT, indenting it to the current column of point.
Ensures a final newline is inserted."
  (el-patch-wrap 3 0
    (if (and (consp object)
             (eq (car object) 'ert-test-failed)
             (stringp (cdr object)))
        (progn
          (delete-region (line-beginning-position) (point))
          (insert (cdr object) "\n"))
      (let ((begin (point))
            (pp-escape-newlines t)
            (print-escape-control-characters t))
        (pp object (current-buffer))
        (unless (bolp) (insert "\n"))
        (save-excursion
          (goto-char begin)
          (indent-sexp))))))

(cl-defmacro tests-utils--test-buffer-contents (&key action contents expected-value initialisation post-content-initialisation buffer-id suppress-cursor)
  (declare (indent nil))
  `(tests-utils--with-temp-buffer
    :initialisation ,initialisation
    :post-content-initialisation ,post-content-initialisation
    :suppress-cursor ,suppress-cursor
    :action
    (progn
      ,action
      (insert "_|_")
      (let ((actual-contents
             (buffer-substring-no-properties (point-min) (point-max)))
            (expected-contents ,expected-value))
        (unless (string-match-p "_|_" expected-contents)
          (error "Expected buffer contents does not provide point position with _|_"))
        (let ((actual-lines (split-into-lines actual-contents t))
              (expected-lines (split-into-lines expected-contents t)))
          (unless (equal actual-lines expected-lines)
            (while (and expected-lines
                        actual-lines
                        (equal (car expected-lines) (car actual-lines)))
              (pop actual-lines)
              (pop expected-lines))
            (signal 'ert-test-failed
                    (format "Mismatch:\nActual:\n%s\nExpected:\n%s\nFirst mismatch in actual:\n%s\nFirst mismatch in expected:\n%s"
                            actual-contents
                            expected-contents
                            (join-lines actual-lines)
                            (join-lines expected-lines)))))))
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

(cl-defmacro tests-utils--multiple-buffer-contents-from-same-init (&key initialisation contents actions-with-results buffer-id)
  `(progn
     ,@(cl-loop
        for entry in actions-with-results
        collecting
        `(ert-deftest ,(car entry) ()
           ,(append
             (list 'tests-utils--test-buffer-contents
                   :contents contents
                   :initialisation initialisation
                   :buffer-id buffer-id)
             (cdr entry))))))

(defmacro skip-if-no-treesitter! ()
  `(unless (and (fboundp 'treesit-available-p)
                (treesit-available-p))
     (ert-skip "treesitter not available")))

(provide 'tests-utils)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; tests-utils.el ends here
