;; dante-tests-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 19 September 2026
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'set-up-platform))

(require 'common)
(require 'dante)

(require 'ert)
(require 'tests-utils)

(defconst dante-test-data/resources-root
  (concat +test-utils--test-root+ "/test-data"))

(defun dante-tests--truename-on-macos (x)
  (if (eq system-type 'darwin)
      (file-truename x)
    x))

(defun dante-tests--paths= (x y)
  (string= (dante-tests--truename-on-macos x)
           (dante-tests--truename-on-macos y)))

(defmacro dante-tests/with-file (path &rest body)
  (declare (indent 1))
  (let ((buf-var '#:buf))
    `(let ((noninteractive nil))
       (with-fresh-buffer-no-switch
           ,buf-var
           (find-file-noselect ,path)
         (unwind-protect
             (with-current-buffer ,buf-var
               ,@body)
           (when (buffer-live-p ,buf-var)
             (awhen (get-buffer (dante-buffer-name (dante-get-config ,buf-var)))
               (when (buffer-live-p it)
                 (kill-buffer it)))
             (awhen (get-buffer (dante-repl-buffer-name ,buf-var))
               (when (buffer-live-p it)
                 (kill-buffer it)))
             (kill-buffer ,buf-var)))))))

(defmacro dante-tests/with-file-no-clean (path &rest body)
  (declare (indent 1))
  `(let ((noninteractive nil))
     (with-current-buffer (find-file-noselect ,path)
       ,@body)))

(defmacro dante-tests/check-buffer-and-assert-when-done (&rest body)
  (let ((checking-done-var '#:checking-done))
    `(let* ((,checking-done-var nil)
            (check-func
             (lambda ()
               (setf ,checking-done-var t))))

       (add-hook 'flycheck-after-syntax-check-hook check-func nil t)

       (haskell-flycheck-force-run)
       ;; (flycheck-buffer)

       (while (not ,checking-done-var)
         (sit-for 0.05))

       (remove-hook 'flycheck-after-syntax-check-hook check-func t)

       (progn
         ,@body))))

(defmacro dante-tests/type-at-point-and-assert-when-done (type-var &rest body)
  (declare (indent 1))
  (cl-assert (symbolp type-var))
  (let ((checking-done-var '#:checking-done)
        (arg-var '#:fresh-var))
    `(let ((,checking-done-var nil)
           (,type-var nil))

       (dante-type-at--with-type-at-point
        (lambda (,arg-var)
          (setf ,type-var ,arg-var
                ,checking-done-var t)))

       (while (not ,checking-done-var)
         (sit-for 0.05))

       (progn
         ,@body))))

(defmacro dante-tests/haskell-symbnav-go-to-symbol-home-and-assert-when-done (&rest body)
  (let ((checking-done-var '#:checking-done)
        (arg-var '#:fresh-var))
    `(let ((,checking-done-var nil))
       (haskell-dante-symbnav/go-to-symbol-home
        (lambda (func args)
          (setf ,checking-done-var t)
          (apply func args)

          (progn
            ,@body)))

       (while (not ,checking-done-var)
         (sit-for 0.05)))))

(defun dante-repl/wait-for-prompt (proc)
  "Spin in a loop until prompt dante-repl prompt shows up before point."
  (cl-assert (processp proc))
  (cl-assert (process-live-p proc))
  (let ((p (point))
        (got-prompt? nil))
    (while (not (setf got-prompt?
                      (and (eq (char-before p) ?\s)
                           (when-let* ((p2 (char-before (- p 1))))
                             (or (eq p2 ?\4)
                                 (eq p2 ?\5))))))
      (accept-process-output proc nil nil t)
      (sit-for 0.05)
      (redisplay t)
      (setf p (point)))))

(defun dante-repl/wait-for-modules-loaded (proc)
  "Spin in a loop until prompt dante-repl prompt shows up before point."
  (cl-assert (processp proc))
  (while (save-excursion
           (goto-char (line-beginning-position 0))
           (not (or (looking-at-p "Ok, modules loaded:.*\\.$")
                    (looking-at-p "Failed, modules loaded: none\\.$"))))
    (accept-process-output proc nil nil t)
    (sit-for 0.05)
    (redisplay t)))

(provide 'dante-tests-utils)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; dante-tests-utils.el ends here
