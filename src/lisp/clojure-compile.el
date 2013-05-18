;; clojure-compile.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 31 March 2013
;; Description:

(eval-when-compile (require 'cl-lib))
(require 'common)
(require 'clojure-util)

(defconst +clojure-compile-warning-regexp+
  (rx bol
      "Reflection warning, "
      (group
       (? "/")
       (+ (+ (regexp "[^/\n]"))
          "/")
       (+ (regexp "[^/\n]"))
       "\.clj")
      ":"
      (group (+ digit))
      ":"
      (group (+ digit))))

(defconst +clojure-compile-error-regexp+
  (rx (? bol "Exception in thread \"main\" ")
      (? "java."
         (or "lang" "io")
         ".")
      (* (regexp "[^ \n\r\t]"))
      (or "exception"
          "Exception")
      (? ":")
      (* anything)
      ", compiling:("
      (group
       (? "/")
       (* (+ (regexp "[^/\n]"))
          "/")
       (+ (regexp "[^/\n]"))
       "\.clj")
      ":"
      (group (+ digit))
      ":"
      (group (+ digit))
      ")"))

(defun clojure-compile/get-selected-warning ()
  "Return filename, line and column for warning on current line (i.e. the selected one)."
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (when (looking-at? +clojure-compile-warning-regexp+)
        (values (match-string-no-properties 1)
                (string->number (match-string-no-properties 2))
                (string->number (match-string-no-properties 3)))))))

(defun clojure-compile/get-selected-error ()
  "Return filename, line and column for error on current line (i.e. the selected one)."
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (when (looking-at? +clojure-compile-error-regexp+)
        (values (match-string-no-properties 1)
                (string->number (match-string-no-properties 2))
                (string->number (match-string-no-properties 3)))))))



(defun clojure-compile/find-buffer (filename)
  "Get buffer that corresponds to FILENAME, which may be neither full nor relative
path, in which case filename with suffix equal to FILENAME will be tried."
  (assert (not (= 0 (length filename))))
  ;; (cond ((char=? ?/ (elt filename 0))
  ;;        ()))
  (current-buffer)
  (aif (find-if (lambda (buf)
                  (string-suffix? filename (buffer-file-name buf)))
                (visible-buffers))
    it
    (when (file-exists? filename)
      (cond ((get-file-buffer filename)
             (get-file-buffer filename))
            (else
             (find-file-noselect filename))))))

(defun clojure-compile/goto-error ()
  "Jump to location of error or warning (file, line and column) in current window."
  (interactive)
  (let ((goto-err
         (lambda (spec)
           (destructuring-bind (filename line column) spec
             (aif (clojure-compile/find-buffer filename)
               (switch-to-buffer it)
               (error "File %s not found" filename))
             (vim:save-position)
             (goto-line line)
             (move-to-column column)))))
    (if-let (err (clojure-compile/get-selected-error))
      (funcall goto-err err)
      (if-let (warning (clojure-compile/get-selected-warning))
        (funcall goto-err warning)))))

(defun clojure-compile/goto-error-other-window ()
  "Jump to location of error or warning (file, line and column) in other window."
  (interactive)
  (let ((goto-err
         (lambda (spec)
           (destructuring-bind (filename line column) spec
             (aif (clojure-compile/find-buffer filename)
               (switch-to-buffer-other-window it)
               (error "File %s not found" filename))
             (vim:save-position)
             (goto-line line)
             (move-to-column column)))))
    (if-let (err (clojure-compile/get-selected-error))
      (funcall goto-err err)
      (if-let (warning (clojure-compile/get-selected-warning))
        (funcall goto-err warning)))))

(defvar clojure-compilation-mode-map
  (let ((m (make-sparse-keymap)))
    (def-keys-for-map m
      ("SPC"      clojure-compile/goto-error-other-window)
      ("<return>" clojure-compile/goto-error)
      ("o"        clojure-compile/goto-error-other-window))
    m))

(define-compilation-mode clojure-compilation-mode "Clojure compilation"
  "Mode for clojure compilation."
  (setq-local compilation-error-regexp-alist
              (list
               (list +clojure-compile-warning-regexp+ ;; regex
                     1                                ;; file-group
                     2                                ;; line-group
                     3                                ;; column-group
                     1                                ;; type - 1 - warning
                     )
               (list +clojure-compile-error-regexp+
                     1 ;; file-group
                     2 ;; line-group
                     3 ;; column-group
                     2 ;; type - 2 - real error
                     )))

  (set (make-local-variable '*compilation-jump-error-regexp*)
       +clojure-compile-error-regexp+)

  (set (make-local-variable 'compilation-first-column) 1) ;; clojure counts from 1
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-scroll-output) nil))



(defvar clojure-compile/avaliable-profiles
  '("dev" "release")
  "Available Leiningen profiles for compilation.")

(defvar clojure-compile/current-profile "dev"
  "Leiningen profile to compile with.")

(defvar-local clojure-compile/lein-command nil
  "Template for leiningen compilation command. Should contain %s placeholder
for profile, e.g. \"lein with-profiles %s compile\". May be set either via
file-local variable in source *.clj file or via file-local variable in
project.clj for current project. Values in individual source files
override values in project.clj")

(put 'clojure-compile/lein-command 'safe-local-variable #'string?)



(defconst clojure-compilation/buffer-name "*clojure-compilation*")

(defun clojure-compile (&optional switch-profile)
  "Start clojure compilation with profile `clojure-compile/current-profile'.
With prefix argument allows to select different profile."
  (interactive "P")
  (when switch-profile
    (setf clojure-compile/current-profile
          (icicle-completing-read "> "
                                  clojure-compile/avaliable-profiles
                                  nil
                                  nil))
    (unless (member clojure-compile/current-profile
                    clojure-compile/avaliable-profiles)
      (push clojure-compile/current-profile
            clojure-compile/avaliable-profiles)))
  (let ((command nil))
    (setf command (if (not (null? clojure-compile/lein-command))
                    clojure-compile/lein-command
                    (clojure-lein/read-variable-from-project-clj
                     (buffer-file-name)
                     'clojure-compile/lein-command)))
    (assert (not (null? command)))
    (compilation-start (format command
                               clojure-compile/current-profile)
                       #'clojure-compilation-mode
                       (lambda (_)
                         clojure-compilation/buffer-name))
    (with-current-buffer (get-buffer clojure-compilation/buffer-name)
      (goto-char (point-max)))))

(provide 'clojure-compile)

;; Local Variables:
;; End:

;; clojure-compile.el ends here
