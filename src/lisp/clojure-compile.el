;; clojure-compile.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 31 March 2013
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'common)
(require 'clojure-util)
(require 'keys-def)
(require 'compilation-setup)

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
      (? ":"
         (group (+ digit)))))

(defconst +clojure-compile-error-regexp+
  (rx (? bol "Exception in thread \"main\" ")
      (? "java."
         (or "lang" "io")
         ".")
      (* (regexp "[^ \n\r\t]"))
      (or "exception"
          "Exception")
      (? ":")
      (* (regexp "[^\n]"))
      (or ", compiling:"
          ;; capture part of a stacktrace
          (seq "\n"
               (* (any ?\s ?\t))
               "at"
               (* (any ?\s ?\t))
               (+ (regexp "[a-zA-Z0-9_.$]"))))
      "("
      (group
       (? "/")
       (* (+ (regexp "[^/\n]"))
          "/")
       (+ (regexp "[^/\n]"))
       "\.clj")
      ":"
      (group (+ digit))
      ;; stacktraces do not include columns
      (? ":"
         (group (+ digit)))
      ")"))

(defvar clojure-compilation-mode-map
  (let ((m (make-sparse-keymap)))
    (def-keys-for-map m
      +vim-special-keys+
      +vim-word-motion-keys+
      ("SPC"      compilation/goto-error-other-window)
      ("<return>" compilation/goto-error)
      ("o"        compilation/goto-error-other-window))
    m))

(define-compilation-mode clojure-compilation-mode "Clojure compilation"
  "Mode for clojure compilation."
  (setq-local compilation-error-regexp-alist
              (list
               (list +clojure-compile-warning-regexp+ ;; regex
                     1                                ;; file-group
                     2                                ;; line-group
                     3                                ;; column-group
                     1 ;; type - 1 - warning
                     )
               (list +clojure-compile-error-regexp+
                     1 ;; file-group
                     2 ;; line-group
                     3 ;; column-group
                     2 ;; type - 2 - real error
                     ))

              compilation-first-column 1 ;; clojure counts from 1
              compilation-disable-input t
              compilation-scroll-output nil))



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

(put 'clojure-compile/lein-command 'safe-local-variable #'stringp)



(defconst clojure-compilation/buffer-name "*clojure-compilation*")

(defun clojure-compile (&optional switch-profile)
  "Start clojure compilation with profile `clojure-compile/current-profile'.
With prefix argument allows to select different profile."
  (interactive "P")
  (when switch-profile
    (setf clojure-compile/current-profile
          (ivy-completing-read "> "
                               clojure-compile/avaliable-profiles
                               nil
                               nil))
    (unless (member clojure-compile/current-profile
                    clojure-compile/avaliable-profiles)
      (push clojure-compile/current-profile
            clojure-compile/avaliable-profiles)))
  (let ((command nil))
    (setf command (if (not (null clojure-compile/lein-command))
                      clojure-compile/lein-command
                    (clojure-lein/read-variable-from-project-clj
                     buffer-file-name
                     'clojure-compile/lein-command)))
    (cl-assert (not (null command)))
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
