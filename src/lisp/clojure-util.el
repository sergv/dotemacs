;; clojure-util.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  7 April 2013
;; Description:

(eval-when-compile (require 'cl-lib))
(require 'common)

(defconst +leiningen-project-file+ "project.clj")

(defun clojure-lein/read-variable-from-project-clj (source-file
                                                    var)
  "Read value of file-local variable VAR from project.clj file SOURCE-FILE is
governed by."
  (assert (symbol? var))
  (if-let (project-root (locate-dominating-file source-file
                                                +leiningen-project-file+))
    (let ((project-file (concat project-root "/" +leiningen-project-file+)))
      (if-let (project-buf (get-file-buffer project-file))
        (buffer-local-value var project-buf)
        (with-temp-buffer
          (setq-local enable-local-variables :safe)
          ;; after content insertion local variables will be set automatically
          (insert-file-contents project-file)
          (buffer-local-value var (current-buffer)))))
    (error "Project root containing project.clj not found")))

(provide 'clojure-util)

;; Local Variables:
;; End:

;; clojure-util.el ends here
