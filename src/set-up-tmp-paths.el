;; set-up-tmp-paths.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 22 March 2021
;; Description:

(eval-when-compile
  (require 'subr-x)
  (require 'set-up-platform))

(require 'set-up-paths)

(defconst +tmp-path+
  (make-temp-name
   (fold-platform-os-type
    "/tmp/emacs-tmp-"
    (or (when-let (tmpdir (or (getenv "TMPDIR")
                              (getenv "TEMP")))
          (concat tmpdir "\\emacs-tmp-"))
        (concat +prog-data-path+ "/tmp"))))
  "Path to temporary directory specific to current Emacs session.
Other Emacs invocations will not be able to share anything in
here. Contents of this directory may be removed on system
restarts.")

(make-directory +tmp-path+ t)

(setf temporary-file-directory +tmp-path+
      small-temporary-file-directory +tmp-path+
      tramp-auto-save-directory (concat +tmp-path+ "/tramp"))

(defun clean-tmp-path-on-emacs-exit ()
  "Delete `+tmp-path+' directory on emacs exit."
  (ignore-errors
    (delete-directory +tmp-path+
                      t   ;; recurse
                      nil ;; don't just move to trash
                      )))
(add-hook 'kill-emacs-hook #'clean-tmp-path-on-emacs-exit)

;; (push (concat +emacs-compiled-path+ "/lib") load-path )
;;
;; ;; Shadow everything else
;; (push (concat +emacs-compiled-path+ "/compiled") load-path)
;; (unless (equal (getenv "EMACS_SKIP_ELC") "1")
;;   (push (concat +emacs-compiled-path+ "/compiled/elc") load-path))

(add-to-list 'exec-path +execs-path+)

(provide 'set-up-tmp-paths)

;; Local Variables:
;; End:

;; set-up-tmp-paths.el ends here
