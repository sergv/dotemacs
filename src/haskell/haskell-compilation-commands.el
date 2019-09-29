;; haskell-compilation-commands.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 29 September 2019
;; Description:

(defvar haskell-compilation-cabal-build-command-presets
  (let ((cabal-command
         (lambda (cmd &rest args)
           (format "cd \"%%s\" && cabal %s%s"
                   cmd
                   (if args
                       (concat " " (s-join " " args))
                     "")))))
    (-mapcat (lambda (entry)                 ;
               (let ((target (car entry)))
                 (if (listp target)
                     (--map (cons it (cdr entry)) target)
                   (list entry))))
             `((build . ,(funcall cabal-command "build" "--builddir" "/tmp/dist"))
               (prof .  ,(funcall cabal-command "build" "--enable-profiling" "--builddir" "/tmp/dist"))
               (clean . ,(funcall cabal-command "clean" "--builddir" "/tmp/dist"))
               (test .  ,(funcall cabal-command "test" "--builddir" "/tmp/dist" "--test-show-details=direct"))
               (bench . ,(funcall cabal-command "bench" "--builddir" "/tmp/dist"))))))

(defvar haskell-compilation-last-build-command
  (or (cdr-safe (assoc 'build haskell-compilation-cabal-build-command-presets))
      (error "Failed to set up haskell-compile-cabal-build-command")))

(defun haskell-start-compilation (&optional edit-command)
  "Run a compile command for the current Haskell buffer."
  (interactive "P")
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let* ((proj-dir (or (haskell-watch-get-project-root)
                       (when-let ((epr (eproj-get-project-for-buf-lax (current-buffer))))
                         (eproj-project/root epr))
                       default-directory))
         (raw-command
          (if edit-command
              (let* ((preset
                      (intern
                       (completing-read "Build preset: "
                                        haskell-compilation-cabal-build-command-presets
                                        nil
                                        t
                                        nil
                                        'haskell-compile--build-presets-history)))
                     (command
                      (cdr
                       (assoc preset haskell-compilation-cabal-build-command-presets))))
                ;; remember command so it will be called again in the future
                (setf haskell-compilation-last-build-command command)
                command)
            haskell-compilation-last-build-command))
         (command (if proj-dir
                      (format raw-command (expand-file-name proj-dir))
                    raw-command)))

    (compilation-start command 'haskell-compilation-mode)))


(provide 'haskell-compilation-commands)

;; Local Variables:
;; End:

;; haskell-compilation-commands.el ends here
