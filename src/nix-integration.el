;; nix-integration.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  1 September 2022
;; Description:

(eval-when-compile
  (require 'cl))

(require 'configurable-compilation)
(require 'cmdline)
(require 's)

;;;###autoload
(defun nix-maybe-call-via-flakes (args &optional proj-dir)
  "If current project has flake.nix then return ‘nix develop
--command ARGS’, otherwise just return ARGS."
  (cmdline-to-executable-command
   (nix-maybe-call-via-flakes-exe-args (car args) (cdr args) proj-dir)))

;;;###autoload
(defun nix-maybe-call-via-flakes-exe-args (exe args &optional proj-dir)
  "If current project has flake.nix then return wrap call to EXE with ARGS into
‘nix develop’ or equivalent. Returns value of ‘cmdline’ structure."
  (if-let ((root (or proj-dir (configurable-compilation-proj-dir))))
      (let ((flake (concat root "/flake.nix")))
        (if (file-exists-p flake)
            (nix-call-via-flakes exe args root)
          (make-cmdline :exe exe :args args)))
    (make-cmdline :exe exe :args args)))

;;;###autoload
(defun nix-call-via-flakes (exe args &optional proj-dir)
  "If current project has flake.nix then return ‘nix develop
--command ARGS’, otherwise just return ARGS."
  (let* ((develop "develop")
         (command "--command")
         (develop-prop (eval-when-compile (configurable-compilation--unimportant-text "develop")))
         (command-prop (eval-when-compile (configurable-compilation--unimportant-text "--command"))))
    (if-let* ((trix-exe (cached-executable-find "trix")))
        (let ((trix-exe-prop (configurable-compilation--unimportant-text trix-exe))
              (packed-quoted-args (s-join " " (-map #'shell-quote-argument (cons exe args))))
              (packed-args (s-join " " (cons exe args))))
          (if proj-dir
              (let ((flake-uri (concat (expand-file-name proj-dir) "#default")))
                (make-cmdline
                 :exe
                 trix-exe
                 :args
                 (cons develop
                       (cons flake-uri
                             (cons command
                                   (list packed-quoted-args))))
                 :rendered
                 (cons trix-exe-prop
                       (cons develop-prop
                             (cons (configurable-compilation--unimportant-text flake-uri)
                                   (cons command-prop
                                         (list packed-args)))))))
            (make-cmdline
             :exe
             trix-exe
             :args
             (cons develop (cons command (list packed-quoted-args)))
             :rendered
             (cons trix-exe-prop (cons develop-prop (cons command-prop packed-args))))))
      (if-let* ((nix-exe (cached-executable-find "nix")))
          (let ((nix-exe-prop (configurable-compilation--unimportant-text nix-exe))
                (no-warn-dirty "--no-warn-dirty")
                (no-warn-dirty-prop (eval-when-compile (configurable-compilation--unimportant-text "--no-warn-dirty"))))
            (if proj-dir
                (let ((expanded-proj-dir (expand-file-name proj-dir)))
                  (make-cmdline
                   :exe
                   nix-exe
                   :arsg
                   (cons develop
                         (cons no-warn-dirty
                               (cons expanded-proj-dir
                                     (cons command
                                           (cons exe args)))))
                   :rendered
                   (cons nix-exe-prop
                         (cons develop-prop
                               (cons no-warn-dirty-prop
                                     (cons (configurable-compilation--unimportant-text expanded-proj-dir)
                                           (cons command-prop
                                                 (cons exe args))))))))
              (make-cmdline
               :exe
               nix-exe
               :args
               (cons develop
                     (cons no-warn-dirty
                           (cons command
                                 (cons exe args))))
               :rendered
               (cons nix-exe-prop
                     (cons develop-prop
                           (cons no-warn-dirty-prop
                                 (cons command-prop
                                       (cons exe args))))))))
        (cons exe args)))))

(provide 'nix-integration)

;; Local Variables:
;; End:

;; nix-integration.el ends here
