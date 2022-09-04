;; nix-integration.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  1 September 2022
;; Description:

;;;###autoload
(defun nix-maybe-call-via-flakes (args &optional proj-dir)
  "If current project has flake.nix then return ‘nix develop
--command ARGS’, otherwise just return ARGS."
  (let ((nix-exe (cached-executable-find "nix")))
    (if-let ((root (or proj-dir (configurable-compilation-proj-dir))))
        (let ((flake (concat root "/flake.nix")))
          (if (file-exists-p flake)
              (nix-call-via-flakes args root)
            args))
      args)))

;;;###autoload
(defun nix-call-via-flakes (args &optional proj-dir)
  "If current project has flake.nix then return ‘nix develop
--command ARGS’, otherwise just return ARGS."
  (if-let ((nix-exe (cached-executable-find "nix")))
      (if proj-dir
          (cons nix-exe (cons "develop" (cons "--no-warn-dirty" (cons proj-dir (cons "--command" args)))))
        (cons nix-exe (cons "develop" (cons "--no-warn-dirty" (cons "--command" args)))))
    args))

(provide 'nix-integration)

;; Local Variables:
;; End:

;; nix-integration.el ends here
