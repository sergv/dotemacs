;; nix-integration.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  1 September 2022
;; Description:

(require 'configurable-compilation)

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
      (let ((nix-exe-prop (configurable-compilation--unimportant-text nix-exe))
            (develop-prop (eval-when-compile (configurable-compilation--unimportant-text "develop")))
            (no-warn-dirty-prop (eval-when-compile (configurable-compilation--unimportant-text "--no-warn-dirty")))
            (command-prop (eval-when-compile (configurable-compilation--unimportant-text "--command"))))
        (if proj-dir
            (cons nix-exe-prop (cons develop-prop (cons no-warn-dirty-prop (cons (configurable-compilation--unimportant-text proj-dir) (cons command-prop args)))))
          (cons nix-exe-prop (cons develop-prop (cons no-warn-dirty-prop (cons command-prop args))))))
    args))

(provide 'nix-integration)

;; Local Variables:
;; End:

;; nix-integration.el ends here
