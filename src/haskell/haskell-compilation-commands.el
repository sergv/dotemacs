;; haskell-compilation-commands.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 29 September 2019
;; Description:

(require 'configurable-compilation)
(require 's)

(defun haskell-compilation--make-cabal-build-command-presets (tmp)
  (let ((cabal-command
         (lambda (cmd &rest args)
           (format "cd \"%%s\" && cabal %s%s %s"
                   cmd
                   (if tmp
                       (concat " --builddir=" tmp)
                     "")
                   (s-join " " args)))))
    (-mapcat (lambda (entry)                 ;
               (let ((target (car entry)))
                 (if (listp target)
                     (--map (cons it (cdr entry)) target)
                   (list entry))))
             `((build . ,(funcall cabal-command "build"))
               (prof .  ,(funcall cabal-command "build" "--enable-profiling"))
               (clean . ,(funcall cabal-command "clean"))
               (test .  ,(funcall cabal-command "test" "--test-show-details=direct"))
               (bench . ,(funcall cabal-command "bench"))))))

(defvar haskell-compilation--default-cabal-build-command-presets
  (haskell-compilation--make-cabal-build-command-presets (fold-platform-os-type "/tmp/dist" nil)))

(defun haskell-compilation-commands-install! (proj)
  (let* ((presets (eproj-query/fold-build-dir
                   proj
                   (constantly (cons haskell-compilation--default-cabal-build-command-presets
                                     dante--default-methods))
                   (lambda (dir)
                     (cons (haskell-compilation--make-cabal-build-command-presets dir)
                           (dante--make-methods dir)))))
         (comp-commands (car presets))
         (dante-check-and-repl-methods (cdr presets)))
    (configurable-compilation-install-command-presets!
     comp-commands
     'haskell-compile--build-presets-history
     'haskell-compilation-mode
     nil ;; Make buffer name.
     )
    (setq-local dante-methods-alist dante-check-and-repl-methods
                dante-methods (-map 'car dante-methods-alist)))

  (vim:local-emap "compile"  'vim:haskell-compile)
  (vim:local-emap "c"        'vim:haskell-compile)
  (vim:local-emap "ccompile" 'vim:haskell-compile-choosing-command)
  (vim:local-emap "cc"       'vim:haskell-compile-choosing-command)

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap)
    (("C-m" "<f9>") vim:haskell-compile)))

(vim:defcmd vim:haskell-compile (nonrepeatable)
  (configurable-compilation-start nil))
(vim:defcmd vim:haskell-compile-choosing-command (nonrepeatable)
  (configurable-compilation-start t))

(provide 'haskell-compilation-commands)

;; Local Variables:
;; End:

;; haskell-compilation-commands.el ends here
