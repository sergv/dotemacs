;; haskell-flycheck-cabal-build.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 June 2023
;; Description:

(eval-when-compile
  (require 'set-up-platform))

(require 'dante)
(require 'flycheck)
(require 'nix-integration)

(defun haskell-flycheck-cabal-build--check (checker cont)
  (unless dante-project-root (dante-initialize-method))

  (let* ((proj (eproj-get-project-for-buf (current-buffer)))
         (proj-dir (configurable-compilation-proj-dir))
         (build-dir (eproj-query/fold-build-dir
                     proj
                     ;; if not defined
                     (lambda ()
                       (fold-platform-os-type "/tmp/dist/flycheck" "dist/flycheck"))
                     ;; if defined
                     (lambda (dir)
                       dir)))
         (command (nix-maybe-call-via-flakes
                   (list "cabal" "build" "--builddir" build-dir dante-target)
                   proj-dir)))
    (flycheck-report-status 'running)
    (let ((buf (get-buffer-create (haskell-flycheck-cabal-build--buffer-name))))
      (buffer-disable-undo buf)
      (with-current-buffer buf
        (erase-buffer))
      (let* ((orig-buf (current-buffer))
             (proc (make-process :name "flycheck-haskell-cabal-build"
                                 :buffer nil
                                 :command command
                                 :noquery t
                                 :filter (lambda (process str)
                                           (with-current-buffer buf
                                             (insert str)))
                                 :sentinel (lambda (process event)
                                             (pcase (process-status process)
                                               (`signal
                                                (funcall cont 'interrupted))
                                               (`exit
                                                (funcall cont
                                                         'finished
                                                         (haskell-flycheck-cabal-build--extract-errors buf))))))))))))

(defun haskell-flycheck-cabal-build--extract-errors (buf)
  (let ((results nil))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (re-search-forward dante-error-regexp nil t)
        (let* ((file (match-string 1))
               (buf (compilation/find-buffer file))
               (location-raw (match-string 2))
               (err-type (match-string 3))
               (msg (match-string 4))

               (type (cdr (--first (string-match (car it) err-type) dante-flycheck-types)))
               (fixed-err-type (if (eq type 'error)
                                   err-type
                                 (replace-match (symbol->string type) nil nil err-type)))
               (location (dante-parse-error-location location-raw)))

          (push (flycheck-error-new-at (car location)
                                       (cadr location)
                                       type
                                       (concat fixed-err-type "\n" (trim-whitespace-right msg))
                                       :buffer buf
                                       :filename (if buf
                                                     (buffer-file-name buf)
                                                   file))
                results))))
    results))

(defun haskell-flycheck-cabal-build--buffer-name ()
  (unless dante-project-root (dante-initialize-method))
  (concat " *flycheck#" dante-target "#" dante-project-root "*"))

(flycheck-define-generic-checker 'haskell-cabal-build
  "A syntax and type checker for Haskell using a Dante worker
process."
  :start 'haskell-flycheck-cabal-build--check
  ;; :predicate (lambda () dante-mode)
  :modes '(haskell-mode haskell-literate-mode haskell-hsc-mode)
  :working-directory (lambda (_checker)
                       (unless dante-project-root (dante-initialize-method))
                       dante-project-root))

;; (flycheck-define-checker haskell-cabal-build
;;   "A Haskell syntax and type checker using `stack ghc'.
;;
;; See URL `https://github.com/commercialhaskell/stack'."
;;   :command ("cabal"
;;             "build"
;;             "--no-install-ghc"
;;             (option "--stack-yaml" flycheck-ghc-stack-project-file)
;;             (option-flag "--nix" flycheck-ghc-stack-use-nix)
;;             "ghc" "--" "-Wall" "-no-link"
;;             "-outputdir" (eval (flycheck-haskell-ghc-cache-directory))
;;             (option-list "-X" flycheck-ghc-language-extensions concat)
;;             (option-list "-i" flycheck-ghc-search-path concat)
;;             (eval (concat
;;                    "-i"
;;                    (flycheck-module-root-directory
;;                     (flycheck-find-in-buffer flycheck-haskell-module-re))))
;;             (eval flycheck-ghc-args)
;;             "-x" (eval
;;                   (pcase major-mode
;;                     (`haskell-mode "hs")
;;                     ((or `literate-haskell-mode 'haskell-literate-mode) "lhs")))
;;             source)
;;   :error-patterns
;;   ((warning line-start (file-name) ":" line ":" column ":"
;;             (or " " "\n    ") (in "Ww") "arning:"
;;             (optional " " "[" (id (one-or-more not-newline)) "]")
;;             (optional "\n")
;;             (message
;;              (one-or-more " ") (one-or-more not-newline)
;;              (zero-or-more "\n"
;;                            (one-or-more " ")
;;                            (one-or-more (not (any ?\n ?|)))))
;;             line-end)
;;    (error line-start (file-name) ":" line ":" column ":" (optional " error:")
;;           (or (message (one-or-more not-newline))
;;               (and "\n"
;;                    (message
;;                     (one-or-more " ") (one-or-more not-newline)
;;                     (zero-or-more "\n"
;;                                   (one-or-more " ")
;;                                   (one-or-more (not (any ?\n ?|)))))))
;;           line-end))
;;   :error-filter
;;   (lambda (errors)
;;     (flycheck-sanitize-errors (flycheck-dedent-error-messages errors)))
;;   :modes (haskell-mode haskell-literate-mode literate-haskell-mode)
;;   :next-checkers ((warning . haskell-hlint))
;;   :working-directory (lambda (_)
;;                        (flycheck-haskell--find-stack-default-directory))
;;   :enabled flycheck-haskell--find-stack-default-directory
;;   :verify (lambda (_)
;;             (let* ((stack (flycheck-haskell--find-stack-default-directory)))
;;               (list
;;                (flycheck-verification-result-new
;;                 :label "stack config"
;;                 :message (or stack "Not found")
;;                 :face (if stack 'success '(bold error)))))))
(provide 'haskell-flycheck-cabal-build)

;; Local Variables:
;; End:

;; haskell-flycheck-cabal-build.el ends here
