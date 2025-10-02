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
        (let* ((file (match-string 4))
               (buf (compilation/find-buffer file))
               (location-raw (match-string 5))
               (err-type (match-string 6))
               (msg (match-string 7))

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
  "A syntax and type checker for Haskell using regular ‘cabal build’.

Not as low-latency as ‘haskell-dante’ checker, but works with .hsc files."
  :start 'haskell-flycheck-cabal-build--check
  :modes '(haskell-mode haskell-ts-mode haskell-literate-mode haskell-hsc-mode)
  :working-directory (lambda (_checker)
                       (unless dante-project-root (dante-initialize-method))
                       dante-project-root))

(provide 'haskell-flycheck-cabal-build)

;; Local Variables:
;; End:

;; haskell-flycheck-cabal-build.el ends here
