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
             (proc
              (let ((default-directory proj-dir))
                (make-process :name "flycheck-haskell-cabal-build"
                              :buffer buf
                              :command command
                              :noquery t
                              :sentinel (lambda (process event)
                                          (pcase (process-status process)
                                            (`signal
                                             (funcall cont 'interrupted))
                                            (`exit
                                             (let ((result (haskell-flycheck-cabal-build--extract-errors buf proj-dir process)))
                                               (funcall cont (car result) (cdr result))))))))))))))

(defun haskell-flycheck-cabal-build--extract-errors (buf dir proc)
  (let ((results nil))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (re-search-forward dante-error-regexp nil t)
        (let* ((file (match-string 4))
               (b (compilation/find-buffer file dir dir))
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
                                       :buffer b
                                       :filename (if b
                                                     (buffer-file-name b)
                                                   file))
                results))))
    (cond
      (results (cons 'finished results))
      ((not (zerop (process-exit-status proc)))
       (let ((sep "\n--------------------------------\n"))
         (cons 'errored
               (format "Cabal checker failed!%sDirectory: ‘%s’%s%s%s%s"
                       sep
                       (buffer-local-value 'default-directory (process-buffer proc))
                       sep
                       (join-lines (process-command proc) " ")
                       sep
                       (with-current-buffer buf
                         (buffer-substring-no-properties (point-min) (point-max)))))))
      (t
       ;; Finished with no errors.
       (cons 'finished nil)))))

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
