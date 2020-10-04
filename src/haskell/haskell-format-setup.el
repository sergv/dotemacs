;; haskell-format-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  2 May 2017
;; Description:

(require 'common)

(defvar haskell-format-default-width 90)

;;;###autoload
(defun haskell-format-pp-region-with-brittany (width)
  "Format selected region using brittany haskell formatter."
  (interactive "p*")
  (with-region-bounds start end
    (haskell-format--format-with-brittany
     haskell-indent-offset
     (if (< 1 width)
         width
       haskell-format-default-width)
     start
     end)))

(defun haskell-format--get-language-extensions (buf &optional without-properties)
  "Get all LANGUAGE pragma extensions from buffer BUF as a list of strings."
  (with-current-buffer buf
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (let* ((case-fold-search t) ;; Ignore case when matching regexps.
               (result (cons t nil))
               (extensions result)
               (get-match-string (if without-properties
                                     #'match-string-no-properties
                                   #'match-string))
               (module-header-position
                (save-excursion
                  ;; Do case-sensitive search for "module" declaration.
                  (let ((case-fold-search nil))
                    (when (re-search-forward "\\_<module\\_>[ \r\n]" nil t)
                      (match-beginning 0))))))
          (while (re-search-forward
                  (rx-let ((wh (* (regexp "[ \v\f\r\n]"))))
                    (rx "{-#"
                        wh
                        "LANGUAGE"
                        (group (regexp "\\(?:.\\|[\r\n]\\)*?"))
                        wh
                        "#-}"))
                  module-header-position ;; bound
                  t                      ;; no error
                  )
            (let ((exts (split-string (funcall get-match-string 1)
                                      "[, \v\f\r\n]+"
                                      t ;; omit nulls
                                      )))
              (dolist (e exts)
                (setf (cdr extensions) (cons e nil)
                      extensions (cdr extensions)))))
          (cdr result))))))

(defun haskell-format--format-with-brittany (indent width start end)
  "Format region using brittany haskell formatter."
  (unless (integerp indent)
    (error "Indentation must be an integer: %s" indent))
  (unless (integerp width)
    (error "Width must be an integer: %s" width))
  (let* ((language-extensions
          (haskell-format--get-language-extensions (current-buffer) t))
         (opts (mapconcat (lambda (x) (concat "-X" x))
                          (--filter (not (string= "CPP" it)) language-extensions)
                          " ")))
    (goto-char start)
    (call-process-region
     start
     end
     (cached-executable-find "brittany")
     ;;"/home/sergey/projects/haskell/projects/dev-tools/brittany/.stack-work-master/install/x86_64-linux-tinfo6/lts-12.14/8.4.3/bin/brittany"
     t ;; delete
     t ;; insert into current buffer before point
     t ;; display
     "--indent" (number-to-string indent)
     "--columns" (number-to-string width)
     "--output-on-errors"
     "--ghc-options" opts)
    (goto-char start)))

(provide 'haskell-format-setup)

;; Local Variables:
;; End:

;; haskell-format-setup.el ends here
