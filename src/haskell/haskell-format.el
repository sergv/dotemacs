; haskell-format.el --- -*- lexical-binding: t; -*--

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  2 May 2017
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'common)
(require 'flycheck-haskell)
(require 'haskell-regexen)

(defvar haskell-format-default-width 100)

(defun haskell-format--format-region-preserving-position (indent-offset width start end format-with-brittany?)
  "Format region between START and END positions. When done try to return point
to the line where it was located before the formatting."
  (let* ((p (point))
         (line-before (buffer-substring-no-properties (line-beginning-position) (point)))
         (line-after (buffer-substring-no-properties (point) (line-end-position)))
         (use-line-after? (< (length line-before) (length line-after)))
         (fingerprint-re (haskell-format--fingerprint-re
                          (if use-line-after?
                              line-after
                            line-before))))
    (if format-with-brittany?
        (with-marker (end-mark (copy-marker end))
          (haskell-format--format-with-brittany indent-offset
                                                (if (and width
                                                         (< 1 width))
                                                    width
                                                  haskell-format-default-width)
                                                start
                                                end)
          (goto-char start)
          (if (re-search-forward fingerprint-re end-mark t)
              (goto-char (if use-line-after? (match-beginning 0) (match-end 0)))
            (goto-char p)))
      (haskell-format--format-region-by-toplevel-chunks-with-treesitter! start end))))

(defun haskell-format--format-region-with-treesitter-preserving-position (start end)
  (with-marker (p (point-marker))
    (haskell-format--format-region-by-toplevel-chunks-with-treesitter! (min start end) (max start end))
    (goto-char p)
    ;; Skip indentation, sometimes treesitter formatter puts p at 0th
    ;; column for some unknown reason.
    (move-to-column (max (current-column) (indentation-size)))))

(defun haskell-format--format-region-by-toplevel-chunks-with-treesitter! (start end)
  "Format the region begin START and END positions line by line using treesitter rules."
  (cl-assert (numberp start))
  (cl-assert (numberp end))
  (cl-assert (<= start end))
  (goto-char start)
  (skip-whitespace-forward)
  (with-marker (m (copy-marker start))
    (with-marker (end-mark (copy-marker end))
      (with-undo-amalgamate
        (with-inhibited-modification-hooks
         (while (< (point) end-mark)
           (let ((beg (point))
                 (topmost-end-pos (save-excursion
                                    (haskell-move-to-topmost-end)
                                    (point))))
             (goto-char (min topmost-end-pos end-mark))
             (let* ((line-count (count-lines-fixed beg (point)))
                    (treesit--indent-region-batch-size (max treesit--indent-region-batch-size
                                                            (+ (* 2 line-count) 10))))
               (set-marker m (point))
               (treesit-indent-region beg (point))
               (goto-char m)
               (skip-whitespace-forward)))))))))

(defun haskell-format--fingerprint-re (str)
  "Take current line and come up with a fingerprint
regexp that will find this line after applying indentation or some
other form of whitespace normalization.

E.g. given a line like

>      foo = bar $ baz (quux fizz) frob

the regex should look like

foo\\w*=\\w*bar\\w*[$]\\w*baz\\w*[(]\\w*quux\\w*fizz\\w*[)]\\w*frob

where \\w matches any whitespace including newlines"
  (s-join "[ \t\r\n]*"
          (--map (regexp-quote it)
                 (--filter (not (s-blank-str? it))
                           (--map (list->string it)
                                  (-partition-by #'char-syntax
                                                 (string->list
                                                  (s-collapse-whitespace
                                                   (s-trim
                                                    str)))))))))

(defun haskell-format--get-language-extensions (buf &optional without-properties)
  "Get all LANGUAGE pragma extensions from buffer BUF as a list of strings."
  (with-current-buffer buf
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (let* ((case-fold-search t) ;; Ignore case when matching regexps.
               (exts nil)
               (get-match-string (if without-properties
                                     #'match-string-no-properties
                                   #'match-string))
               (module-header-position
                (save-excursion
                  ;; Do case-sensitive search for "module" declaration.
                  (let ((case-fold-search nil))
                    (when (re-search-forward haskell-regexen/module-header-start nil t)
                      (match-beginning 0))))))
          (while (re-search-forward
                  (eval-when-compile
                    (concat haskell-regexen/language-pragma-prefix
                            (rx (group-n 1 (*? anything))
                                (* (regexp "[ \t\r\n]"))
                                "#-}")))
                  module-header-position ;; bound
                  t                      ;; no error
                  )
            (let ((new-exts (split-string (funcall get-match-string 1)
                                          "[, \t\r\n]+"
                                          t ;; omit nulls
                                          )))
              (setf exts (nconc new-exts exts))))
          exts)))))

(defun haskell-format--format-with-brittany (indent width start end)
  "Format region using brittany haskell formatter."
  (unless (integerp indent)
    (error "Indentation must be an integer: %s" indent))
  (unless (integerp width)
    (error "Width must be an integer: %s" width))
  (let* ((buffer-exts (haskell-format--get-language-extensions (current-buffer) t))
         (cabal-exts
          (let ((buf (current-buffer)))
            (when-let ((config (flycheck-haskell-get-configuration-for-buf buf (eproj-get-project-for-buf-lax buf))))
              (cdr (assq 'extensions config)))))
         (language-extensions
          (remove-duplicates-sorting (nconc buffer-exts
                                            cabal-exts)
                                     #'string=
                                     #'string<))
         (opts (mapconcat (lambda (x) (concat "-X" x))
                          (--filter (not (string= "CPP" it)) language-extensions)
                          " ")))
    (goto-char start)
    (call-process-region
     start
     end
     (or (cached-executable-find "brittany")
         (error "Formatting failed: brittany executable not found"))
     ;;"/home/sergey/projects/haskell/projects/dev-tools/brittany/.stack-work-master/install/x86_64-linux-tinfo6/lts-12.14/8.4.3/bin/brittany"
     t ;; delete
     t ;; insert into current buffer before point
     t ;; display
     "--indent" (number-to-string indent)
     "--columns" (number-to-string width)
     "--output-on-errors"
     "--ghc-options" opts)
    (goto-char start)))

(provide 'haskell-format)

;; Local Variables:
;; End:

;; haskell-format.el ends here
