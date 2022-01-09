;; eproj-haskell.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 28 February 2015
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'common-heavy)
(require 'eproj)
(require 'eproj-ctags)
(require 'eproj-tag-index)

(require 'haskell-cabal)

;;;###autoload
(defun eproj/create-haskell-tags (proj project-files-thunk parse-tags-proc)
  (with-temp-buffer
    (with-disabled-undo
     (with-inhibited-modification-hooks
      (let ((out-buffer (current-buffer))
            (ext-re (eproj-language/extension-re
                     (gethash 'haskell-mode eproj/languages-table)))
            (fast-tags-exe
             (cached-executable-find "fast-tags")))
        (unless fast-tags-exe
          (error "Fast tags executable not found"))
        (unless (file-executable-p fast-tags-exe)
          (error "Fast tags executable does not exist: %s"
                 fast-tags-exe))
        (with-temp-buffer
          (with-disabled-undo
           (with-inhibited-modification-hooks
            (dolist (file (eproj-thunk-get-value project-files-thunk))
              (when (string-match-p ext-re file)
                (insert file "\n")))
            (unless (= 0
                       (call-process-region
                        (point-min)
                        (point-max)
                        fast-tags-exe
                        nil
                        ;; Discard error output from fast-tags
                        (list out-buffer nil)
                        nil
                        "-o-"
                        "--nomerge"
                        "-"))
              (error "fast-tags invokation failed: %s"
                     (with-current-buffer out-buffer
                       (buffer-substring-no-properties (point-min) (point-max)))))
            (erase-buffer))))
        (funcall parse-tags-proc (eproj-project/root proj) out-buffer))))))

;;;###autoload
(defun eproj/get-fast-tags-tags-from-buffer (proj-root buffer)
  "Constructs hash-table of (tag . eproj-tag) bindings extracted from buffer BUFFER.
BUFFER is expected to contain simplified output of ctags - fast-tags command.

Function does not attempt to parse <key>=<value> pairs after ;\",
and expects single character there instead (this isn't be checked at
runtime but rather will be silently relied on)."
  (with-current-buffer buffer
    (save-match-data
      (goto-char (point-min))
      (let ((tags-index (empty-eproj-tag-index))
            (gc-cons-threshold (min (* 100 1024 1024)
                                    (max gc-cons-threshold
                                         ;; Every 1000 lines takes up 1 mb or so.
                                         (/ (* (count-lines-fixed (point-min) (point-max)) 1024 1024)
                                            1000))))
            (progress-reporter (when eproj-verbose-tag-loading
                                 (let ((total-tags-count (count-lines-fixed (point-min) (point-max))))
                                   (make-standard-progress-reporter total-tags-count "tags"))))
            (file-name-cache (eproj-normalise-file-name-expand-cached/make-cache))
            (sharing-cache (eproj-ctags--make-sharing-cache)))
        (garbage-collect)
        (while (looking-at-p "^!_TAG_")
          (forward-line 1))
        (while (not (eobp))
          (beginning-of-line)
          (when (looking-at eproj-ctags--line-re)
            (let ((symbol (match-string-no-properties 1))
                  (file (eproj-ctags--share
                         (eproj-normalise-file-name-expand-cached/with-explicit-cache
                          file-name-cache
                          (match-string-no-properties 2)
                          proj-root)
                         sharing-cache))
                  (line (string->number (match-string-no-properties 3))))
              (goto-char (match-end 0))
              ;; now we're past ;"
              (skip-chars-forward "\t")
              (let ((type (char-after (point))))
                (eproj-tag-index-add! symbol
                                      file
                                      line
                                      type
                                      nil
                                      tags-index))))
          (forward-line 1)
          (when eproj-verbose-tag-loading
            (funcall progress-reporter 1)))
        tags-index))))

;;;###autoload
(defun eproj/haskell-tag-kind (tag)
  (cl-assert (eproj-tag-p tag) nil "Invalid tag: %s" tag)
  (aif (eproj-tag/type tag)
      (pcase it
        (?m "Module")
        (?f "Function")
        (?c "Class")
        (?t "Type")
        (?C "Constructor")
        (?o "Operator")
        (?p "Pattern")
        (?F "Type family")
        (?D "Preprocessor definition")
        (invalid
         (error "Invalid Haskell tag type %c" invalid)))
    "Unknown"))

;;;###autoload
(defun eproj/haskell-tag->string (proj tag-name tag)
  (cl-assert (eproj-tag-p tag))
  (concat tag-name
          " ["
          (eproj/haskell-tag-kind tag)
          "]\n"
          (eproj/format-tag-path-and-line proj tag)
          (awhen (eproj-tag/column tag)
            (concat ":" (number->string it)))
          "\n"
          (awhen (eproj/haskell-extract-tag-signature proj tag)
            (concat it "\n"))))

;;;###autoload
(defun eproj/haskell-extract-tag-signature (proj tag)
  "Fetch line where TAG is defined."
  (cl-assert (eproj-tag-p tag) nil "Eproj tag is required.")
  (let ((is-module?
         (pcase (eproj-tag/type tag)
           (?m t)
           (_  nil)))
        (inhibit-field-text-motion t))
    (unless is-module?
      (for-buffer-with-file
          (eproj-resolve-to-abs-path (eproj-tag/file tag) proj)
        (save-excursion
          (goto-line-dumb (eproj-tag/line tag))
          (eproj/haskel-extract-block)
          ;; alternative implementation with regexps
          ;; (save-match-data
          ;;   (goto-line-dumb (eproj-tag/line tag))
          ;;   (if (looking-at "^\\([^ \t\n\r\f\v].* ::\\(?: .*\n\\|\n\\)\\(?:^[ \t]+.+\n\\)*\\)")
          ;;     (match-string-no-properties 1)
          ;;     (current-line)))
          )))))

;;;###autoload
(defun eproj/haskel-extract-block ()
  "Extract indented Haskell block that starts on the current line."
  (beginning-of-line)
  (let ((start (point)))
    (cl-symbol-macrolet
        ((advance
          (progn
            (forward-line 1)
            (beginning-of-line)
            (skip-chars-forward " \t"))))
      (skip-chars-forward " \t")
      (let ((col (current-column)))
        ;; actualy this is a loop with postcondition
        advance
        (while (< col (current-column))
          advance)
        (let ((previous-line-end (line-end-position 0)))
          (buffer-substring-no-properties start previous-line-end))))))

(defun eproj-haskell--cabal--get-field (name)
  "Try to read value of field with NAME from current buffer."
  (save-match-data
    (save-excursion
      (let ((case-fold-search t)
            (res nil))
        (goto-char (point-min))
        (while (re-search-forward
                (concat "^[ \t]*" (regexp-quote name)
                        (rx-let ((nl (any ?\n ?\r))
                                 (ws (any ?\s ?\t))
                                 (wsnl (any ?\s ?\t ?\n ?\r)))
                          (rx ":"
                              (* ws)
                              (group-n 1
                                       (* any)
                                       (* (group-n 2
                                                   nl
                                                   (+ wsnl)
                                                   wsnl
                                                   (* any)))))))
                nil t)
          (let ((val (match-string-no-properties 1))
                (start 1))
            (when (match-end 2) ;; Multiple lines.
              ;; The documentation is not very precise about what to do about
              ;; the \n and the indentation: are they part of the value or
              ;; the encoding?  I take the point of view that \n is part of
              ;; the value (so that values can span multiple lines as well),
              ;; and that only the first char in the indentation is part of
              ;; the encoding, the rest is part of the value (otherwise, lines
              ;; in the value cannot start with spaces or tabs).
              (while (string-match "^[ \t]\\(?:\\.$\\)?" val start)
                (setq start (1+ (match-beginning 0)))
                (setq val (replace-match "" t t val))))
            (push val res)))
        res))))

(defun eproj-haskell--get-related-projects-from-cabal-proj (root cabal-proj-file)
  (cl-assert (file-regular-p cabal-proj-file))
  (let* ((dir (file-name-directory cabal-proj-file))
         (dir-norm (eproj-normalise-file-name-expand-cached dir nil)))
    (with-temp-buffer
      (insert-file-contents cabal-proj-file)

      ;; Make sure current directory is removed so that we won’t have infinite loop here.
      (--remove (or (string= root it)
                    (string= root (eproj-get-initial-project-root it)))
                (--map (if (file-regular-p it) (file-name-directory it) it)
                       (--map (eproj-normalise-file-name-expand-cached (eproj-haskell--trim-quotes it)
                                                                       root)
                              (--remove (string-prefix-p "--" it)
                                        (-map #'trim-whitespace
                                              (mapcan #'split-into-lines
                                                      (eproj-haskell--cabal--get-field "packages"))))))))))

(defun eproj-haskell--trim-quotes (str)
  "Trim leading and tailing \" from STR."
  (cl-assert (stringp str))
  (delete ?\" str))

;;;###autoload
(defun eproj--infer-haskell-project (root)
  (let ((cabal-proj-file (concat root "/cabal.project"))
        (cabal-proj-local-file (concat root "/cabal.project.local")))
    (cond
      ((file-exists-p cabal-proj-file)
       (let* ((related (eproj-haskell--get-related-projects-from-cabal-proj root cabal-proj-file))
              (related-local (when (file-exists-p cabal-proj-local-file)
                               (eproj-haskell--get-related-projects-from-cabal-proj root cabal-proj-local-file)))
              (all-related (nconc related related-local)))
         (awhen (--filter (and (file-name-absolute-p it) (not (file-exists-p it))) all-related)
           (error "Some related projects inferred from cabal.project do not exist: %s"
                  (s-join ", " it)))
         `((languages haskell-mode)
           (related ,@all-related)
           (flycheck-checker
            (haskell-mode lsp)))))
      ((file-exists-p (concat root "/package.yaml"))
       '((languages haskell-mode)
         (flycheck-checker
          (haskell-mode lsp))))
      ((directory-files root
                        nil
                        (rx (or ".cabal"
                                (seq "stack" (* any) "." (or "yml" "yaml")))
                            eos)
                        t)
       '((languages haskell-mode)
         (flycheck-checker
          (haskell-mode lsp))))
      (t
       nil))))

(provide 'eproj-haskell)

;; Local Variables:
;; End:

;; eproj-haskell.el ends here
