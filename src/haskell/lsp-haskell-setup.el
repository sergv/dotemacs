;; lsp-haskell-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 21 December 2021
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'lsp-haskell)
(require 'lsp-setup)

(setf lsp-haskell-formatting-provider "none"

      ;; lsp-haskell-server-args
      ;; `("-d" "-l" ,lsp-haskell-server-log-file)
      lsp-haskell-server-args nil)

(defun lsp-ui-sideline--haskell-margin-width (old-margin-width)
  (if (derived-mode-p 'haskell-mode)
      (+ 1
         (if fringes-outside-margins right-margin-width 0))
    (funcall old-margin-width)))

(advice-add 'lsp-ui-sideline--margin-width :around #'lsp-ui-sideline--haskell-margin-width)

(defun lsp-haskell-get-range-text (range)
  (-let* (((&Range :start (&Position :character start-char :line start-line)
                   :end (&Position :character end-char :line end-line))
           range)
          (start-pos
           (save-excursion
             (goto-line-dumb (lsp-translate-line (1+ start-line)))
             (move-to-character-column (lsp-translate-column start-char))
             (point)))
          (end-pos
           (save-excursion
             (goto-line-dumb (lsp-translate-line (1+ end-line)))
             (move-to-character-column (lsp-translate-column end-char))
             (point))))
    (buffer-substring-no-properties start-pos end-pos)))

(defun lsp-haskell-type-at-point (insert)
  (interactive "P")
  (-let* (((hover &as &Hover? :contents :range?)
           (lsp-request "textDocument/hover"
                        (lsp--text-document-position-params)))
          (response-markdown
           (if (and (lsp-markup-content? contents)
                    (string= (lsp:markup-content-kind contents) lsp/markup-kind-markdown))
               (lsp:markup-content-value contents)
             (error "Expected markdown MarkupContent but got something else: %s" contents))))

    ;; Don’t do any processing if lsp didn’t identify any identifiers at point.
    (when (string= "" response-markdown)
      (error "No identifier at point"))

    (let* ((lines
            (reverse
             (--take-while (not (s-starts-with? "```haskell" it))
                           (cdr
                            (--drop-while (not (s-starts-with? "```" it))
                                          (reverse
                                           (s-lines response-markdown)))))))
           (orig-text (lsp-haskell-get-range-text range?))
           (ty
            (replace-regexp-in-string "_ ::"
                                      (concat (if (and (string-match-p (concat "^" haskell-regexen/sym "$") orig-text))
                                                  (concat "(" orig-text ")")
                                                orig-text)
                                              " ::")
                                      (s-join "\n" lines))))

      (if insert
          (save-excursion (goto-char (line-beginning-position))
                          (insert (dante-fontify-expression ty) "\n"))
        (message "%s" (dante-fontify-expression ty))))))

(cl-defun setup-lsp-haskell-symbnav (&key (bind-keybindings t))
  (setq-local xref-show-definitions-function #'eproj-xref-symbnav-show-xrefs
              xref-show-xrefs-function #'eproj-xref-symbnav-show-xrefs)
  (when bind-keybindings
    (awhen (current-local-map)
      (def-keys-for-map it
        ("C-." lsp-haskell-symbnav/go-to-symbol-home)
        ("C-," lsp-haskell-symbnav/go-back)
        ("C-?" lsp-symbnav/find-references)

        ("M-." eproj-symbnav/go-to-symbol-home)
        ("M-," eproj-symbnav/go-back)
        ("M-?" xref-find-references)))
    (def-keys-for-map vim-normal-mode-local-keymap
      ("C-." lsp-haskell-symbnav/go-to-symbol-home)
      ("C-," lsp-haskell-symbnav/go-back)
      ("C-?" lsp-symbnav/find-references)

      ("M-." eproj-symbnav/go-to-symbol-home)
      ("M-," eproj-symbnav/go-back)
      ("M-?" xref-find-references))))

(defalias 'lsp-haskell-symbnav/go-back #'eproj-symbnav/go-back)

(defun lsp-haskell-symbnav/go-to-symbol-home (&optional use-regexp?)
  (interactive "P")
  (if use-regexp?
      (let* ((re (read-regexp "enter regexp to search for"))
             (lsp-tags
              (-map #'lsp-symbnav--symbol-information->eproj-tag-triple
                    (lsp-request "workspace/symbol" `(:query ,re)))))
        (if lsp-tags
            (lsp-haskell-symbnav/go-to-symbol-home-impl re lsp-tags)
          (eproj-symbnav/go-to-symbol-home-impl re use-regexp?)))
    (let* ((identifier (eproj-symbnav/identifier-at-point nil))
           (lsp-tags
            (lsp-request "textDocument/definition"
                         (lsp--text-document-position-params))))
      (if lsp-tags
          (lsp-haskell-symbnav/go-to-symbol-home-impl
           identifier
           (--map (list identifier it nil)
                  (lsp-symbnav--locations->eproj-tags identifier lsp-tags)))
        (lsp-haskell-symbnav/go-to-symbol-home/with-lsp-location-hint identifier)))))

(defun lsp-haskell-symbnav/go-to-symbol-home-impl (ident tags)
  (let ((proj nil)
        (enable-shortcut? t))
    (eproj-symbnav/choose-location-to-jump-to
     ident
     (lambda (_proj tag-name tag)
       (cl-assert (stringp tag-name))
       (concat tag-name
               (awhen (eproj-tag/type tag)
                 (concat " [" it "]"))
               "\n"
               (eproj-xref-symbnav--tag->string tag)))
     #'lsp-symbnav--tag-kind
     (eproj-symbnav-get-file-name)
     proj
     (eproj-symbnav-current-home-entry)
     tags
     enable-shortcut?
     "Choose symbol\n\n")))

(defun lsp-haskell-symbnav/go-to-symbol-home/with-lsp-location-hint (identifier)
  (interactive "P")
  (-let* (((hover &as &Hover? :contents :range?)
           (lsp-request "textDocument/hover"
                        (lsp--text-document-position-params))))

    (unless contents
      (error "No response from server, invoke eproj tags via M-."))

    (let ((response-markdown
           (if (and (lsp-markup-content? contents)
                    (string= (lsp:markup-content-kind contents) lsp/markup-kind-markdown))
               (lsp:markup-content-value contents)
             (error "Expected markdown MarkupContent but got something else: %s" contents))))

      ;; Don’t do any processing if lsp didn’t identify any identifiers at point.
      (when (string= "" response-markdown)
        (error "Empty response from server, invoke eproj tags via M-."))

      (unless (string-match-p (regexp-quote identifier) (lsp-haskell-get-range-text range?))
        (error "Identifier at point for Emacs and LSP doesn’t match: ‘%s’ vs ‘%s’"
               identifier
               (lsp-haskell-get-range-text range?)))

      (save-match-data
        (unless (string-match (rx "*Defined in ‘"
                                  (group (+ (not (any ?\n ?\s ?\’)))) "’*"
                                  (? ?\r)
                                  ?\n
                                  " *(" (group (+ (not (any ?\n ?\))))) ")*")
                              response-markdown)
          (error "Response does not contain hint about where ‘%s’ is defined"
                 identifier))

        (let* ((module (match-string-no-properties 1 response-markdown))
               (package (match-string-no-properties 2 response-markdown))
               (package-without-version
                (replace-regexp-in-string (rx ?- (+ (any (?0 . ?9) ?.)) eos) "" package)))


          (haskell-symbnav--jump-to-filtered-tags
           identifier
           (concat "/"
                   "\\(:?" (regexp-quote package-without-version) "\\)"
                   ".*"
                   "/"
                   (replace-regexp-in-string "\\." "/" module)
                   "."
                   (eval-when-compile (regexp-opt +haskell-extensions+)))))))))

(defun haskell-symbnav--jump-to-filtered-tags (identifier filter-re)
  (let* ((proj (eproj-get-project-for-buf (current-buffer)))
         (effective-major-mode (eproj/resolve-synonym-modes major-mode)))

    (eproj-symbnav/ensure-tags-loaded! effective-major-mode proj)

    (let* ((candidate-tags
            (eproj-get-matching-tags proj
                                     effective-major-mode
                                     identifier
                                     nil))
           (filtered-tags
            (--filter (and (not (equal ?m (eproj-tag/type (cadr it))))
                           (string-match-p filter-re (eproj-tag/file (cadr it))))
                      candidate-tags))

           (lang (aif (gethash effective-major-mode eproj/languages-table)
                     it
                   (error "unsupported language %s" effective-major-mode)))
           (tag->string (eproj-language/tag->string-func lang))
           (tag->kind (eproj-language/show-tag-kind-procedure lang)))

      (eproj-symbnav/choose-location-to-jump-to
       identifier
       tag->string
       tag->kind
       (eproj-symbnav-get-file-name)
       proj
       (eproj-symbnav-current-home-entry)
       (or filtered-tags
           candidate-tags)
       t
       "Choose symbol\n\n"))))

(provide 'lsp-haskell-setup)

;; Local Variables:
;; End:

;; lsp-haskell-setup.el ends here
