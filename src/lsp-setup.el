;; lsp-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created:  7 January 2021
;; Description:

(eval-when-compile
  (defvar lsp-diagnostics-attributes)
  (defvar lsp-modeline-code-actions-segments)
  (defvar lsp-modeline-diagnostics-scope)
  (defvar lsp-rust-analyzer-cargo-load-out-dirs-from-check)
  (defvar lsp-rust-analyzer-display-chaining-hints)
  (defvar lsp-rust-analyzer-display-parameter-hints)
  (defvar lsp-rust-analyzer-proc-macro-enable)
  (defvar lsp-rust-analyzer-server-args)
  (defvar lsp-rust-analyzer-server-command)
  (defvar lsp-rust-analyzer-server-display-inlay-hints)
  (defvar lsp-rust-crate-blacklist)
  (defvar lsp-rust-full-docs)
  (defvar lsp-rust-racer-completion)
  (defvar lsp-rust-server)
  (defvar lsp-rust-target-dir)
  (defvar lsp-ui-sideline-enable)
  (defvar lsp-ui-sideline-show-code-actions)
  (defvar lsp-ui-sideline-show-diagnostics)
  (defvar lsp-ui-sideline-show-hover))

(require 'common)
(require 'eproj-ctags)

(require 'lsp)
(require 'lsp-ui-doc)
(require 's)

(setf lsp-client-packages '(lsp-rust lsp-clangd lsp-cmake)

      ;; Disable LSP sessions.
      lsp-session-file nil

      ;; lsp-log-io t
      ;; lsp-print-performance t

      lsp-disabled-clients
      '(ccls lsp-ada lsp-angular lsp-bash lsp-clojure
             lsp-crystal lsp-csharp lsp-css lsp-dart lsp-dhall lsp-dockerfile lsp-elm
             lsp-elixir lsp-erlang lsp-eslint lsp-fortran lsp-fsharp lsp-gdscript lsp-go
             lsp-hack lsp-groovy lsp-haskell lsp-haxe lsp-java lsp-javascript lsp-json
             lsp-kotlin lsp-lua lsp-nim lsp-nix lsp-metals lsp-ocaml lsp-perl lsp-php lsp-pwsh
             lsp-pyls lsp-python-ms lsp-purescript lsp-r lsp-rf lsp-solargraph lsp-sorbet
             lsp-tex lsp-terraform lsp-vala lsp-verilog lsp-vetur lsp-vhdl lsp-vimscript lsp-xml
             lsp-yaml lsp-sqls lsp-svelte lsp-steep)

      lsp-diagnostics-provider :flycheck

      lsp-file-watch-threshold nil
      lsp-vscode-ext-url nil

      lsp-headerline-breadcrumb-enable nil

      lsp-modeline-code-actions-segments '(count)
      ;; lsp-modeline-diagnostics-scope :workspace
      lsp-modeline-diagnostics-scope :file

      lsp-auto-execute-action nil
      lsp-enable-relative-indentation t

      lsp-ui-sideline-enable nil
      lsp-ui-sideline-show-diagnostics nil
      lsp-ui-sideline-show-hover t
      lsp-ui-sideline-show-code-actions nil

      ;; lsp-progress-via-spinner nil

      lsp-rust-server 'rust-analyzer
      lsp-rust-analyzer-server-command '("rust-analyzer")
      lsp-rust-analyzer-server-args '(nil "--log-file" "/tmp/rust-analyzer-log.txt")

      lsp-ui-doc-enable nil
      lsp-signature-auto-activate nil
      lsp-signature-render-documentation nil
      lsp-eldoc-enable-hover nil

      ;; ;; For debugging
      ;; lsp-response-timeout 10000
      ;; lsp-rust-analyzer-server-command '("/tmp/target/release/rust-analyzer")

      lsp-rust-crate-blacklist []
      lsp-rust-racer-completion nil
      lsp-rust-target-dir (fold-platform-os-type "/tmp/target/rls" nil)
      lsp-rust-full-docs t

      lsp-rust-analyzer-proc-macro-enable t
      lsp-rust-analyzer-cargo-run-build-scripts t

      lsp-diagnostics-attributes
      '((unnecessary :foreground "#666666")
        (deprecated  :strike-through t))

      lsp-rust-analyzer-server-display-inlay-hints t
      lsp-rust-analyzer-display-parameter-hints t
      lsp-rust-analyzer-display-chaining-hints t)

;;;###autoload
(defun lsp-doc-other-window ()
  (interactive)
  (let ((buf (current-buffer)))
    (lsp-request-async
     "textDocument/hover"
     (lsp--text-document-position-params)
     (lambda (hover)
       (lsp-doc-other-window--callback hover buf))
     :mode 'current ;; 'tick
     :cancel-token :lsp-doc-hover)))

;;;; lsp-doc-presentation-mode

(defvar lsp-doc-presentation--prev-contents nil)
(defvar lsp-doc-presentation--next-contents nil)
(defvar lsp-doc-presentation--current-doc nil)

(defun lsp-doc-presentation-set-up-buffer (contents)
  (with-inhibited-read-only
    (erase-buffer)
    (insert (setf lsp-doc-presentation--current-doc contents))
    (lsp-ui-doc--make-clickable-link)))

(defun lsp-doc-presentation-go-back ()
  (interactive)
  (if (null lsp-doc-presentation--prev-contents)
      (error "No more previous history items")
    (let ((new-contents (car lsp-doc-presentation--prev-contents)))
      (setf lsp-doc-presentation--prev-contents (cdr lsp-doc-presentation--prev-contents))
      (awhen lsp-doc-presentation--current-doc
        (push it lsp-doc-presentation--next-contents))
      (lsp-doc-presentation-set-up-buffer new-contents))))

(defun lsp-doc-presentation-go-forward ()
  (interactive)
  (if (null lsp-doc-presentation--next-contents)
      (error "No more next history items")
    (let ((new-contents (car lsp-doc-presentation--next-contents)))
      (setf lsp-doc-presentation--next-contents (cdr lsp-doc-presentation--next-contents))
      (awhen lsp-doc-presentation--current-doc
        (push it lsp-doc-presentation--prev-contents))
      (lsp-doc-presentation-set-up-buffer new-contents))))

(defvar lsp-doc-presentation-mode-map
  (let ((map (make-sparse-keymap)))
    (def-keys-for-map map
      +vi-keys+
      +vim-search-keys+
      +vim-search-extended-keys+
      +vim-mock:word-motion-keys+
      +vim-special-keys+
      ("<up>"   lsp-doc-presentation-go-back)
      ("<down>" lsp-doc-presentation-go-forward)
      ("v"      set-mark-command)
      ("y"      copy-region-as-kill))
    map)
  "Keymap for Rust doc mode.")

(define-derived-mode lsp-doc-presentation-mode special-mode "Rust doc"
  "Major mode to presenting Rust documentation."
  :group 'rust-mode
  (setq-local truncate-lines t)
  (read-only-mode +1))

(lsp-defun lsp-doc-other-window--callback ((hover &as &Hover? :contents) buf)
  (unless contents
    (error "Empty response from server. Aborting"))
  (when (equal buf (current-buffer))
    (let ((doc-buf (get-buffer-create "*doc*")))
      (with-current-buffer doc-buf
        (lsp-doc-presentation-mode)
        (awhen lsp-doc-presentation--current-doc
          (push it lsp-doc-presentation--prev-contents))
        (lsp-doc-presentation-set-up-buffer
         (replace-regexp-in-string "\r" "" (lsp-ui-doc--extract contents))))
      (switch-to-buffer-other-window doc-buf))))

(defvar-local lsp-rust--has-experimental-hover-range-cap +undef+)

(defun lsp-rust-type-of-thing-at-point (&optional goto-def)
  "Display the type of the thing at point. With universal
argument jump to type definition."
  (interactive "P")
  (lsp--check-capability "experimental/hoverRange"
    lsp-rust--has-experimental-hover-range-cap)
  (let* ((bounds (if (region-active-p)
                     (with-region-bounds start end
                       (cons start end))
                   (bounds-of-thing-at-point 'symbol)))
         (range (lsp--text-document-range-params (car bounds) (cdr bounds))))
    (lsp-request-async
     "textDocument/hover"
     range
     (lambda (hover)
       (lsp-rust-type-of-thing-at-point--callback hover))
     :mode 'current ;; 'tick
     :cancel-token :lsp-doc-type-at-point)))

(defun lsp-rust-type-of-thing-at-point--callback (response)
  (let ((contents (lsp:hover-contents response)))
    (if (and contents
             (not (equal contents "")))
        (message "%s" (string-trim-right (lsp--render-on-hover-content contents t)))
      (lsp--info "No content at point."))))

(defun lsp-rust-type-at-point-dumb ()
  (interactive)
  (let ((buf (current-buffer)))
    (lsp-request-async
     "textDocument/hover"
     (lsp--text-document-position-params)
     (lambda (hover)
       (lsp-rust-type-at-point-dumb--callback hover buf))
     :mode 'current ;; 'tict
     :cancel-token :lsp-doc-hover)))

(lsp-defun lsp-rust-type-at-point-dumb--callback ((hover &as &Hover? :contents) buf)
  (unless contents
    (error "Empty response from server. Aborting"))
  (when (equal buf (current-buffer))
    (if (and (lsp-markup-content? contents)
             (string= (lsp:markup-content-kind contents) lsp/markup-kind-markdown))
        (let* ((lines (--drop-while (not (s-starts-with? "```" it))
                                    (s-lines (lsp:markup-content-value contents))))
               (blocks (--partition-by (equal "```" it) lines)))
          (pcase blocks
            (`(,x . (,y . nil))
             (message
              (lsp-ui-doc--extract-marked-string (s-join "\n" (append x y))
                                                 lsp/markup-kind-markdown)))
            (`(,x . (,y . (,z . (,w . ,_))))
             (message
              (lsp-ui-doc--extract-marked-string (s-join "\n" (append x y z w))
                                                 lsp/markup-kind-markdown)))
            (other
             (error "Unhandled case: %s" other))))
      (error "Expected markdown MarkupContent but got something else: %s" contents))))

;;;; Define some faces so that they’ll look ok

(defface lsp-lsp-flycheck-warning-unnecessary-face
  '((t :foreground "#666666"))
  "Face for text that will evaporate when modified/overwritten."
  :group 'lsp-faces)

;;;; Symbnav

(defun* setup-lsp-symbnav (&key (bind-keybindings t))
  (setq-local xref-show-definitions-function #'eproj-xref-symbnav-show-xrefs
              xref-show-xrefs-function #'eproj-xref-symbnav-show-xrefs)
  (when bind-keybindings
    (awhen (current-local-map)
      (def-keys-for-map it
        ("C-." lsp-symbnav/go-to-symbol-home)
        ("C-," lsp-symbnav/go-back)
        ("C-?" lsp-symbnav/find-references)

        ("M-." eproj-symbnav/go-to-symbol-home)
        ("M-," eproj-symbnav/go-back)
        ("M-?" xref-find-references)))
    (def-keys-for-map vim:normal-mode-local-keymap
      ("C-." lsp-symbnav/go-to-symbol-home)
      ("C-," lsp-symbnav/go-back)
      ("C-?" lsp-symbnav/find-references)

      ("M-." eproj-symbnav/go-to-symbol-home)
      ("M-," eproj-symbnav/go-back)
      ("M-?" xref-find-references))))

(defalias 'lsp-symbnav/go-back #'eproj-symbnav/go-back)

(defsubst lsp-symbnav--tag-kind (tag)
  (awhen (eproj-tag/type tag)
    (cl-assert (stringp it))
    it))

(defun lsp-symbnav/go-to-symbol-home (&optional use-regexp?)
  (interactive "P")
  (let ((ident-and-tags
         (if use-regexp?
             (let ((re (read-regexp "enter regexp to search for")))
               (cons re
                     (-map #'lsp-symbnav--symbol-information->eproj-tag-triple
                           (lsp-request "workspace/symbol" `(:query ,re)))))
           (let ((identifier (eproj-symbnav/identifier-at-point nil)))
             (cons identifier
                   (--map (list identifier it nil)
                          (lsp-symbnav--locations->eproj-tags
                           identifier
                           (lsp-request "textDocument/definition"
                                        (lsp--text-document-position-params)))))))))
    (destructuring-bind (ident . tags) ident-and-tags
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
         "Choose symbol\n\n")))))

(defun lsp-symbnav/find-references (&optional include-declaration?)
  (interactive "P")
  (let* ((identifier (eproj-symbnav/identifier-at-point nil))
         (tag-triples
          (--map (list identifier it nil)
                 (lsp-symbnav--locations->eproj-tags
                  identifier
                  (lsp-request "textDocument/references"
                               (lsp--make-reference-params nil include-declaration?))))))
    (let ((proj nil)
          (enable-shortcut? nil))
      (eproj-symbnav/choose-location-to-jump-to
       identifier
       (lambda (_proj _tag-name tag)
         (eproj-xref-symbnav--tag->string tag))
       #'lsp-symbnav--tag-kind
       (eproj-symbnav-get-file-name)
       proj
       (eproj-symbnav-current-home-entry)
       tag-triples
       enable-shortcut?
       (concat "Uses of " identifier "\n\n")))))

(defun lsp-symbnav/find-implementations ()
  (interactive)
  (let* ((identifier (eproj-symbnav/identifier-at-point nil))
         (tag-triples
          (--map (list identifier it nil)
                 (lsp-symbnav--locations->eproj-tags
                  identifier
                  (lsp-request "textDocument/implementation"
                               (lsp--text-document-position-params))))))
    (let ((proj nil)
          (enable-shortcut? nil))
      (eproj-symbnav/choose-location-to-jump-to
       identifier
       (lambda (_proj _tag-name tag)
         (eproj-xref-symbnav--tag->string tag))
       #'lsp-symbnav--tag-kind
       (eproj-symbnav-get-file-name)
       proj
       (eproj-symbnav-current-home-entry)
       tag-triples
       enable-shortcut?
       (concat "Implementations for " identifier "\n\n")))))

;; sync with `lsp--symbol-information-to-xref’
(lsp-defun lsp-symbnav--symbol-information->eproj-tag-triple
  ((&SymbolInformation :kind :name :deprecated?
                       :location (&Location :uri :range (&Range :start
                                                                (&Position :line :character)))))
  "Return a `xref-item' from SYMBOL information."
  (let ((tag-kind (aref lsp--symbol-kinds (- kind 1)))
        (tag-name (if deprecated?
                      (propertize name 'face 'lsp-face-semhl-deprecated)
                    name))
        (tag-line line)
        (tag-column character)
        (tag-path (lsp--uri-to-path uri)))
    (list tag-name
          (make-eproj-tag tag-path
                          tag-line
                          tag-kind
                          (vector (cons 'column tag-column)))
          nil)))

;; sync with `lsp--locations-to-xref-items’
(lsp-defun lsp-symbnav--locations->eproj-tags (identifier locations)
  (setq locations
        (pcase locations
          (`nil
           (error "No matches for %s" identifier))
          ((seq (or (Location)
                    (LocationLink)))
           (append locations nil))
          ((or (Location)
               (LocationLink))
           (list locations))))

  (let ((get-tags-in-file
         (lambda (file-locs)
           (-let (((filename . matches) file-locs))
             (condition-case err
                 (let ((visiting (find-buffer-visiting filename))
                       (fn (lambda (loc)
                             (lsp-with-filename filename
                               (lsp-symbnav--range->eproj-tag filename
                                                              (lsp--location-range loc))))))
                   (if visiting
                       (with-current-buffer visiting
                         (seq-map fn matches))
                     (when (file-readable-p filename)
                       (with-temp-buffer
                         (insert-file-contents-literally filename)
                         (seq-map fn matches)))))
               (error (lsp-warn "Failed to process xref entry for filename '%s': %s"
                                filename (error-message-string err)))
               (file-error (lsp-warn "Failed to process xref entry, file-error, '%s': %s"
                                     filename (error-message-string err))))))))

    (mapcan get-tags-in-file
            (seq-group-by
             (lambda (x) (lsp--uri-to-path (lsp--location-uri x)))
             (seq-sort #'lsp--location-before-p locations)))))

(defface lsp-symbnav-focus
  '((t :underline t))
  "Face to show focused parts."
  :group 'lsp-faces)

(lsp-defun lsp-symbnav--range->eproj-tag
  (filename (&Range :start (start &as &Position :character start-char :line start-line)
                    :end (end &as &Position :character end-char)))
  "Return a xref-item from a RANGE in FILENAME."
  (let* ((line (lsp--extract-line-from-buffer start))
         (len (length line)))
    (add-face-text-property (max (min start-char len) 0)
                            (max (min end-char len) 0)
                            'lsp-symbnav-focus t line)
    (make-eproj-tag filename
                    (lsp-translate-line (1+ start-line))
                    nil
                    (vector (cons 'column (lsp-translate-column start-char))
                            (cons 'summary line)))))

(provide 'lsp-setup)

;; Local Variables:
;; End:

;; lsp-setup.el ends here
