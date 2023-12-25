;; lsp-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created:  7 January 2021
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util)
  (defvar lsp-diagnostics-attributes)
  (defvar lsp-modeline-code-actions-segments)
  (defvar lsp-modeline-diagnostics-scope)
  (defvar lsp-ui-sideline-enable)
  (defvar lsp-ui-sideline-show-code-actions)
  (defvar lsp-ui-sideline-show-diagnostics)
  (defvar lsp-ui-sideline-show-hover))

(require 'common)

(require 'eproj-tag-index)
(require 'lsp)
(require 'lsp-ui-doc)
(require 's)

(setf lsp-client-packages '(lsp-rust lsp-clangd lsp-cmake)

      ;; Disable LSP sessions.
      lsp-session-file nil

      ;; lsp-log-io t
      ;; lsp-print-performance t

      lsp-enable-suggest-server-download nil

      lsp-disabled-clients
      '(ccls lsp-ada lsp-angular lsp-bash lsp-clojure
             lsp-crystal lsp-csharp lsp-css lsp-dart lsp-dhall lsp-dockerfile lsp-elm
             lsp-elixir lsp-erlang lsp-eslint lsp-fortran lsp-fsharp lsp-gdscript lsp-go
             lsp-hack lsp-groovy lsp-haxe lsp-java lsp-javascript lsp-json
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

      lsp-ui-sideline-enable t
      lsp-ui-sideline-show-diagnostics nil
      lsp-ui-sideline-show-hover t
      lsp-ui-sideline-show-code-actions t

      ;; lsp-progress-via-spinner nil

      lsp-ui-doc-enable nil
      lsp-signature-auto-activate nil
      lsp-signature-render-documentation nil
      lsp-eldoc-enable-hover nil

      ;; The ‘message-log-max’ got updated while loading our config so
      ;; propagate updated value here.
      lsp-log-max message-log-max

      ;; ;; For debugging
      ;; lsp-response-timeout 10000

      lsp-diagnostics-attributes
      '((unnecessary :foreground "#666666")
        (deprecated  :strike-through t)))

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

(lsp-defun lsp-doc-other-window--callback ((hover &as &Hover? :contents) buf)
  (unless contents
    (error "Empty response from server. Aborting"))
  (when (equal buf (current-buffer))
    (let ((doc-buf (get-buffer-create "*doc*"))
          (markdown-fontify-code-blocks-natively t)
          (markdown-fontify-code-block-default-mode major-mode))
      (with-current-buffer doc-buf
        (lsp-doc-presentation-mode)
        (awhen lsp-doc-presentation--current-doc
          (push it lsp-doc-presentation--prev-contents))
        (lsp-doc-presentation-set-up-buffer
         (replace-regexp-in-string "\r" "" (lsp-ui-doc--extract contents))))
      (switch-to-buffer-other-window doc-buf))))

;;;; lsp-doc-presentation-mode

(defvar lsp-doc-presentation-mode-map
  (let ((map (make-sparse-keymap)))
    (def-keys-for-map map
      +vi-keys+
      +vim-search-keys+
      +vim-search-extended-keys+
      +vim-word-motion-keys+
      +vim-special-keys+
      ("<up>"   lsp-doc-presentation-go-back)
      ("<down>" lsp-doc-presentation-go-forward)
      ("v"      set-mark-command)
      ("y"      copy-region-as-kill))
    map)
  "Keymap for LSP doc mode, ‘lsp-doc-presentation-mode’.")

(define-derived-mode lsp-doc-presentation-mode special-mode "LSP doc"
  "Major mode to presenting LSP documentation."
  :group 'lsp-mode
  (setq-local truncate-lines t)
  (read-only-mode +1)
  (hl-line-mode +1))

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

;;;; Define some faces so that they’ll look ok

(defface lsp-lsp-flycheck-warning-unnecessary-face
  '((t :foreground "#666666"))
  "Face for text that will evaporate when modified/overwritten."
  :group 'lsp-faces)

;;;; Symbnav

(cl-defun setup-lsp-symbnav (&key (bind-keybindings t) (enable-regexp? t))
  (setq-local xref-show-definitions-function #'eproj-xref-symbnav-show-xrefs
              xref-show-xrefs-function #'eproj-xref-symbnav-show-xrefs)
  (setup-eproj-symbnav :bind-keybindings bind-keybindings)
  (when bind-keybindings
    (awhen (current-local-map)
      (def-keys-for-map it
        ("M-," lsp-symbnav/go-back)
        ("M-?" lsp-symbnav/find-references))
      (if enable-regexp?
          (def-keys-for-map it
            ("M-." #'lsp-symbnav/go-to-symbol-home))
        (def-keys-for-map it
          ("M-." #'lsp-symbnav/go-to-symbol-home-no-regexp))))
    (def-keys-for-map vim-normal-mode-local-keymap
      ("M-." (if enable-regexp?
                 #'lsp-symbnav/go-to-symbol-home
               #'lsp-symbnav/go-to-symbol-home-no-regexp))
      ("M-," lsp-symbnav/go-back)
      ("M-?" lsp-symbnav/find-references))))

(defalias 'lsp-symbnav/go-back #'eproj-symbnav/go-back)

(defun lsp-symbnav--tag-kind (tag)
  (awhen (eproj-tag/type tag)
    (cl-assert (stringp it) nil "Expected string tag type but got: %s" it)
    it))

(defun lsp-symbnav/go-to-symbol-home (&optional use-regexp?)
  (interactive "P")
  (if use-regexp?
      (let* ((re (read-regexp "enter regexp to search for"))
             (lsp-tags
              (-map #'lsp-symbnav--symbol-information->eproj-tag-triple
                    (lsp-request "workspace/symbol" `(:query ,re)))))
        (lsp-symbnav/go-to-symbol-home-impl re lsp-tags))
    (lsp-symbnav/go-to-symbol-home-no-regexp)))

(defun lsp-symbnav/go-to-symbol-home-no-regexp ()
  (interactive)
  (let* ((identifier (eproj-symbnav/identifier-at-point nil))
         (lsp-tags
          (--map (list identifier it nil)
                 (lsp-symbnav--locations->eproj-tags
                  identifier
                  (lsp-request "textDocument/definition"
                               (lsp--text-document-position-params))))))
    (lsp-symbnav/go-to-symbol-home-impl
     identifier
     lsp-tags)))

(defun lsp-symbnav/go-to-symbol-home-impl (ident tags)
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
  (let ((tag-kind (aref lsp-symbol-kinds (- kind 1)))
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
                          (list (cons 'column tag-column)))
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
               (error
                (lsp-warn "Failed to process xref entry for filename '%s': %s"
                          filename (error-message-string err))
                nil)
               (file-error
                (lsp-warn "Failed to process xref entry, file-error, '%s': %s"
                          filename (error-message-string err))
                nil))))))

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
    (add-face-text-property (cap-floor len 0 start-char)
                            (cap-floor len 0 end-char)
                            'lsp-symbnav-focus t line)
    (make-eproj-tag filename
                    (lsp-translate-line (1+ start-line))
                    nil
                    (list (cons 'column (lsp-translate-column start-char))
                          (cons 'summary line)))))


(defhydra hydra-lsp-toggle (:exit nil :foreign-keys nil :hint nil)
  "
Toggle:
_f_ormatting on typing             %`lsp-enable-on-type-formatting
_h_ighlight of symbol at point     %`lsp-enable-symbol-highlighting
_l_ens                             %`lsp-lens-mode
"
  ("f" lsp-toggle-on-type-formatting)
  ("h" lsp-toggle-symbol-highlight)
  ("l" lsp-lens-mode))

(provide 'lsp-setup)

;; Local Variables:
;; End:

;; lsp-setup.el ends here
