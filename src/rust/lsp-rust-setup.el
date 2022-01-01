;; lsp-rust-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 19 December 2021
;; Description:

(eval-when-compile
  (require 'set-up-platform)

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
  (defvar lsp-rust-target-dir))

(require 'lsp-setup)

(setf lsp-rust-server 'rust-analyzer
      ;; lsp-rust-analyzer-server-command '("/tmp/target/release/rust-analyzer")
      lsp-rust-analyzer-server-command '("rust-analyzer")
      lsp-rust-analyzer-server-args '(nil "--log-file" "/tmp/rust-analyzer-log.txt")

      lsp-rust-crate-blacklist []
      lsp-rust-racer-completion nil
      lsp-rust-target-dir (fold-platform-os-type "/tmp/target/rls" nil)
      lsp-rust-full-docs t

      lsp-rust-analyzer-proc-macro-enable t
      lsp-rust-analyzer-cargo-run-build-scripts t

      lsp-rust-analyzer-server-display-inlay-hints t
      lsp-rust-analyzer-display-parameter-hints t
      lsp-rust-analyzer-display-chaining-hints t)

(defvar-local lsp-rust--has-experimental-hover-range-cap +undef+)

(defun lsp-rust-type-of-thing-at-point ()
  "Display the type of the thing at point. With universal
argument jump to type definition."
  (interactive)
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

(provide 'lsp-rust-setup)

;; Local Variables:
;; End:

;; lsp-rust-setup.el ends here
