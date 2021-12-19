;; lsp-rust-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 19 December 2021
;; Description:

(eval-when-compile
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

;;;; lsp-rust-doc-presentation-mode

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

(define-derived-mode lsp-rust-doc-presentation-mode special-mode "Rust doc"
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
        (lsp-rust-doc-presentation-mode)
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

(provide 'lsp-rust-setup)

;; Local Variables:
;; End:

;; lsp-rust-setup.el ends here
