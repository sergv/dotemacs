;;; hl-tags-mode.el --- Highlight the current SGML tag context --- -*- lexical-binding: t; -*-

;; Copyright (c) 2011 Mike Spindel <deactivated@gmail.com>
;; Modified by Amit J Patel <amitp@cs.stanford.edu> for nxml-mode
;; Further modified by Sergey Vinokurov <serg.foo@gmail.com> (fiddling with context)

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; hl-tags-mode is a minor mode for SGML and XML editing that
;; highlights the current start and end tag.
;;
;; To use hl-tags-mode, add the following to your .emacs:
;;
;;   (require 'hl-tags-mode)
;;   (add-hook 'sgml-mode-hook (lambda () (hl-tags-mode 1)))
;;   (add-hook 'nxml-mode-hook (lambda () (hl-tags-mode 1)))

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'common)

(defvar-local hl-tags-start-overlay nil)
(defvar-local hl-tags-end-overlay nil)


(defun hl-tags-sgml-get-context ()
  (save-excursion (car (last (sgml-get-context)))))

(defun hl-tags-sgml-pair (ctx)
  (if ctx
      (cons (sgml-tag-start ctx) (sgml-tag-end ctx))
    '(1 . 1)))

(defun hl-tags-context-sgml-mode ()
  (save-excursion
    (when (looking-at-p "<") (forward-char 1))
    (let* ((ctx (hl-tags-sgml-get-context))
           (boundaries
            (and ctx (cl-case (sgml-tag-type ctx)
                       (empty (cons ctx nil))
                       (close
                        (goto-char (sgml-tag-start ctx))
                        (cons (hl-tags-sgml-get-context) ctx))
                       (open
                        (goto-char (sgml-tag-start ctx))
                        (sgml-skip-tag-forward 1)
                        (backward-char 1)
                        (cons ctx (hl-tags-sgml-get-context)))))))
      (when boundaries
        (cons (hl-tags-sgml-pair (car boundaries))
              (hl-tags-sgml-pair (cdr boundaries)))))))

;;;###autoload
(defun hl-tags-context-nxml-mode ()
  (ignore-errors

    (when (or (not (boundp 'nxml-prolog-end))
              (not nxml-prolog-end))
      (nxml-scan-prolog))

    (save-excursion
      (let (start1
            end1
            start2
            end2

            xmltok-type
            xmltok-start
            xmltok-name-end
            xmltok-name-colon
            xmltok-attributes
            xmltok-namespace-attributes)

        (when (eq (char-after) ?<) (forward-char))
        (nxml-up-element 1)
        (setq end2 (point))

        (nxml-token-before)
        (setq start2 xmltok-start)

        (nxml-backward-single-balanced-item)

        (setq start1 (point))
        (setq end1 (nxml-token-after))

        (cons (cons start1 end1) (cons start2 end2))))))

(defsubst hl-tags-context ()
  "Return a pair ((start . end) . (start . end)) containing the
boundaries of the current start and end tag , or nil."
  (funcall *markup-tags-context-func*))

(defun hl-tags-update ()
  (ignore-errors
    (let ((ctx (hl-tags-context)))
      (if (null ctx)
          (hl-tags-hide)
        (progn
          (hl-tags-show)
          (move-overlay hl-tags-start-overlay (caar ctx) (cdar ctx))
          (move-overlay hl-tags-end-overlay (cadr ctx) (cddr ctx)))))))

(defun hl-tags-show ()
  (unless hl-tags-start-overlay
    (setq hl-tags-start-overlay (make-overlay 1 1)
          hl-tags-end-overlay (make-overlay 1 1))
    (overlay-put hl-tags-start-overlay
                 'face 'show-paren-match-face)
    (overlay-put hl-tags-end-overlay
                 'face 'show-paren-match-face)))

(defun hl-tags-hide ()
  (when hl-tags-start-overlay
    (delete-overlay hl-tags-start-overlay)
    (delete-overlay hl-tags-end-overlay)))

;;;###autoload
(define-minor-mode hl-tags-mode
  "Toggle hl-tags-mode."
  :init-value nil
  :lighter ""
  :keymap nil
  (if hl-tags-mode
      (progn
        (add-hook 'post-command-hook 'hl-tags-update nil t)
        (add-hook 'change-major-mode-hook 'hl-tags-hide nil t))
    (progn
      (remove-hook 'post-command-hook 'hl-tags-update t)
      (remove-hook 'change-major-mode-hook 'hl-tags-hide t)
      (hl-tags-hide))))


(provide 'hl-tags-mode)
;;; hl-tags-mode.el --- ends here

