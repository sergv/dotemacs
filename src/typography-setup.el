;; typography-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 25 October 2016
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'electric)
(require 'typopunct)

(provide 'typography-setup)

;;;###autoload
(eval-after-load "typopunct"
  '(progn
     (require 'typography-setup)))

(setq-default typopunct-buffer-language 'english)

(setf electric-quote-replace-double t
      electric-quote-comment t
      electric-quote-string nil
      electric-quote-context-sensitive t)

(defun typography-insert-vanilla-dash (&optional n)
  (interactive "*P")
  (insert-char ?- n))

(defun typography-insert-vanilla-quotation-mark (&optional n quoted?)
  (interactive "*p")
  (if quoted?
      (dotimes (_ (or n 1))
        (insert-char ?\\)
        (insert-char ?\"))
    (insert-char ?\" n)))

(defun typography-insert-vanilla-single-quotation-mark (&optional n)
  (interactive "*p")
  (insert-char ?\' n))

(defvar-local typography-setup-enable-typographic-quotes? t)

(defun typography-smart-insert-double-quote (&optional n quoted?)
  (interactive "*p")
  (if typography-setup-enable-typographic-quotes?
      (typopunct-insert-quotation-mark nil)
    (typography-insert-vanilla-quotation-mark n quoted?)))

(defun typography-smart-insert-single-quote (&optional n)
  (interactive "*p")
  (if typography-setup-enable-typographic-quotes?
      (typopunct-insert-single-quotation-mark)
    (typography-insert-vanilla-single-quotation-mark n)))

(defun typography-smart-insert-double-quote-inverted (&optional n quoted?)
  (interactive "*p")
  (if (or typopunct-mode
          electric-quote-mode)
      (typography-insert-vanilla-quotation-mark n quoted?)
    (typopunct-insert-quotation-mark nil)))

(defun typography-smart-insert-single-quote-inverted (&optional n)
  (interactive "*p")
  (if (or typopunct-mode
          electric-quote-mode)
      (typography-insert-vanilla-single-quotation-mark n)
    (typopunct-insert-single-quotation-mark)))

;;;###autoload
(cl-defun typography-setup (&key (bind-keys t))
  (typopunct-mode 1)
  (electric-quote-local-mode 1)

  (when (and vim-mode
             bind-keys)
    (def-keys-for-map vim-insert-mode-local-keymap
      ("C--"  typography-insert-vanilla-dash)
      ("C-\"" typography-insert-vanilla-quotation-mark)
      ("C-\'" typography-insert-vanilla-single-quotation-mark))))

;; Local Variables:
;; End:

;; typography-setup.el ends here
