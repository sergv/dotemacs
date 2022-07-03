;; typography-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 25 October 2016
;; Description:

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

(defun typography-insert-vanilla-dash (n)
  (interactive "P")
  (insert-char ?- n))

(defun typography-insert-vanilla-quotation-mark (n)
  (interactive "P")
  (insert-char ?\" n))

(defun typography-insert-vanilla-single-quotation-mark (n)
  (interactive "P")
  (insert-char ?\' n))

;;;###autoload
(defun typography-setup ()
  (typopunct-mode 1)
  (electric-quote-local-mode 1)

  (when vim-mode
    (def-keys-for-map vim-insert-mode-local-keymap
      ("C--"  typography-insert-vanilla-dash)
      ("C-\"" typography-insert-vanilla-quotation-mark)
      ("C-\'" typography-insert-vanilla-single-quotation-mark))))

;; Local Variables:
;; End:

;; typography-setup.el ends here
