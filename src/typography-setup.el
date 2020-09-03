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

;;;###autoload
(defun typography-setup ()
  (typopunct-mode 1)
  (electric-quote-local-mode 1))

;; Local Variables:
;; End:

;; typography-setup.el ends here
