;; typography-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 25 October 2016
;; Description:

(require 'typopunct)

(setq-default typopunct-buffer-language 'english)

;;;###autoload
(defun typography-setup ()
  (typopunct-mode 1))

(provide 'typography-setup)

;; Local Variables:
;; End:

;; typography-setup.el ends here
