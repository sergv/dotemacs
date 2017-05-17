;; haskell-format-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  2 May 2017
;; Description:

(defparameter haskell-format-brittany-executable
  (executable-find "brittany"))

;;;###autoload
(defun haskell-format-pp-region-with-brittany (width)
  "Format selected region using brittany haskell formatter."
  (interactive "p")
  (haskell-format--format-with-brittany
   2
   width
   (region-beginning)
   (region-end)))

(defun haskell-format--format-with-brittany (indent width start end)
  "Format region using brittany haskell formatter."
  (unless (integerp indent)
    (error "Width must be an integer: %s" indent))
  (unless (integerp width)
    (error "Width must be an integer: %s" width))
  (goto-char start)
  (call-process-region
   start
   end
   haskell-format-brittany-executable
   t ;; delete
   t ;; insert into current buffer before point
   t ;; display
   "--indent" (format "%s" indent)
   "--columns" (format "%s" width)))

(provide 'haskell-format-setup)

;; Local Variables:
;; End:

;; haskell-format-setup.el ends here
