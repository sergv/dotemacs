;; vim-ex-commands.el - Implementation of some ex-mode commands. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'vim-ex)

(cl-defun vim--save-buffer (file-name &key begin end force append)
  "Saves the lines from `begin' to `end' to file `file-name'."
  (with-current-buffer vim-ex--current-buffer
    (when (null file-name)
      (setq file-name buffer-file-name)
      (unless file-name
        (error "Please specify a file-name for this buffer!")))

    (let (beg-pos end-pos)
      (when begin
        (setq beg-pos (save-excursion
                        (goto-line-dumb begin)
                        (line-beginning-position)))
        (setq end-pos (if end
                          (save-excursion
                            (goto-line-dumb end)
                            (line-end-position))
                        beg-pos)))

      (cond
        ((and (null beg-pos)
              (string= file-name buffer-file-name))
         (indirect-aware-save-buffer))
        ((and (null beg-pos)
              (null buffer-file-name))
         (write-file file-name (not force)))
        (t
         (write-region beg-pos end-pos file-name append nil nil (not force)))))))

(vim-defcmd vim:cmd-quit (force nonrepeatable noninteractive)
  "Closes the current window, exits Emacs if this is the last window."
  (condition-case nil
      (delete-window)
    (error
     (condition-case nil
         (delete-frame)
       (error
        (if force
            (kill-emacs)
          (save-buffers-kill-emacs)))))))

(defun vim--ex-complete-mode-argument (mode predicate flag)
  "Completes a registered vim-mode submode."
  (when mode
    (let ((modes (-map #'cdr vim--mode-alist)))
      (with-current-buffer vim-ex--current-buffer
        (pcase flag
          (`nil    (try-completion mode modes predicate))
          (`t      (all-completions mode modes predicate))
          (`lambda (test-completion mode modes predicate)))))))

(vim--define-arg-handler 'mode
  :complete 'vim--ex-complete-mode-argument)

(provide 'vim-ex-commands)

;; Local Variables:
;; End:

;; vim-ex-commands.el ends here
