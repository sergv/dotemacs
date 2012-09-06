;;; dired-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 31 January 2012
;; Keywords:
;; Requirements:
;; Status:


(eval-after-load
 "dired"
 '(progn
   (require 'dired-single)
   (require 'dired-aux)
   (require 'dired-x)

   (setf dired-omit-files
         (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
                 (seq "~" eol)                 ;; backup-files
                 ))
         dired-omit-extensions
         (append dired-latex-unclean-extensions
                 dired-tex-unclean-extensions
                 dired-bibtex-unclean-extensions
                 dired-texinfo-unclean-extensions))
   (add-hook 'dired-mode-hook
    (lambda ()
      (dired-omit-mode 1)))

   (def-keys-for-map dired-mode-map
     +control-x-prefix+
     +vim-special-keys+
     ("t"        dired-cycle-files-forward)
     ("n"        dired-cycle-files-backward)
     ("<down>"   dired-cycle-files-forward)
     ("<up>"     dired-cycle-files-backward)
     ("p"        nil)
     ("q"        nil)
     ("e"        dired-do-open-marked)
     ("f"        dired-do-open-marked)
     ("o"        dired-do-open-marked)
     ("Q"        dired-prompt-and-do-query-replace-regexp)
     ("<return>" dired-single-buffer)
     ("^"        dired-single-up-directory)
     ("r"        revert-buffer) ;; refresh

     ("/"        search-start-forward)
     ;; ? is already used by dired
     ;; ("?"        search-start-backward)
     )


   (defun dired--open ()
     (let ((filename (dired-get-filename)) failure)
       (condition-case err
           (save-window-excursion
            (save-excursion
             (find-file filename)))
         (error (setq failure err)))
       (if (not failure)
         nil
         (progn
           (dired-log "Open error for %s:\n%s\n" filename failure)
           (dired-make-relative filename)))))

   (defun dired-do-open-marked ()
     (interactive)
     (dired-map-over-marks-check #'dired--open
                                 nil
                                 'open
                                 ;; don't redisplay dired after each file
                                 nil))))


(provide 'dired-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; dired-setup.el ends here
