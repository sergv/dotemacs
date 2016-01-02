;; dired-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 31 January 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile (require 'cl-lib))

(require 'set-up-paths)
(require 'custom)

(require 'dired-single)
(require 'dired-aux)
(require 'dired-x)

(require 'dired-single)
(setf image-dired-dir (path-concat +prog-data-path+ "image-dired"))

(setf dired-omit-files
      (rx (or (seq bol (? ".") "#") ;; emacs autosave files
              (seq "~" eol)         ;; backup-files
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
  +vim-special-keys+
  +vim-search-keys+
  ("h"        dired-cycle-files-forward)
  ("t"        dired-cycle-files-backward)
  ("<down>"   dired-cycle-files-forward)
  ("<up>"     dired-cycle-files-backward)
  ("k"        dired-unmark)
  ("K"        dired-unmark-all-marks)
  ("p"        nil)
  ("q"        nil)
  ("e"        nil)
  ("f"        nil)
  ("o"        dired-do-open-marked)
  ("Q"        dired-prompt-and-do-query-replace-regexp)
  ("<return>" dired-single-buffer)
  ("SPC"      dired-single-buffer-other-window)
  ("^"        dired-single-up-directory)
  ("r"        revert-buffer) ;; refresh

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
  "Just open currently makred files as emacs buffers without switching to
them."
  (interactive)
  (dired-map-over-marks-check #'dired--open
                              nil
                              'open
                              ;; don't redisplay dired after each file
                              nil))

(defun dired-single-buffer-other-window (&optional file-to-visit)
  "Similar to `dired-single-buffer' but opens file window that the
current one."
  (interactive)
  (switch-to-buffer-other-window
   (find-file-noselect (or file-to-visit
                           (dired-get-file-for-visit)))))

(defun dired-cycle-files-forward (count)
  "Cycle through file list forward selecting next entry"
  (interactive "p")
  (funcall
   (make-cycle-on-lines-in-region 2 -1 forward
                                  #'dired-next-line #'dired-previous-line)
   count))

(defun dired-cycle-files-backward (count)
  "Cycle through file list backward selecting next entry"
  (interactive "p")
  (funcall
   (make-cycle-on-lines-in-region 2 -1 backward
                                  #'dired-next-line #'dired-previous-line)
   count))

(provide 'dired-setup)

;; Local Variables:
;; End:

;; dired-setup.el ends here
