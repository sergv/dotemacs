;; dired-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 31 January 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl)
  (require 'cycle-on-lines)
  (require 'macro-util))

(require 'common)
(require 'set-up-paths)
(require 'vim-setup)

(require 'dired-single)
(require 'dired-aux)
(require 'dired-x)
(require 'dired-single)

;;;###autoload
(autoload 'dired-single-buffer "dired-single" "" t)
;;;###autoload
(autoload 'dired-setup "dired-setup")

(setf image-dired-dir (path-concat +prog-data-path+ "image-dired"))

(setf dired-omit-files
      (rx (or (seq bol (? ".") "#") ;; emacs autosave files
              (seq "~" eol)         ;; backup-files
              ))
      dired-omit-extensions
      (--remove (equal ".log" it)
                (append dired-latex-unclean-extensions
                        dired-tex-unclean-extensions
                        dired-bibtex-unclean-extensions
                        dired-texinfo-unclean-extensions)))

(defhydra-ext hydra-dired-sort (:exit t :foreign-keys nil :hint nil)
  "
Sort by:
_n_ame
_s_ize
_t_ime
e_x_tension

_S_: toggle sorting
"
  ("n" dired-ext-sort-by-name)
  ("s" dired-ext-sort-by-size)
  ("t" dired-ext-sort-by-time)
  ("x" dired-ext-sort-by-ext)

  ("S" dired-ext-toggle-sorting :exit nil))

(def-keys-for-map dired-mode-map
  +vim-special-keys+
  +vim-search-keys+
  (("h" "<down>") dired-cycle-files-forward)
  (("t" "<up>")   dired-cycle-files-backward)
  (","            dired-do-delete)
  ("k"            dired-unmark)
  ("K"            dired-unmark-all-marks)
  ("p"            nil)
  ("q"            nil)
  ("e"            dired-do-eval)
  ("f"            nil)
  ("l"            dired-do-symlink)
  ("S"            hydra-dired-sort/body)
  ("o"            dired-do-open-marked)
  ("Q"            dired-do-query-replace-regexp)
  ("<return>"     dired-single-buffer)
  ("SPC"          dired-single-buffer-other-window)
  ("^"            dired-single-up-directory)
  (("H" "<f5>")   revert-buffer) ;; refresh

  ;; ? is already used by dired
  ;; ("?"        search-start-backward)
  )

(defun dired--open ()
  (let ((filename (dired-get-filename)))
    (condition-case err
        (save-window-excursion
          (save-excursion
            (find-file filename)
            nil))
      (error
       (dired-log "Open error for %s:\n%s\n" filename err)
       (dired-make-relative filename)))))

(defun dired-do-open-marked ()
  "Open currently makred files as emacs buffers without switching
to them."
  (interactive)
  (dired-map-over-marks-check #'dired--open
                              nil
                              'open
                              ;; don't redisplay dired after each file
                              nil))

(defun dired-with-marked-files (f if-none-selected)
  "Open each marked file and call F with its buffer."
  (if-let (files (dired-get-marked-files nil nil #'dired-nondirectory-p))
      (dolist (filename files)
        (let ((buf (find-file-noselect filename)))
          (cl-assert (bufferp buf))
          (funcall f buf)))
    (funcall if-none-selected)))

(defun dired-do-eval (expr)
  (interactive (list (read--expression "Eval in marked buffers: ")))
  (dired-with-marked-files
   (lambda (buf)
     (with-current-buffer buf
       (eval expr)))
   (lambda ()
     (error "No files selected"))))

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
   (make-cycle-on-lines-in-region
    2
    -1
    t
    #'dired-next-line
    #'dired-previous-line)
   count))

(defun dired-cycle-files-backward (count)
  "Cycle through file list backward selecting next entry"
  (interactive "p")
  (funcall
   (make-cycle-on-lines-in-region
    2
    -1
    nil
    #'dired-next-line
    #'dired-previous-line)
   count))

(defun dired-single-up-directory ()
  (interactive)
  (dired-single-buffer ".."))

(defun dired-ext-toggle-sorting ()
  (interactive)
  (dired-sort-toggle))

(defun dired-ext-sort-by-name ()
  (interactive)
  (dired-sort-other (concat dired-actual-switches " --sort=name")))

(defun dired-ext-sort-by-size ()
  (interactive)
  (dired-sort-other (concat dired-actual-switches " --sort=size")))

(defun dired-ext-sort-by-time ()
  (interactive)
  (dired-sort-other (concat dired-actual-switches " --sort=time")))

(defun dired-ext-sort-by-ext ()
  (interactive)
  (dired-sort-other (concat dired-actual-switches " --sort=extension")))

;;;###autoload
(defun dired-setup ()
  (hl-line-mode +1)
  (dired-omit-mode +1))

;;;###autoload
(add-hook 'dired-mode-hook #'dired-setup)

(provide 'dired-setup)

;; Local Variables:
;; End:

;; dired-setup.el ends here
