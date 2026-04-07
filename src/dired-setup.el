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
  (require 'el-patch)
  (require 'macro-util))

(require 'common)
(require 'el-patch)
(require 'set-up-paths)
(require 'vim-setup)

(require 'dired-aux)
(require 'dired-single)
(require 'dired-x)

;;;###autoload
(autoload 'dired-single-buffer "dired-single" "" t)
;;;###autoload
(autoload 'dired-setup "dired-setup")

(setf image-dired-dir (path-concat +prog-data-path+ "image-dired"))

(setf dired-omit-files
      (rx (or (seq bol (? ".") "#") ;; emacs autosave files
              (seq "~" eol)         ;; backup-files
              ))
      dired-omit-verbose nil
      dired-omit-extensions
      (--remove (equal ".log" it)
                (append dired-latex-unclean-extensions
                        dired-tex-unclean-extensions
                        dired-bibtex-unclean-extensions
                        dired-texinfo-unclean-extensions))
      dired-movement-style 'cycle-files)

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
  (("h" "<down>") dired-next-line)
  (("t" "<up>")   dired-previous-line)
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

(el-patch-feature 'dired)

(el-patch-defun dired--move-to-next-line (arg jumpfun)
  (let ((wrapped nil)
        (old-arg arg)
        (old-position (progn
                        ;; It's always true that we should move
                        ;; to the filename when possible.
                        (dired-move-to-filename)
                        (point)))
        ;; Up/Down indicates the direction.
        (moving-down (if (cl-plusp arg)
                         1              ; means Down.
                       -1)))            ; means Up.
    ;; Line by line in case we forget to skip empty lines.
    (while (not (zerop arg))
      (funcall jumpfun moving-down)
      (el-patch-add
        (when-let* ((bounds (bounds-of-thing-at-point 'filename)))
          (when (string= "."
                         (buffer-substring-no-properties (car bounds) (cdr bounds)))
            (funcall jumpfun moving-down))))
      (when (= old-position (point))
        ;; Now point is at beginning/end of movable area,
        ;; but it still wants to move farther.
        (cond
          ;; `cycle': go to the other end.
          ((memq dired-movement-style '(cycle cycle-files))
           ;; Argument not changing on the second wrap
           ;; means infinite loop with no files found.
           (if (and wrapped (eq old-arg arg))
               (setq arg 0)
             (goto-char (if (cl-plusp moving-down)
                            (point-min)
                          (point-max))))
           (setq wrapped t))
          ;; `bounded': go back to the last non-empty line.
          (dired-movement-style ; Either 'bounded or anything else non-nil.
           (while (and (dired-between-files)
                       (or (eq dired-movement-style 'bounded-files)
                           (not (dired-get-subdir)))
                       (not (zerop arg)))
             (funcall jumpfun (- moving-down))
             ;; Point not moving means infinite loop.
             (if (= old-position (point))
                 (setq arg 0)
               (setq old-position (point))))
           ;; Encountered a boundary, so let's stop movement.
           (setq arg (if (and (dired-between-files)
                              (or (eq dired-movement-style 'bounded-files)
                                  (not (dired-get-subdir))))
                         0 moving-down)))))
      (unless (and (dired-between-files)
                   (or (memq dired-movement-style '(cycle-files bounded-files))
                       (not (dired-get-subdir))))
        ;; Has moved to a non-empty line.  This movement does
        ;; make sense.
        (cl-decf arg moving-down))
      (setq old-position (point)))))

(provide 'dired-setup)

;; Local Variables:
;; End:

;; dired-setup.el ends here
