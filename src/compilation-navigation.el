;; compilation-navigation.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 25 November 2019
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'common)
(require 'compile)
(require 'configurable-compilation)
(require 'flycheck-setup)

(cl-defstruct (compilation-error
               (:conc-name compilation-error/))
  compilation-root-directory
  filename
  line-number
  column-number)

;;;###autoload
(defun compilation--error-at-point ()
  (awhen (plist-get (text-properties-at (point)) 'compilation-message)
    (let* ((loc (compilation--message->loc it))
           (file (caar (compilation--loc->file-struct loc)))
           (line (compilation--loc->line loc))
           (col (awhen (compilation--loc->col loc) (1- it))))
      (make-compilation-error :compilation-root-directory default-directory
                              :filename file
                              :line-number line
                              :column-number col))))

(defun compilation/find-buffer (filename &optional root proj-dir)
  "Get buffer that corresponds to FILENAME, which may be neither full nor
relative path. In case it's neither, a buffer visiting filename
with suffix equal to FILENAME will searched for."
  (cl-assert (not (= 0 (length filename))))
  (let ((root-norm (and root
                        (normalise-file-name root)))
        (proj-dir-norm (and proj-dir
                            (normalise-file-name proj-dir))))
    (aif (-find (lambda (buf)
                  (awhen (buffer-file-name buf)
                    (let ((buf-file (normalise-file-name it)))
                      (and (string-suffix-p filename buf-file)
                           (or (and root-norm (string-prefix-p root-norm buf-file))
                               (and proj-dir-norm (string-prefix-p proj-dir-norm buf-file)))))))
                (visible-buffers))
        it
      (cl-block done
        ;; If filename did not resolve in immediate root then try all the parents,
        ;; perhaps compilation was actually executed/reported its errors from the
        ;; directory above?
        (dolist (parent (file-name-all-parents root))
          (let ((resolved-filename (resolve-to-abs-path-lax filename parent)))
            (when (and resolved-filename
                       (file-exists-p resolved-filename))
              (cl-return-from done
                (aif (get-file-buffer resolved-filename)
                    it
                  (find-file-noselect resolved-filename))))))))))

(defun compilation/jump-to-error (err &optional other-window proj-dir)
  "Jump to source of compilation error. ERR should be structure describing
error location - value of compilation-error structure."
  (cl-assert (compilation-error-p err))
  (aif (compilation/find-buffer
        (compilation-error/filename err)
        (compilation-error/compilation-root-directory err)
        proj-dir)
    (funcall (if other-window
               #'switch-to-buffer-other-window
               #'switch-to-buffer)
             it)
    (error "Could not find buffer for file %s" (compilation-error/filename err)))
  (vim-save-position)
  (goto-line-dumb (compilation-error/line-number err))
  (awhen (compilation-error/column-number err)
    (move-to-character-column it)))

(defun compilation/goto-error ()
  "Jump to location of error or warning (file, line and column) in current window."
  (interactive)
  (when-let (err (compilation--error-at-point))
    (compilation/jump-to-error err nil nil)))

(defun compilation/goto-error-other-window ()
  "Jump to location of error or warning (file, line and column) in other window."
  (interactive)
  (when-let (err (compilation--error-at-point))
    (compilation/jump-to-error err t nil)))

(defun compilation-navigation--use-selected-error-or-jump-to-next (win buf jump-to-next-err-func)
  "Either return error currently selected in the compilation buffer BUF, if
point is not located on it, or return the next error if current position argees
with the position of the selected error."
  (let ((curr-buf (current-buffer))
        (line (line-number-at-pos)))
    (with-selected-window win
      (with-current-buffer buf
        (prog1
            (if-let ((selected-err (compilation--error-at-point)))
                (if (and
                     (eq (compilation/find-buffer
                          (compilation-error/filename selected-err)
                          (compilation-error/compilation-root-directory selected-err))
                         curr-buf)
                     (equal (compilation-error/line-number selected-err)
                            line))
                    ;; If we're already on the selected error then jump to next error.
                    (progn
                      (funcall jump-to-next-err-func)
                      (compilation--error-at-point))
                  selected-err)
              (progn
                (funcall jump-to-next-err-func)
                (compilation--error-at-point)))
          (when hl-line-mode
            (hl-line-highlight)))))))

(defun compilation-navigation--go-navigate-errors (comp-buf jump-to-next-err-func fallback)
  "Navigate errors in compilation buffer BUF."
  (cl-assert (bufferp comp-buf))
  (if (buffer-live-p comp-buf)
      (let ((win (get-buffer-window comp-buf
                                    t ;; all-frames
                                    )))
        (if (and win
                 (window-live-p win))
            (if-let (err (compilation-navigation--use-selected-error-or-jump-to-next
                          win
                          comp-buf
                          jump-to-next-err-func))
                (compilation/jump-to-error err nil nil)
              (funcall fallback))
          (funcall fallback)))
    (funcall fallback)))

;;;###autoload
(defun compilation-navigation-next-error-in-buffer-other-window (buf)
  "Select next error in compilation buffer BUF and jump to
it's position in current window."
  (compilation-navigation--go-navigate-errors
   buf
   #'compilation-jump-to-next-error
   #'flycheck-enhancements-next-error-with-wraparound))

;;;###autoload
(defun compilation-navigation-prev-error-in-buffer-other-window (buf)
  "Select previous error in compilation buffer BUF and jump to
it's position in current window."
  (compilation-navigation--go-navigate-errors
   buf
   #'compilation-jump-to-prev-error
   #'flycheck-enhancements-previous-error-with-wraparound))

(defun compilation-navigation-next-or-prev-error-other-window (is-next?)
  "Select another error in suitable compilation buffer (resolved
via ‘configurable-compilation-buffer-name’ and
‘configurable-compilation-proj-dir’) and jump to it's position in
current window."
  (let* ((proj-dir (configurable-compilation-proj-dir))
         (bufname (configurable-compilation-buffer-name proj-dir)))
    (if-let ((buf (get-buffer bufname)))
        (if-let ((win (get-buffer-window buf t)))
            (let ((err (with-selected-window win
                         (with-current-buffer buf
                           (if is-next?
                               (compilation-jump-to-next-error)
                             (compilation-jump-to-prev-error))
                           (when hl-line-mode
                             (hl-line-highlight))
                           (compilation--error-at-point)))))
              (compilation/jump-to-error err nil proj-dir))
          (error "Compilation buffer for current project is not visible: %s" bufname))
      (error "No compilation buffer: %s" bufname))))

;;;###autoload
(defun compilation-navigation-next-error-other-window ()
  "Select next error in suitable compilation buffer and jump to
it's position in current window."
  (interactive)
  (compilation-navigation-next-or-prev-error-other-window t))

;;;###autoload
(defun compilation-navigation-prev-error-other-window ()
  "Select previous error in suitable compilation buffer and jump to
it's position in current window."
  (interactive)
  (compilation-navigation-next-or-prev-error-other-window nil))

;; Local Variables:
;; End:

(provide 'compilation-navigation)

;; compilation-navigation.el ends here
