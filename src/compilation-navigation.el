;; compilation-navigation.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 25 November 2019
;; Description:

(eval-when-compile
  (require 'macro-util)
  (require 'trie)

  (defvar hl-line-mode))

(require 'common)
(require 'compile)
(require 'configurable-compilation)
(require 'flycheck-setup)

(cl-defstruct (compilation-error
               (:conc-name compilation-error/))
  compilation-root-directory
  compilation-eproj-root
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
                              :compilation-eproj-root (awhen configurable-compilation--command
                                                        (cc-command/eproj-root it))
                              :filename file
                              :line-number line
                              :column-number col))))

(defun compilation/find-all-buffers (filename compilation-root eproj-root)
  "Get all buffers that corresponds to FILENAME within current project and/or current root that
we could reasonably identify.

Returns cons pair of opened buffers that we found and filename, if any, resolved under COMPILATION-ROOT."
  (cl-assert (and filename
                  (not (zerop (length filename)))))
  (cl-assert (if compilation-root (file-name-absolute-p compilation-root) t))
  (cl-assert (if eproj-root (file-name-absolute-p eproj-root) t))
  (let* ((compilation-root-norm (and compilation-root
                                     (normalise-file-name (expand-file-name compilation-root))))
         (eproj-root-norm (and eproj-root
                               (normalise-file-name (expand-file-name eproj-root))))
         (proj (eproj-get-project-for-path-exact-lax eproj-root))
         (prepare-filename-for-comparisons (fold-platform-os-type #'identity
                                                                  #'downcase))
         (ignore-case? (fold-platform-os-type nil t))
         (related-roots (when proj
                          (trie-from-list
                           (--map (cons (funcall prepare-filename-for-comparisons (eproj-project/root it)) t)
                                  (eproj-get-non-default-related-projects proj)))))
         (candidates
          (append
           (-filter (lambda (buf)
                      (awhen (buffer-file-name buf)
                        (let ((buf-file (normalise-file-name it)))
                          (and (string-suffix-p filename buf-file)
                               (or (and compilation-root-norm (string-prefix-p compilation-root-norm buf-file ignore-case?))
                                   (and eproj-root-norm (string-prefix-p eproj-root-norm buf-file ignore-case?))
                                   (and related-roots
                                        (trie-matches-string-prefix? related-roots
                                                                     (funcall prepare-filename-for-comparisons buf-file))))))))
                    (visible-buffers))
           (when proj
             (let ((root (eproj-project/root proj))
                   (eproj-candidates nil))
               (eproj-with-all-project-files-for-navigation proj
                                                            (lambda (rel-path)
                                                              (when (string-suffix-p filename rel-path (fold-platform-os-type nil t))
                                                                (push (concat root "/" rel-path) eproj-candidates))))
               eproj-candidates)
             ))))
    (cons
     candidates
     (cl-block done
       ;; If filename did not resolve in immediate root then try all the parents,
       ;; perhaps compilation was actually executed/reported its errors from the
       ;; directory above?
       (dolist (parent (file-name-all-parents compilation-root))
         (let ((resolved-filename (resolve-to-abs-path-lax filename parent)))
           (when (and resolved-filename
                      (file-exists-p resolved-filename))
             (cl-return-from done
               (aif (get-file-buffer resolved-filename)
                   it
                 (find-file-noselect resolved-filename))))))))))

(defun compilation/find-buffer (filename compilation-root eproj-root)
  "Get buffer that corresponds to FILENAME within current project and/or current root.

The FILENAME may be neither full nor relative path. In case it’s
neither, a buffer visiting filename with suffix equal to FILENAME will
searched for within specified root and/or project.

If EPROJ-ROOT resolves to an existing eproj project then FILENAME will be
searched among its files as a last resort measure."
  (let ((candidates (compilation/find-all-buffers filename compilation-root eproj-root)))
    (aif (car candidates)
        (let ((first-candidate (car it)))
          (cond
            ((bufferp first-candidate)
             first-candidate)
            ((stringp first-candidate)
             (find-file-noselect first-candidate))
            (t
             (error "‘compilation/find-all-buffers’ returned not a buffer or string: ‘%s’"
                    first-candidate))))
      (cdr candidates))))

(defun compilation/jump-to-error (err &optional other-window)
  "Jump to source of compilation error. ERR should be structure describing
error location - value of compilation-error structure."
  (cl-assert (compilation-error-p err))
  (aif (compilation/find-buffer
        (compilation-error/filename err)
        (compilation-error/compilation-root-directory err)
        (compilation-error/compilation-eproj-root err))
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
    (compilation/jump-to-error
     err
     nil)))

(defun compilation/goto-error-other-window ()
  "Jump to location of error or warning (file, line and column) in other window."
  (interactive)
  (when-let (err (compilation--error-at-point))
    (compilation/jump-to-error
     err
     t)))

(defun compilation--get-compilation-buffer-proj-root (comp-buf)
  "Return eproj project root for compilation buffer"
  (cl-assert (provided-mode-derived-p (buffer-local-value 'major-mode comp-buf) '(compilation-mode)))
  (buffer-local-value 'default-directory comp-buf))

(defun compilation-navigation--use-selected-error-or-jump-to-next (win comp-buf jump-to-next-err-func)
  "Either return error currently selected in the compilation buffer BUF, if
point is not located on it, or return the next error if current position argees
with the position of the selected error."
  (let ((curr-buf (current-buffer))
        (line (line-number-at-pos)))
    (with-selected-window win
      (with-current-buffer comp-buf
        (prog1
            (if-let ((selected-err (compilation--error-at-point)))
                (if (and
                     (eq (compilation/find-buffer
                          (compilation-error/filename selected-err)
                          (compilation-error/compilation-root-directory err)
                          (compilation-error/compilation-eproj-root selected-err))
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
                (compilation/jump-to-error err nil)
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
              (compilation/jump-to-error err nil))
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
