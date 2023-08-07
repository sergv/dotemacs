;; backups.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 28 August 2011
;; Keywords:
;; Requirements:
;; Status:

;; Backup files on saves every hour or so and obligatory
;; backup on emacs exit

(eval-when-compile
  (require 'cl)

  (require 'set-up-platform)
  (require 'macro-util))

(require 'common)
(require 'set-up-paths)

(defconst b/backup-directory (path-concat +prog-data-path+ "backup"))

(defvar b/backup-interval 1800
  "Time interval in seconds to make backups on save.")

(defvar b/last-backup-time nil
  "Time of last backup made by `run-backup-if-has-unsaved-changes'.")
(make-variable-buffer-local 'b/last-backup-time)
(set-default 'b/last-backup-time nil)

(defvar b/has-unbacked-up-changes nil
  "Becomes true whenever you perform save that has no corresponding backup.")
(make-variable-buffer-local 'b/has-unbacked-up-changes)
(set-default 'b/has-unbacked-up-changes nil)

(defvar backups--ignore-buffer nil
  "Buffers with non-nil value of this variable will not be backed up.")
(make-variable-buffer-local 'backups--ignore-buffer)
(set-default 'backups--ignore-buffer nil)


(defsubst b/get-time ()
  "Return current time as a number of seconds since epoch."
  (let ((x (current-time)))
    (+ (* 65536 (car x)) (nth 1 x))))

(defsubst b/shorten-path-from-root (x)
  (replace-regexp-in-string "^[^/]*/" "" x))

(defun b/make-backup-name (abs-filename)
  "Make backup filename from absolute filename"
  (unless (file-name-absolute-p abs-filename)
    (error "b/make-backup-name: error: absolute filename expected"))
  (let* ((file (file-name-nondirectory abs-filename))
         (time (format-time-string "%Y-%m-%d %H-%M"))
         (half-name (concat file " " time " "))
         (extension ".bak")
         ;; we have to deal with utf8 strings and limit of
         ;; 256 bytes (on ext4) of filename length, so
         ;; this string should be truncated from the left
         (size (- 255 (+ (string-bytes half-name) (length extension))))
         (directory (replace-regexp-in-string
                     "/$"
                     ""
                     (file-name-directory abs-filename))))
    (while (< size (string-bytes directory))
      (setq directory (b/shorten-path-from-root directory)))
    (concat half-name
            (fold-platform-os-type
             (replace-regexp-in-string "/" "%" directory)
             nil)
            extension)))

(defun backups--perform-backup (buf)
  "Make backup of specified buffer or current buffer if BUF is nil."
  (with-current-buffer buf
    (when-buffer-has-file
      ;; Ensure that destination directory exists.
      (unless (file-exists-p b/backup-directory)
        (make-directory b/backup-directory t))
      (let* ((file buffer-file-name)
             (dest (path-concat b/backup-directory
                                (b/make-backup-name file))))
        (when (file-exists? file)
          (copy-file file dest t t t)
          ;; (if (file-exists-p dest)
          ;; (error "make-backup: fatal error: backup file %s already exists"
          ;; dest)
          ;; (copy-file file dest t t t))
          )))))

(defun backups--should-backup-buffer? (buf)
  (and (not (buffer-local-value 'backups--ignore-buffer buf))
       (if (boundp 'recentf-save-file)
           (not (string= (buffer-file-name buf)
                         recentf-save-file))
         t)))

(defun backups-ignore-current-buffer! ()
  (setq-local backups--ignore-buffer t))

(defun run-backup-if-has-unsaved-changes ()
  "Run `backups--perform-backup' on save at time intervals specified
by `b/backup-interval'."
  (let ((buf (current-buffer)))
    (when (backups--should-backup-buffer? buf)
      (if (or (not b/last-backup-time)
              (< b/backup-interval (- (b/get-time) b/last-backup-time)))
          (progn
            (backups--perform-backup buf)
            (setq b/last-backup-time (b/get-time)
                  b/has-unbacked-up-changes nil))
        (setq b/has-unbacked-up-changes t)))))

(defun backup-on-buffer-kill ()
  "Backup buffer if it has unsaved changes."
  (when b/has-unbacked-up-changes
    (backups--perform-backup (current-buffer))
    (setq b/last-backup-time (b/get-time)
          b/has-unbacked-up-changes nil)))

(defun backup-all-buffers ()
  "Make backup of files in all buffers that have files :)."
  (dolist (buf (buffer-list))
    (when (and (buffer-file-name buf)
               (buffer-local-value 'b/has-unbacked-up-changes buf))
      (backups--perform-backup buf))))


(add-hook 'after-save-hook #'run-backup-if-has-unsaved-changes)
(add-hook 'kill-buffer-hook #'backup-on-buffer-kill)
(add-hook 'kill-emacs-hook #'backup-all-buffers)

(provide 'backups)

;; Local Variables:
;; End:

;; backups.el ends here
