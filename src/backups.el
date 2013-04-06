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

(eval-when-compile (require 'cl-lib))

(require 'custom)
(require 'common)

(defconst b/backup-directory (path-concat +prog-data-path+ "backup"))

(defvar b/backup-interval 3600
  "Time interval in seconds to make backups on save.")

(defvar b/last-backup-time nil
  "Time of last backup made by `run-backup-as-needed'.")
(make-variable-buffer-local 'b/last-backup-time)
(set-default 'b/last-backup-time nil)

(defvar b/has-unbackupped-changes nil
  "Becomes true whenever you perform save that has no corresponding backup.")
(make-variable-buffer-local 'b/has-unbackupped-changes)
(set-default 'b/has-unbackupped-changes nil)

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
         (time (format-time-string (if (platform-os-type? 'linux)
                                     "%H:%M %d %b %Y"
                                     "%H_%M %d %b %Y")))
         (half-name (concat file " - " time " - "))
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
            (if (platform-os-type? 'linux)
              (replace-regexp-in-string "/" ":" directory)
              nil)
            extension)))

(defun make-backup (&optional buf)
  "Make backup of specified buffer or current buffer if BUF is nil."
  ;; ensure that destination directory exists
  (unless (file-exists-p b/backup-directory)
    (make-directory b/backup-directory t))

  (setq buf (or buf (current-buffer)))
  (with-current-buffer buf
    (if-buffer-has-file
      (let* ((file (buffer-file-name))
             (dest (path-concat b/backup-directory
                                (b/make-backup-name file))))
        (when (file-exist-p file)
          (copy-file file dest t t t)
          ;; (if (file-exists-p dest)
          ;; (error "make-backup: fatal error: backup file %s already exists"
          ;; dest)
          ;; (copy-file file dest t t t))
          dest)))))


(defun run-backup-as-needed ()
  "Run `make-backup' on save at time intervals specified
by `b/backup-interval'."
  (if (or (not b/last-backup-time)
          (< b/backup-interval (- (b/get-time) b/last-backup-time)))
    (progn
      (make-backup)
      (setq b/last-backup-time (b/get-time)
            b/has-unbackupped-changes nil))
    (setq b/has-unbackupped-changes t)))

(defun backup-on-kill ()
  "Backup buffer if it has unsaved changes."
  (when b/has-unbackupped-changes
    (make-backup)
    (setq b/last-backup-time (b/get-time)
          b/has-unbackupped-changes nil)))

(defun backup-all-buffers ()
  "Make backup of files in all buffers that have files :)."
  (mapc #'make-backup
        (remove-if-not (lambda (buf)
                         (and (buffer-file-name buf)
                              (buffer-local-value
                               'b/has-unbackupped-changes
                               buf)))
                       (buffer-list))))


(add-hook 'after-save-hook #'run-backup-as-needed)
(add-hook 'kill-buffer-hook #'backup-on-kill)
(add-hook 'kill-emacs-hook #'backup-all-buffers)

(provide 'backups)

;; Local Variables:
;; End:

;; backups.el ends here
