;;; haskell-commands.el --- Commands that can be run on the process -*- lexical-binding: t -*-

;;; Commentary:

;;; This module provides varoius `haskell-mode' specific commands such
;;; as show type signature, show info, haskell process commands and
;;; etc.

;; Copyright © 2014 Chris Done.  All rights reserved.
;;             2016 Arthur Fayzrakhmanov

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'current-column-fixed)

(require 'cl-lib)
(require 'etags)
(require 'haskell-mode)
(require 'haskell-font-lock)
(require 'haskell-string)
(require 'haskell-utils)
(require 'haskell-cabal)
(require 'haskell-ghc-support)

(defcustom haskell-mode-stylish-haskell-path "stylish-haskell"
  "Path to `stylish-haskell' executable."
  :group 'haskell
  :type 'string)

(defcustom haskell-mode-stylish-haskell-args nil
  "Arguments to pass to program specified by haskell-mode-stylish-haskell-path."
  :group 'haskell
  :type 'list)

;;;###autoload
(defun haskell-mode-stylish-buffer ()
  "Apply stylish-haskell to the current buffer.

Use `haskell-mode-stylish-haskell-path' to know where to find
stylish-haskell executable.  This function tries to preserve
cursor position and markers by using
`haskell-mode-buffer-apply-command'."
  (interactive)
  (haskell-mode-buffer-apply-command haskell-mode-stylish-haskell-path haskell-mode-stylish-haskell-args))

(defun haskell-mode-buffer-apply-command (cmd &optional args)
  "Execute shell command CMD with ARGS and current buffer as input and output.
Use buffer as input and replace the whole buffer with the
output.  If CMD fails the buffer remains unchanged."
  (set-buffer-modified-p t)
  (let* ((out-file (make-temp-file "stylish-output"))
         (err-file (make-temp-file "stylish-error"))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8))
    (unwind-protect
        (let* ((_errcode
                (apply 'call-process-region (point-min) (point-max) cmd nil
                       `((:file ,out-file) ,err-file)
                       nil args))
               (err-file-empty-p
                (equal 0 (nth 7 (file-attributes err-file))))
               (out-file-empty-p
                (equal 0 (nth 7 (file-attributes out-file)))))
          (if err-file-empty-p
              (if out-file-empty-p
                  (message "Error: %s produced no output and no error information, leaving buffer alone" cmd)
                ;; Command successful, insert file with replacement to preserve
                ;; markers.
                (insert-file-contents out-file nil nil nil t))
            (progn
              ;; non-null stderr, command must have failed
              (with-current-buffer
                  (get-buffer-create "*haskell-mode*")
                (insert-file-contents err-file)
                (buffer-string))
              (message "Error: %s ended with errors, leaving buffer alone, see *haskell-mode* buffer for stderr" cmd)
              (with-temp-buffer
                (insert-file-contents err-file)
                ;; use (warning-minimum-level :debug) to see this
                (display-warning cmd
                                 (buffer-substring-no-properties (point-min) (point-max))
                                 :debug)))))
      (ignore-errors
        (delete-file err-file))
      (ignore-errors
        (delete-file out-file)))))

(provide 'haskell-commands)
;;; haskell-commands.el ends here
