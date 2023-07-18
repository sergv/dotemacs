;;; flycheck-haskell.el --- Flycheck: Automatic Haskell configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018 Sergey Vinokurov <serg.foo@gmail.com>
;; Copyright (C) 2014-2016 Sebastian Wiesner <swiesner@lunaryorn.com>
;; Copyright (C) 2016-2018 Danny Navarro <j@dannynavarro.net>
;; Copyright (C) 2015 Mark Karpov <markkarpov@opmbx.org>
;; Copyright (C) 2015 Michael Alan Dorman <mdorman@ironicdesign.com>
;; Copyright (C) 2015 Alex Rozenshteyn <rpglover64@gmail.com>
;; Copyright (C) 2014 Gracjan Polak <gracjanpolak@gmail.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/flycheck/flycheck-haskell
;; Keywords: tools, convenience
;; Version: 0.9-cvs
;; Package-Requires: ((emacs "24.3") (flycheck "0.25") (haskell-mode "13.7") (dash "2.4.0") (seq "1.11") (let-alist "1.0.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Automatically configure Flycheck for Haskell.

;;;; Cabal support

;; Try to find a Cabal file for the current Haskell buffer, and configure syntax
;; checking according to the Cabal project settings.

;;;; Stack support

;; Try to find a stack.yaml file for current project and configure stack projct
;; according to the Stack project settings.

;;;; Setup

;;; Code:

(eval-when-compile
  (require 'rx)
  (require 'let-alist))

(require 'seq)
(require 'haskell-cabal)
(require 'flycheck)
(require 'dash)
(require 'dante)

(require 'nix-integration)


;;; Customization

(defgroup flycheck-haskell nil
  "Haskell support for Flycheck."
  :prefix "flycheck-haskell-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck-haskell"))

(defcustom flycheck-haskell-runghc-command
  (let ((stack-exe (funcall flycheck-executable-find "stack"))
        (runghc-exe (funcall flycheck-executable-find "runghc")))
    (cond
      (stack-exe
       `(,stack-exe "--verbosity" "silent" "runghc" "--no-ghc-package-path" "--" "-i"
                    "-packageCabal"
                    "-packageCabal-syntax"
                    "-packagebase"
                    "-packagebytestring"
                    "-packagecontainers"
                    "-packageprocess"
                    "-packagedirectory"
                    "-packagefilepath"))
      (runghc-exe
       `(,runghc-exe "--" "-i"
                     "-packageCabal"
                     "-packageCabal-syntax"
                     "-packagebase"
                     "-packagebytestring"
                     "-packagecontainers"
                     "-packageprocess"
                     "-packagedirectory"
                     "-packagefilepath"))
      (t
       ;; A reasonable default.
       '("runghc" "-i"))))
  "Command for `runghc'.

This library uses `runghc' to run various Haskell helper scripts
to extract information from Cabal files.  This option provides
the command to invoke `runghc'.  The default is to use `stack'
and otherwise fall back to standard `runghc'."
  :type '(repeat (string :tag "Command"))
  :risky t
  :group 'flycheck-haskell)

(defcustom flycheck-haskell-hpack-executable (funcall flycheck-executable-find "hpack")
  "Path to the `hpack' executable.

This library uses `hpack' to get package configuration if `package.yaml' file
is present.  This option provides the path to the `hpack' executable.  The nil
value will make this library ignore `package.yaml' file, even if it's present."
  :type 'string
  :risky t
  :group 'flycheck-haskell)


;;; Cabal support
(defconst flycheck-haskell-directory
  (file-name-directory (if load-in-progress
                           load-file-name
                         (buffer-file-name)))
  "The package directory of flycheck-haskell.")

(defconst flycheck-haskell-helper
  (expand-file-name "get-cabal-configuration.hs" flycheck-haskell-directory)
  "The helper to dump the Cabal configuration.")

(defconst flycheck-haskell--compiled-haskell-helper
  (let ((base-name (expand-file-name "get-cabal-configuration" flycheck-haskell-directory)))
    (-find #'file-executable-p
           (--map (concat base-name it)
                  exec-suffixes)))
  "The helper to dump the Cabal configuration.")

(defun flycheck-haskell-runghc-command (proj args)
  "Create a runghc command with ARGS.

Take the base command from `flycheck-haskell-runghc-command'."
  (nix-maybe-call-via-flakes
   (append flycheck-haskell-runghc-command args)
   (when proj
     (eproj-project/root proj))))

(defun flycheck-haskell--read-configuration-with-helper (args proj)
  (let ((env process-environment)
        (path exec-path))
    (with-temp-buffer
      ;; Copy the entire environment just in case there's something we need.
      (setq-local process-environment env)
      ;; Set path so we can find the command.
      (setq-local exec-path path)
      (let* ((cmd
              (if flycheck-haskell--compiled-haskell-helper
                  (cons flycheck-haskell--compiled-haskell-helper args)
                (flycheck-haskell-runghc-command proj (cons flycheck-haskell-helper args))))
             (retcode (apply #'call-process (car cmd) nil (current-buffer) nil (cdr cmd))))
        (if (zerop retcode)
            (progn
              (goto-char (point-min))
              (read (current-buffer)))
          (progn
            (goto-char (point-max))
            (delete-whitespace-backward)
            (error "Reading Haskell configuration failed with exit code %s:\n\nCommand: %s\n\nOutput:\n%s"
                   retcode
                   (s-join " " cmd)
                   (buffer-string))))))))

(defun flycheck-haskell-read-cabal-configuration (cabal-file proj)
  "Read the Cabal configuration from CABAL-FILE."
  (let ((args (list "--cabal-file" (expand-file-name cabal-file))))
    (flycheck-haskell--read-configuration-with-helper args proj)))


;;; Cabal configuration caching
(defvar flycheck-haskell-config-cache (make-hash-table :test 'equal)
  "Cache of Cabal configuration.

A hash table, mapping the name of a cabal file to a
cons-cell `(MODTIME . CONFIG)', where MODTIME is the modification
time of the cabal file, and CONFIG the extracted configuration.")

(defun flycheck-haskell-clear-config-cache ()
  "Clear the cache of configurations."
  (interactive)
  (clrhash flycheck-haskell-config-cache))

(defun flycheck-haskell-get-cached-configuration (config-file)
  "Get the cached configuration for CABAL-FILE.

Return the cached configuration, or nil, if there is no cache
entry, or if the cache entry is outdated."
  (pcase-let* ((cache-entry (gethash config-file flycheck-haskell-config-cache))
               (`(,modtime . ,config) cache-entry))
    (when (and modtime (file-exists-p config-file))
      (let ((current-modtime (nth 5 (file-attributes config-file))))
        (if (time-less-p modtime current-modtime)
            ;; The entry is outdated, drop it.  `remhash' always
            ;; returns nil, so we are safe to use it here.
            (remhash config-file flycheck-haskell-config-cache)
          ;; The configuration is up to date, use it
          config)))))

(defun flycheck-haskell-read-and-cache-configuration (config-file proj)
  "Read and cache configuration from CABAL-FILE.

Return the configuration."
  (let ((modtime (nth 5 (file-attributes config-file)))
        (config (flycheck-haskell-read-cabal-configuration config-file proj)))
    (puthash config-file (cons modtime config) flycheck-haskell-config-cache)
    config))

(defun flycheck-haskell-get-configuration (config-file proj)
  "Get the Cabal configuration from CABAL-FILE.

Get the configuration either from our cache, or by reading the
CABAL-FILE.

Return the configuration."
  (or (flycheck-haskell-get-cached-configuration config-file)
      (flycheck-haskell-read-and-cache-configuration config-file proj)))

(defun flycheck-haskell-get-configuration-for-buf (buf proj)
  (when-let ((config-file (flycheck-haskell--find-config-file buf)))
    (flycheck-haskell-get-configuration config-file proj)))


;;; Buffer setup

(defun flycheck-haskell--find-config-file (buf)
  (unless (dante-cabal-script-buf? buf)
    (haskell-cabal-find-file)))

(provide 'flycheck-haskell)

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:


;;; flycheck-haskell.el ends here
