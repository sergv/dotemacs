;;; ivy-smex.el --- Use Ivy completion with Smex command matching

;; Copyright (C) 2016  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: convenience
;; Package-Requires: ((smex "3.0") (ivy "0.8.0"))

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

;; Smex has really nice handling of command histories, favouring
;; commands which have been executed most frequently in the past.

;; This package provides an Ivy interface to those internals via the
;; `ivy-smex' command, which is meant as a replacement for
;; `execute-extended-command' or `smex':

;; (global-set-key (kbd "M-x") 'ivy-smex)

;;; Code:

(require 'smex)
(require 'ivy)

(defun ivy-smex--init ()
  "Ensure smex is ready to help with completion candidates."
  (unless smex-initialized-p
    (smex-initialize))
  (and smex-auto-update
       (smex-detect-new-commands)
       (smex-update)))

(defun ivy-smex--execute-command (command)
  "Invoke COMMAND and ask smex to rank it."
  (let ((command (intern command))
        (prefix-arg current-prefix-arg))
    (unwind-protect
        (command-execute command 'record)
      (smex-rank command))))

;;;###autoload
(defun ivy-smex ()
  "A version of `smex' using Ivy instead of IDO for completion."
  (interactive)
  (ivy-smex--init)
  (ivy-read "M-x "
            smex-ido-cache
            :require-match t
            :action 'ivy-smex--execute-command
            :caller 'ivy-smex))

(ivy-set-actions
 'ivy-smex
 '(("d" (lambda (c) (describe-function (intern c))) "describe")
   ("g" (lambda (c) (find-function (intern c))) "go to definition")))


(provide 'ivy-smex)
;;; ivy-smex.el ends here
