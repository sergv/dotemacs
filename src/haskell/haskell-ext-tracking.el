;; haskell-ext-tracking.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 21 April 2022
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'haskell-regexen))

(require 'flycheck-haskell)

(cl-defstruct haskell-ext-tracking-known-exts
  magic-hash
  import-qualified-post)

(defsubst haskell-ext-tracking-have-magic-hash? ()
  (haskell-ext-tracking-known-exts-magic-hash haskell-ext-tracking-known-exts--store))

(defsubst haskell-ext-tracking-have-import-qualified-post? ()
  (haskell-ext-tracking-known-exts-import-qualified-post haskell-ext-tracking-known-exts--store))

(defun haskell-ext-tracking-known-exts--default ()
  (make-haskell-ext-tracking-known-exts
   :magic-hash nil
   :import-qualified-post nil))

(defvar haskell-ext-tracking-known-exts--store (haskell-ext-tracking-known-exts--default))

(defun haskell-ext-tracking--update! ()
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((end (if (re-search-forward "^module " nil t)
                     (match-beginning 1)
                   nil)))
        (goto-char (point-min))
        (while (re-search-forward (eval-when-compile
                                    (concat haskell-regexen/language-pragma-prefix
                                            (rx (+? anything)
                                                symbol-start
                                                (or (group-n 1 "MagicHash")
                                                    (group-n 2 "ImportQualifiedPost"))
                                                symbol-end)))
                                  end
                                  t)
          (cond
            ((match-beginning 1)
             (setf (haskell-ext-tracking-known-exts-magic-hash haskell-ext-tracking-known-exts--store) t))
            ((match-beginning 2)
             (setf (haskell-ext-tracking-known-exts-import-qualified-post haskell-ext-tracking-known-exts--store) t))))))))

;;;###autoload
(define-minor-mode haskell-ext-tracking-mode
  "Toggle tracking of which extensions are enabled in current buffer for other elisp to use"
  :init-value nil
  :lighter nil
  :keymap nil
  :global nil
  (progn
    (if haskell-ext-tracking-mode
        (progn
          (set (make-local-variable 'haskell-ext-tracking-known-exts--store)
               (haskell-ext-tracking-known-exts--default))
          (let ((buf (current-buffer)))
            (when-let ((config (flycheck-haskell-get-configuration-for-buf buf (eproj-get-project-for-buf-lax buf))))
              (let-alist-static config (extensions languages)
                (setf (haskell-ext-tracking-known-exts-magic-hash haskell-ext-tracking-known-exts--store)
                      (member "MagicHash" extensions)
                      (haskell-ext-tracking-known-exts-import-qualified-post haskell-ext-tracking-known-exts--store)
                      (or (member "ImportQualifiedPost" extensions)
                          (member "GHC2021" languages))))))
          (haskell-ext-tracking--update!)
          (dolist (hook '(after-save-hook after-revert-hook))
            (add-hook hook
                      #'haskell-ext-tracking--update!
                      nil ;; append
                      t   ;; local
                      )))
      (progn
        (kill-local-variable 'haskell-ext-tracking-known-exts--store)
        (dolist (hook '(after-save-hook after-revert-hook))
          (remove-hook hook
                       #'haskell-ext-tracking--update!
                       t ;; local
                       ))))))

(provide 'haskell-ext-tracking)

;; Local Variables:
;; End:

;; haskell-ext-tracking-mode.el ends here
