;; util-vim-replace.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


(require 'vim-core)

(defun* util:construct-ex-replace-command (str
                                           &key
                                           (word nil)
                                           (symbol nil)
                                           (fill-replace nil))
  (concat "%s,"
          (cond (word "\\<") (symbol "\\_<"))
          (regexp-quote str)
          (cond (word "\\>") (symbol "\\_>"))
          ","
          (when fill-replace
            (vim:substitute-quote str))))

(defun vim:substitute-quote (text)
  "Just quote backslash for now because it has special meaning and all other
special characters are introduced via backlash only."
  (replace-regexp-in-string "\\\\"      ;; single backslash
                            "\\\\\\\\"  ;; double backslash
                            text
                            ))



(defsubst util:get-str-covered-by-vim-motion (motion)
  (buffer-substring-no-properties (vim:motion-begin-pos motion)
                                  (vim:motion-end-pos motion)))

(defun util:ex-customized-substitute-command (str)
  (interactive)
  (let ((vim:ex-current-buffer (current-buffer))
        (vim:ex-current-window (selected-window)))
    (let ((minibuffer-local-completion-map vim:ex-keymap))
      (add-hook 'minibuffer-setup-hook #'vim:ex-start-session)
      (let ((result (completing-read ":"
                                     'vim:ex-complete
                                     nil
                                     nil
                                     str
                                     'vim:ex-history)))
        (when (and result
                   (not (zerop (length result))))
          (vim:ex-execute-command result))))))


(vim:defcmd vim:replace-word (nonrepeatable)
  "Partially construct vim ex-replace command from word at point."
  (util:ex-customized-substitute-command
   (util:construct-ex-replace-command
    (util:get-str-covered-by-vim-motion
     (save-excursion
       (vim:motion-inner-word)))
    :word t
    :fill-replace (not current-prefix-arg))))

(vim:defcmd vim:replace-WORD (nonrepeatable)
  "Partially construct vim ex-replace command from WORD at point."
  (util:ex-customized-substitute-command
   (util:construct-ex-replace-command
    (util:get-str-covered-by-vim-motion
     (save-excursion
       (vim:motion-inner-WORD))))
   :fill-replace (not current-prefix-arg)))

(vim:defcmd vim:replace-selected (nonrepeatable)
  "Partially construct vim ex-replace command from selected region."
  (when (vim:visual-mode-p)
    (util:ex-customized-substitute-command
     (util:construct-ex-replace-command
      (buffer-substring-no-properties
       (region-beginning)
       (1+ (region-end)))
      :fill-replace (not current-prefix-arg)))))

(vim:defcmd vim:replace-symbol-at-point (nonrepeatable)
  "Partially construct vim ex-replace command from symbol at point.
With prefix argument puts symbol at point also in substitute part"
  (util:ex-customized-substitute-command
   (util:construct-ex-replace-command
    (thing-at-point 'symbol)
    :symbol t
    :fill-replace (not current-prefix-arg))))

(provide 'util-vim-replace)


;; Local Variables:
;; End:

;; util-vim-replace.el ends here
