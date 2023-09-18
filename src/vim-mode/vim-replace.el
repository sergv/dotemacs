;; vim-replace.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'vim-core)
(require 'vim-ex)

(cl-defun vim--construct-ex-replace-command (str
                                             &key
                                             (word nil)
                                             (symbol nil)
                                             (fill-replacement nil))
  (unless str
    (error "No string for replace"))
  (let ((separator
         (or (cl-find-if (lambda (sep)
                           (not (cl-search sep str)))
                         '("," "/" "@" "|" "#" "=" "&"))
             ",")))
    (concat "%s"
            separator
            (cond (word "\\<") (symbol "\\_<"))
            (regexp-quote str)
            (cond (word "\\>") (symbol "\\_>"))
            separator
            (when fill-replacement
              (vim--substitute-quote str))
            separator)))

(defun vim--substitute-quote (text)
  "Just quote backslash for now because it has special meaning and all other
special characters are introduced via backlash only."
  (replace-regexp-in-string "\\\\"     ;; single backslash
                            "\\\\\\\\" ;; double backslash
                            text
                            ))


(defsubst vim--get-str-covered-by-motion (motion)
  (buffer-substring-no-properties
   (vim-motion-begin-pos motion)
   (vim-motion-end-pos motion)))

(defun vim-replace--minibuffer-setup-jump-before-last-separator ()
  "Hook function to properly position cursor in the %s command, e.g.
%s/foo/bar_|_/
"
  (remove-hook 'minibuffer-setup-hook #'vim-replace--minibuffer-setup-jump-before-last-separator)
  (goto-char (point-max))
  (backward-char))

(defun vim--start-ex-with-customized-substitute-command (str)
  (interactive)
  (let ((vim-ex--current-buffer (current-buffer))
        (vim-ex--current-window (selected-window)))
    (let ((minibuffer-local-completion-map vim-ex-keymap))
      (add-hook 'minibuffer-setup-hook #'vim-ex--start-session)
      (add-hook 'minibuffer-setup-hook #'vim-replace--minibuffer-setup-jump-before-last-separator)
      (let ((result (read-from-minibuffer vim-ex--prompt
                                          str
                                          vim-ex-keymap
                                          nil
                                          'vim-ex--history)))
        (when (and result
                   (not (zerop (length result))))
          (vim-ex-execute-command result))))))

(vim-defcmd vim:replace-word (nonrepeatable)
  "Partially construct vim ex-replace command from word at point."
  (vim--start-ex-with-customized-substitute-command
   (vim--construct-ex-replace-command
    (vim--get-str-covered-by-motion
     (save-excursion
       (vim:motion-inner-word:wrapper)))
    :word t
    :fill-replacement (not current-prefix-arg))))

(vim-defcmd vim:replace-WORD (nonrepeatable)
  "Partially construct vim ex-replace command from WORD at point."
  (vim--start-ex-with-customized-substitute-command
   (vim--construct-ex-replace-command
    (vim--get-str-covered-by-motion
     (save-excursion
       (vim:motion-inner-WORD:wrapper)))
    :fill-replacement (not current-prefix-arg))))

(vim-defcmd vim:replace-selected (nonrepeatable)
  "Partially construct vim ex-replace command from selected region."
  (vim--start-ex-with-customized-substitute-command
   (vim--construct-ex-replace-command
    (get-region-string-no-properties)
    :fill-replacement (not current-prefix-arg))))

(vim-defcmd vim:replace-symbol-at-point (nonrepeatable)
  "Partially construct vim ex-replace command from symbol at point.
With prefix argument puts symbol at point also in substitute part"
  (vim:replace-symbol-at-point--impl 'symbol))

(defun vim:replace-symbol-at-point--impl (thing)
  (vim--start-ex-with-customized-substitute-command
   (vim--construct-ex-replace-command
    (thing-at-point thing)
    :symbol t
    :fill-replacement (not current-prefix-arg))))

(provide 'vim-replace)

;; Local Variables:
;; End:

;; vim-replace.el ends here
