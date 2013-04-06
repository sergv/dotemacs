;; util-vim-replace.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


(require 'vim-core)

(defun* util:construct-ex-replace-command (str &key
                                               (word nil)
                                               (symbol nil)
                                               (fill-replace nil))
  (concat "%s,"
          (cond (word "\\<") (symbol "\\_<"))
          (regexp-quote str)
          (cond (word "\\>") (symbol "\\_>"))
          ","
          (when fill-replace
            str)))



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
    :fill-replace current-prefix-arg)))

(vim:defcmd vim:replace-WORD (nonrepeatable)
  "Partially construct vim ex-replace command from WORD at point."
  (util:ex-customized-substitute-command
   (util:construct-ex-replace-command
    (util:get-str-covered-by-vim-motion
     (save-excursion
       (vim:motion-inner-WORD))))
   :fill-replace current-prefix-arg))

(vim:defcmd vim:replace-selected (nonrepeatable)
  "Partially construct vim ex-replace command from selected region."
  (when (vim:visual-mode-p)
    (util:ex-customized-substitute-command
     (util:construct-ex-replace-command
      (buffer-substring-no-properties
       (region-beginning)
       (1+ (region-end)))
      :fill-replace current-prefix-arg))))

(vim:defcmd vim:lisp-replace-symbol (nonrepeatable)
  "Partially construct vim ex-replace command from symbol at point.
With prefix argument puts symbol at point also in substitute part"
  (util:ex-customized-substitute-command
   (util:construct-ex-replace-command
    (slime-symbol-at-point)
    :symbol t
    :fill-replace current-prefix-arg)))


(vim:defcmd vim:replace-symbol-at-point (nonrepeatable)
  "Partially construct vim ex-replace command from symbol at point.
With prefix argument puts symbol at point also in substitute part"
  (util:ex-customized-substitute-command
   (util:construct-ex-replace-command
    (symbol-name (symbol-at-point))
    :symbol t
    :fill-replace current-prefix-arg)))


;; ;; all this code duplication stuff probably could be reduced to
;; ;; one or two macro's but as for now (11.08.11, 11:28) I'm too tired
;; ;; to invent any macro's
;;
;; (defun* util:construct-convenient-ex-replace-command (str &key
;;                                                           (word nil)
;;                                                           (symbol nil))
;;   (let ((clean-str (trim-whitespaces str)))
;;     (concat
;;      (util:construct-ex-replace-command clean-str
;;                                         :word word
;;                                         :symbol symbol)
;;      clean-str)))
;;
;;
;; (vim:defcmd vim:convenient-replace-word (count nonrepeatable)
;;   (util:ex-customized-substitute-command
;;    (util:construct-convenient-ex-replace-command
;;     (util:get-str-covered-by-vim-motion
;;      (save-excursion
;;        (vim:motion-inner-word :count (or count 1))))
;;     :word t)))
;;
;; (vim:defcmd vim:convenient-replace-WORD (count nonrepeatable)
;;   (util:ex-customized-substitute-command
;;    (util:construct-convenient-ex-replace-command
;;     (util:get-str-covered-by-vim-motion
;;      (save-excursion
;;        (vim:motion-inner-WORD :count (or count 1)))))))
;;
;; (vim:defcmd vim:convenient-replace-selected (nonrepeatable)
;;   (when (vim:visual-mode-p)
;;     (util:ex-customized-substitute-command
;;      (util:construct-convenient-ex-replace-command
;;       (buffer-substring-no-properties
;;        (region-beginning)
;;        (region-end))))))
;;
;; (vim:defcmd vim:lisp-convenient-replace-symbol (nonrepeatable)
;;   "Partially construct vim ex-replace command from symbol at point."
;;   (util:ex-customized-substitute-command
;;    (util:construct-convenient-ex-replace-command
;;     (slime-symbol-at-point)
;;     :symbol t)))

(provide 'util-vim-replace)


;; Local Variables:
;; End:

;; util-vim-replace.el ends here
