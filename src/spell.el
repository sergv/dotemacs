;;; spell.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 24 January 2012
;; Keywords:
;; Requirements:
;; Status:


(require 'flyspell)
(require 'ispell)

(setf ispell-program-name "aspell"

      ;; my dictionary-alist, using for redefinition russian dictionary
      ispell-dictionary-alist
      '(("english"                       ;; English
         "[a-zA-Z]"
         "[^a-zA-Z]"
         "['-]"
         nil
         ("-d" "en")
         nil
         utf-8
         ;;iso-8859-1
         )
        ("russian"                       ;; Russian
         "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
         "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
         "[`'-]"
         nil
         ("-C" "-d" "ru")
         nil
         utf-8)
        (nil                             ;; Default
         "[A-Za-z]"
         "[^A-Za-z]"
         "[']"
         nil
         ("-C")
         nil
         iso-8859-1))

      ispell-russian-dictionary "russian"
      ispell-english-dictionary "english"
      flyspell-default-dictionary ispell-russian-dictionary
      ispell-dictionary ispell-english-dictionary
      ;; ispell-local-dictionary ispell-russian-dictionary
      ispell-extra-args '("--sug-mode=normal")
      ;; '("--sug-mode=ultra")
      ;; '("--sug-mode=fast")
      )

(defun flyspell-russian ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary ispell-russian-dictionary)
  (flyspell-buffer)
  (message "Russian dictionary - Spell Checking completed."))

;; English
(defun flyspell-english ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary ispell-english-dictionary)
  (flyspell-buffer)
  (message "English dictionary - Spell Checking completed."))


(add-hook 'text-mode-hook 'flyspell-mode)

(setf ispell-have-new-look t
      ispell-enable-tex-parser t
      ;; flyspell-delay 1
      flyspell-always-use-popup t)

(provide 'spell)


;; Local Variables:
;; lexical-binding: t
;; End:

;;; spell.el ends here
