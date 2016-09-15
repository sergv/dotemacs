;; spell.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 24 January 2012
;; Keywords:
;; Requirements:
;; Status:


(require 'flyspell)
(require 'ispell)

;; (global-set-key [f1] 'ispell-word)
;; (global-set-key [f7] 'ispell-buffer); проверить орфографию в текущем буфере
;; (global-set-key [f8] 'ispell-region)
;; (global-set-key [f9] 'auto-fill-mode); вкл/выкл автозаполнения
;; (global-set-key [f10] 'flyspell-english)
;; (global-set-key [f11] 'flyspell-russian)
;; (global-set-key [f12] 'flyspell-mode); вкл/выкл проверки орфографии "на ходу"

(setf ispell-program-name "aspell"

      ;; my dictionary-alist, using for redefinition russian dictionary
      ispell-dictionary-alist
      '(("english" ;; English
         "[a-zA-Z]"
         "[^a-zA-Z]"
         "['-]"
         nil
         ("-d" "en")
         nil
         utf-8
         ;;iso-8859-1
         )
        ("russian" ;; Russian
         "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
         "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
         "[`'-]"
         nil
         ("-C" "-d" "ru")
         nil
         utf-8)
        (nil ;; Default
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

;;;###autoload
(defun flyspell-russian ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary ispell-russian-dictionary)
  (flyspell-buffer)
  (message "Russian dictionary - Spell Checking completed."))

;; English
;;;###autoload
(defun flyspell-english ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary ispell-english-dictionary)
  (flyspell-buffer)
  (message "English dictionary - Spell Checking completed."))


;;;###autoload
(add-hook 'text-mode-hook 'flyspell-mode)

(setf ispell-have-new-look t
      ispell-enable-tex-parser t
      ;; flyspell-delay 1
      flyspell-always-use-popup t)

(provide 'spell)

;; Local Variables:
;; End:

;; spell.el ends here
