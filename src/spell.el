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

(setf ispell-program-name "aspell"
      ;; my dictionary-alist, using for redefinition russian dictionary
      ispell-dictionary-alist
      '(("english"   ;; English
         "[a-zA-Z]"  ;; casecshars
         "[^a-zA-Z]" ;; not-casechars
         "['-’]"     ;; other-chars
         nil
         ("-d" "en" "--lang" "en_GB" "--encoding=en_GB.utf8" "--size=90")
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
         utf-8))
      ispell-aspell-dictionary-alist ispell-dictionary-alist

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

;; ;;;###autoload
;; (add-hook 'text-mode-hook 'flyspell-mode)

(setf ispell-have-new-look t
      flyspell-always-use-popup t
      flyspell-case-fold-duplications t)

(provide 'spell)

;; Local Variables:
;; End:

;; spell.el ends here
