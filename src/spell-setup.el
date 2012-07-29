;;; spell-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 24 January 2012
;; Keywords:
;; Requirements:
;; Status:

(autoload 'flyspell-russian
          "spell"
          "Check russian spelling."
          t)


(autoload 'flyspell-english
          "spell"
          "Check english spelling."
          t)

(provide 'spell-setup)

;; (global-set-key [f1] 'ispell-word)
;; (global-set-key [f7] 'ispell-buffer); проверить орфографию в текущем буфере
;; (global-set-key [f8] 'ispell-region)
;; (global-set-key [f9] 'auto-fill-mode); вкл/выкл автозаполнения
;; (global-set-key [f10] 'flyspell-english)
;; (global-set-key [f11] 'flyspell-russian)
;; (global-set-key [f12] 'flyspell-mode); вкл/выкл проверки орфографии "на ходу"

;;============================================================================

;; Local Variables:
;; lexical-binding: t
;; End:

;;; spell-setup.el ends here
