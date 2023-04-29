;; haskell-outline.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'folding-setup)
(require 'haskell-misc)
(require 'search)
(require 'vim-setup)
(require 'yafolding)

(search-def-autoexpand-advices
 (yafolding-show-element)
 (haskell-mode))

;;;###autoload
(cl-defun haskell-setup-folding (&key (enable-hideshow t) (enable-cpp t))
  "Configure folding for Haskell. ENABLE-HS-MINOR-MODE controls whether
to enable folding of balanced S-expressions."
  (setup-hideshow-yafolding (and enable-hideshow
                                 (if enable-cpp
                                     'enable-cpp
                                   t))
                            '(:header-symbol "-" :length-min 3)))

(provide 'haskell-outline)

;; Local Variables:
;; End:

;; haskell-outline.el ends here
