;; vim-common.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 10 November 2022
;; Description:

(defconst vim-common-command-delimiters
  (eval-when-compile (string-to-list "/|,;:!@#(")))

(provide 'vim-common)

;; Local Variables:
;; End:

;; vim-common.el ends here
