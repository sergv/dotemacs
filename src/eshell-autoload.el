;; eshell-autoload.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 14 September 2020
;; Description:

(provide 'eshell-autoload)
(require 'set-up-paths)

(setf eshell-buffer-name "esh"
      eshell-directory-name (concat +prog-data-path+ "/eshell/")
      eshell-bad-command-tolerance 10000 ;; don't want eshell autocorrection aliases
      eshell-buffer-maximum-lines 65536
      eshell-buffer-shorthand t
      eshell-cmpl-autolist nil ;; this option is really harmful when set to t
      eshell-cmpl-cycle-completions t
      eshell-cmpl-expand-before-complete nil
      eshell-cmpl-ignore-case t
      eshell-cmpl-recexact nil
      eshell-cmpl-cycle-cutoff-length 3
      eshell-show-lisp-completions nil
      eshell-command-interpreter-max-length 4096
      eshell-error-if-no-glob t
      eshell-glob-include-dot-dot nil
      eshell-hist-ignoredups t
      eshell-history-size 20000
      eshell-password-prompt-regexp "[Pp]ass\\(?:word\\|phrase\\).*:\\s *\\'"
      eshell-prefer-lisp-functions t
      eshell-scroll-show-maximum-output nil
      eshell-scroll-to-bottom-on-input nil
      eshell-scroll-to-bottom-on-output nil
      eshell-send-direct-to-subprocesses nil
      eshell-tar-regexp +tar-regexp+
      eshell-highlight-prompt t

      eshell-ls-product-regexp
      (eval-when-compile
        (concat (regexp-opt +build-products-extensions+) "\\'"))

      eshell-ls-archive-regexp +archive-regexp+

      ;; Can set up files to ignore here.
      ;; eshell-ls-exclude-regexp nil
      )

(add-hook 'eshell-mode-hook #'eshell-setup)

;; Local Variables:
;; End:

;; eshell-autoload.el ends here
