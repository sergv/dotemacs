;; maxima-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 20 February 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
 (require 'cl))

(when (executable-find "maxima")
  (require 'browse-kill-ring-setup)
  (setf imaxima-fnt-size "huge"
        imaxima-label-color "#dc322f"
        imaxima-latex-preamble ""
        imaxima-maxima-options (format "-q --disable-readline --preload-lisp='%s/third-party/maxima/imaxima.lisp'"
                                       +emacs-config-path+)
        imaxima-pt-size 12
        imaxima-scale-factor 1.0
        imaxima-tmp-dir (concat +prog-data-path+ "/imaxima-tmp/")
        maxima-args (list "-q"
                          "--disable-readline"
                          (format "--preload-lisp=%s/third-party/maxima/maxima-tex.lisp"
                                  +emacs-config-path+))
        maxima-input-history-file (concat +prog-data-path+
                                          "/.maxima_history")
        maxima-input-history-length 65536
        maxima-save-input-history t
        maxima-use-full-color-in-process-buffer t)

  (autoload 'maxima-mode "maxima" "Maxima mode" t)
  (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
  (autoload 'maxima "maxima" "Maxima interaction" t)
  (autoload 'imath-mode "imath" "Imath mode for math formula input" t)
  (setf imaxima-use-maxima-mode-flag t)

  (add-to-list 'auto-mode-alist '("\\.max" . maxima-mode))
  (add-to-list 'auto-mode-alist '("\\.mac" . maxima-mode))

  (make-align-function maxima-align-matrix-commas
                       ","
                       :repeat t
                       :require-one-or-more-spaces t
                       :put-align-spaces-after-str t)

  (defun maxima-setup ()
    (init-common :use-yasnippet nil :use-render-formula t)
    (autopair-mode t)

    (setf vim:normal-mode-local-keymap (make-keymap)
          vim:insert-mode-local-keymap (make-sparse-keymap)
          vim:visual-mode-local-keymap (make-sparse-keymap))

    (def-keys-for-map vim:normal-mode-local-keymap
      ("SPC SPC" switch-to-maxima)

      ("j"       maxima-send-previous-form)
      ("J"       maxima-send-previous-form))

    (def-keys-for-map (vim:normal-mode-local-keymap
                       vim:insert-mode-local-keymap)
      ("M-/"     maxima-complete)
      ;; use load file instead of send buffer to be more consistent
      ;; with another similar modes, e.g. slime, octave, prolog etc
      ;; upd: maxima denies to load file my files, says there's errors
      ;; and I have no clue about these errors. I'll just use send buffer
      ("<f1>"    ;; maxima-load-current-file
       maxima-send-buffer)

      ("C-<right>"   paredit-forward-slurp-sexp)
      ("C-S-<right>" paredit-forward-barf-sexp)
      ("C-<left>"    paredit-backward-slurp-sexp)
      ("C-S-<left>"  paredit-backward-barf-sexp))

    (define-key vim:normal-mode-local-keymap
        (kbd ", h")
      maxima-help-map)

    (def-keys-for-map vim:visual-mode-local-keymap
      ("j"     maxima-send-region)
      ("J"     maxima-send-region)
      ("g a ," maxima-align-matrix-commas))

    (def-keys-for-map maxima-mode-map
      ("<f12>" nil)))

  (define-switch-to-interpreter
      switch-to-maxima
    ((when (processp inferior-maxima-process)
       (process-buffer inferior-maxima-process))
     "*maxima*"
     "*imaxima*")
    (imaxima)
    :test-if-already-running
    (and (processp inferior-maxima-process)
         (buffer-live-p (process-buffer inferior-maxima-process)))
    :doc "Switch to maxima repl."
    :save-buffer t
    :error-msg "Can't switch to maxima repl")

  (define-circular-jumps
      maxima-jump-to-next-prompt
    maxima-jump-to-prev-prompt
    comint-prompt-regexp
    (unless (string-match-p "\\*i?maxima\\*" (buffer-name))
      (error "Not in the maxima buffer")))

  (defun interactive-maxima-setup ()
    (init-repl)
    (autopair-mode t)

    (setf vim:normal-mode-local-keymap (make-keymap)
          vim:insert-mode-local-keymap (make-keymap))

    (def-keys-for-map vim:normal-mode-local-keymap
      ("SPC SPC" comint-clear-prompt))

    (def-keys-for-map (vim:normal-mode-local-keymap
                       vim:insert-mode-local-keymap)
      ("M-/"         inferior-maxima-input-complete)
      ("M-p"         browse-kill-ring)
      ("M-P"         browse-comint-input-history)

      ("C-c C-c"     maxima-stop)
      ("<up>"        comint-previous-input)
      ("<down>"      comint-next-input)
      ("<return>"    inferior-maxima-check-and-send-line)
      ("RET"         inferior-maxima-check-and-send-line)

      ("C-<up>"      maxima-jump-to-prev-prompt)
      ("C-<down>"    maxima-jump-to-next-prompt)
      ("S-<up>"      maxima-jump-to-prev-prompt)
      ("S-<down>"    maxima-jump-to-next-prompt)

      ("C-<right>"   paredit-forward-slurp-sexp)
      ("C-S-<right>" paredit-forward-barf-sexp)
      ("C-<left>"    paredit-backward-slurp-sexp)
      ("C-S-<left>"  paredit-backward-barf-sexp))

    (define-key vim:normal-mode-local-keymap
        (kbd ", h")
      maxima-help-map))

  (add-hook 'maxima-mode-hook #'maxima-setup)
  (add-hook 'imaxima-startup-hook #'interactive-maxima-setup)
  (add-hook 'inferior-maxima-mode-hook #'interactive-maxima-setup))


(provide 'maxima-setup)

;; Local Variables:
;; End:

;; maxima-setup.el ends here
