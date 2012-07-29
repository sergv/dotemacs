
;; set up shell scripting files and shell interaction mode

(require 'custom)
(require 'comint-setup)

(autoload 'shell-command-on-region+ "shell-command+" nil t)
(fset 'shell-command-on-region 'shell-command-on-region+)


(defun shell-run-file ()
  "Run buffer's script file."
  (interactive)
  (compilation-start (concat "./"
                             (file-name-nondirectory
                              (shell-quote-argument
                               (buffer-file-name))))
                     t))

(defun shell-script-setup ()
  (add-hook 'after-save-hook #'make-script-file-exec nil t)

  (init-common)
  (autopair-mode)
  (which-function-mode -1)

  (setq vim:insert-mode-local-keymap (make-sparse-keymap)
        vim:normal-mode-local-keymap (make-sparse-keymap))

  (def-keys-for-map1 vim:insert-mode-local-keymap
      (("M-/" icicle-comint-dynamic-complete-filename)))

  (def-keys-for-map1 vim:normal-mode-local-keymap
      (("<f9>"  shell-run-file)
       ("M-/" icicle-comint-dynamic-complete-filename)))

  (setq whitespace-line-column 80)
  (setq whitespace-style '(tabs lines-tail))
  (whitespace-mode t))


(defun shell-setup ()
  (init-repl :show-directory t)

  (setq autopair-dont-activate t)
  (linum-mode 1)
  (ansi-color-for-comint-mode-on)

  (setq vim:normal-mode-local-keymap (make-keymap)
        vim:insert-mode-local-keymap (make-keymap))

  (def-keys-for-map2 vim:normal-mode-local-keymap
    ;; clear all previous output
    ("SPC SPC" comint-clear-prompt))

  (def-keys-for-map2 (vim:normal-mode-local-keymap
                      vim:insert-mode-local-keymap
                      shell-mode-map)
    ("C-SPC"   comint-clear-buffer-above-prompt)
    ("<tab>"   pcomplete)
    ("M-/"     pcomplete)

    ("M-p"     browse-kill-ring)
    ("M-P"     browse-comint-input-history)

    ("C-w"      backward-delete-word)
    ("C-S-w"    backward-delete-word*)

    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)
    ("C-<up>"   comint-previous-prompt)
    ("C-<down>" comint-next-prompt)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt))

  (setq comint-scroll-to-bottom-on-input t))


(provide 'shell-setup)
