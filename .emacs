
(eval-when-compile (require 'cl-lib))

(require 'cl)

;; 1e4   recursion depth is quite safe since:
;; 1.6e4 still works
;; 2e4   makes emacs crash
(setq max-lisp-eval-depth 10000
      ;; to handle all unwind-protects and similar stuff
      ;; in deep recursion
      max-specpdl-size    42000)

;; speeds up startup time considerably, worth to use
(setq gc-cons-threshold (* 5 1024 1024)
      gc-cons-percentage 0.01)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Customizations done by emacs customize routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables)

(provide 'custom-variables-defined)

(unless (featurep 'dotemacs)
  (dolist (path (cl-remove-duplicates
                 (append
                  (list "/home/sergey/emacs"
                        (expand-file-name "~/emacs"))
                  (when (eq system-type 'windows-nt)
                    (list
                     ;; unfortunately Windows has no reasonable symlinks
                     (expand-file-name "~/.emacs.d"))))
                 :test #'string=))
    (when (and (file-exists-p path)
               (file-directory-p path))
      (add-to-list 'load-path path)))

  (load-library "dotemacs"))

;; do not squander the memory
;; (setq gc-cons-threshold (* 5 1024 1024)
;;       gc-cons-percentage 0.10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Custom function declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Customizations done by emacs customize routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Emacs/W3 Configuration
;; (custom-set-faces)





