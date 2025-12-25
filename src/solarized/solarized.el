;; solarived+.el --- -*- lexical-binding: t; -*-

;; Author: Ethan Schoonover, Solarized; Greg Pfeil, Emacs adaptation
;; URL: http://ethanschoonover.com/solarized

(eval-when-compile
  (require 'cl)
  (require 'set-up-platform)

  (require 'macro-util))

(require 'solarized-palettes)

;; (defun solarized-color-clamp-lab (lab)
;;   "Restricts a LAB colorspace color if it is out of bounds."
;;   (list (min (max (nth 0 lab) 0.0) 100.0)
;;         (min (max (nth 1 lab) -128) 127)
;;         (min (max (nth 2 lab) -128) 127)))
;;
;; (defun solarized-color-rgb-to-hex (red green blue &optional digits-per-component round)
;;   "Return hexadecimal #RGB notation for the color specified by RED GREEN BLUE.
;; RED, GREEN, and BLUE should be numbers between 0.0 and 1.0, inclusive.
;; Optional argument DIGITS-PER-COMPONENT can be either 4 (the default)
;; or 2; use the latter if you need a 24-bit specification of a color.
;; Optional argument ROUND rounds values which probably is what you usually want."
;;   (or digits-per-component (setq digits-per-component 4))
;;   (let* ((maxval (if (= digits-per-component 2) 255 65535))
;;          (fmt (if (= digits-per-component 2) "#%02x%02x%02x" "#%04x%04x%04x")))
;;     (if round
;;         (format fmt (+ 0.5 (* red maxval)) (+ 0.5 (* green maxval)) (+ 0.5(* blue maxval)))
;;       (format fmt (* red maxval) (* green maxval) (* blue maxval)))))
;;
;; (defun solarized-color-blend (color1 color2 alpha &optional digits-per-component)
;;   "Blends COLOR1 onto COLOR2 with ALPHA.
;;
;; COLOR1 and COLOR2 should be color names (e.g. \"white\") or RGB
;; triplet strings (e.g. \"#ff12ec\").
;;
;; Alpha should be a float between 0 and 1.
;;
;; Optional argument DIGITS-PER-COMPONENT can be either 4 (the default) or 2;
;; use the latter if you need a 24-bit specification of a color."
;;   (let ((args (mapcar #'color-clamp
;;                       (apply #'color-lab-to-srgb
;;                              (solarized-color-clamp-lab
;;                               (cl-mapcar (lambda (v1 v2)
;;                                            (+ v1 (* alpha (- v2 v1))))
;;                                          (apply #'color-srgb-to-lab (color-name-to-rgb color2))
;;                                          (apply #'color-srgb-to-lab (color-name-to-rgb color1))))))))
;;     (apply #'solarized-color-rgb-to-hex `(,@args ,digits-per-component t))))


;; ;; ;; All fields are strings denoting html colors, i.e. of the form "#XXXXXX" for x being a hex digit.
;; (cl-defstruct solarized-base-palette
;;   darkest-base
;;   brightest-base
;;   yellow
;;   orange
;;   red
;;   magenta
;;   violet
;;   blue
;;   cyan
;;   green)



(defmacro solarized-define-faces-into (out-var &rest body)
  "Regular two-level quoting macro."
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (x)
                 `(push (,'\` ,(list (car x) `((t ,@(cdr x)))))
                        ,out-var))
               body)))


;; (when (eq 'light mode)
;;   (cl-rotatef background aux-high-2)
;;   (cl-rotatef background-highlights aux-high-1)
;;   (cl-rotatef secondary-content emphasized-content)
;;   (cl-rotatef aux-mid primary-content)
;;   (cl-rotatef black white)
;;
;;   (setf highlight-orange-background     "#f9ea7c"
;;         highlight-yellow-background     "#fbee00"
;;         highlight-green-background      "#e2f500"
;;         highlight-cyan-background       "#98f9eb"
;;         highlight-blue-background       "#ceeef1"
;;         highlight-violet-background     "#f2e3fd"
;;         highlight-cyan-green-background "#a2fe8e"
;;         highlight-pink-background       "#fddfff"
;;         highlight-red-background        highlight-pink-background))
;;
;; (when (= (display-color-cells) 8)
;;   (setf background  "black"
;;         background-highlights  "black"
;;         aux-high-1   "white"
;;         aux-high-2   "white"
;;         red     "red"
;;         orange  "red"
;;         yellow  "yellow"
;;         green   "green"
;;         cyan    "cyan"
;;         blue    "blue"
;;         violet  "magenta"
;;         magenta "magenta")
;;   (if (eq 'light mode)
;;       (progn
;;         (setf secondary-content                      "black"
;;               aux-mid                      "black"
;;               primary-content                       "black"
;;               emphasized-content                       "black"
;;               highlight-orange-background     "white"
;;               highlight-yellow-background     "white"
;;               highlight-green-background      "white"
;;               highlight-cyan-background       "white"
;;               highlight-blue-background       "white"
;;               highlight-violet-background     "white"
;;               highlight-cyan-green-background "white"
;;               highlight-pink-background       "white"
;;               highlight-red-background        "white"))
;;     (setf secondary-content                      "white"
;;           aux-mid                      "white"
;;           primary-content                       "white"
;;           emphasized-content                       "white"
;;           highlight-orange-background     "black"
;;           highlight-yellow-background     "black"
;;           highlight-green-background      "black"
;;           highlight-cyan-background       "black"
;;           highlight-blue-background       "black"
;;           highlight-violet-background     "black"
;;           highlight-cyan-green-background "black"
;;           highlight-pink-background       "black"
;;           highlight-red-background        "black")))


(defun solarized-apply-palette (palette)
  "Solarized color theme."
  (let* ((mode                      (solarized-palette-mode palette))

         (background                (num->color (solarized-palette-background                palette)))
         (background-highlights     (num->color (solarized-palette-background-highlights     palette)))
         (secondary-content         (num->color (solarized-palette-secondary-content         palette)))
         (aux-mid                   (num->color (solarized-palette-aux-mid                   palette)))
         (primary-content           (num->color (solarized-palette-primary-content           palette)))
         (emphasized-content        (num->color (solarized-palette-emphasized-content        palette)))
         (aux-high-1                (num->color (solarized-palette-aux-high-1                palette)))
         (aux-high-2                (num->color (solarized-palette-aux-high-2                palette)))

         (red                       (num->color (solarized-palette-red                       palette)))
         (orange                    (num->color (solarized-palette-orange                    palette)))
         (yellow                    (num->color (solarized-palette-yellow                    palette)))
         (green                     (num->color (solarized-palette-green                     palette)))
         (cyan                      (num->color (solarized-palette-cyan                      palette)))
         (blue                      (num->color (solarized-palette-blue                      palette)))
         (violet                    (num->color (solarized-palette-violet                    palette)))
         (magenta                   (num->color (solarized-palette-magenta                   palette)))

         ;; (red-aux-fg                (num->color (solarized-palette-red-aux-fg                palette)))
         ;; (orange-aux-fg             (num->color (solarized-palette-orange-aux-fg             palette)))
         ;; (yellow-aux-fg             (num->color (solarized-palette-yellow-aux-fg             palette)))
         ;; (green-aux-fg              (num->color (solarized-palette-green-aux-fg              palette)))
         ;; (cyan-aux-fg               (num->color (solarized-palette-cyan-aux-fg               palette)))
         ;; (blue-aux-fg               (num->color (solarized-palette-blue-aux-fg               palette)))
         ;; (violet-aux-fg             (num->color (solarized-palette-violet-aux-fg             palette)))
         ;; (magenta-aux-fg            (num->color (solarized-palette-magenta-aux-fg            palette)))

         (red-aux-bg                (num->color (solarized-palette-red-aux-bg                palette)))
         (orange-aux-bg             (num->color (solarized-palette-orange-aux-bg             palette)))
         (yellow-aux-bg             (num->color (solarized-palette-yellow-aux-bg             palette)))
         (green-aux-bg              (num->color (solarized-palette-green-aux-bg              palette)))
         (cyan-aux-bg               (num->color (solarized-palette-cyan-aux-bg               palette)))
         (blue-aux-bg               (num->color (solarized-palette-blue-aux-bg               palette)))
         (violet-aux-bg             (num->color (solarized-palette-violet-aux-bg             palette)))
         (magenta-aux-bg            (num->color (solarized-palette-magenta-aux-bg            palette)))

         ;; (red-aux-aux-fg            (num->color (solarized-palette-red-aux-aux-fg            palette)))
         ;; (orange-aux-aux-fg         (num->color (solarized-palette-orange-aux-aux-fg         palette)))
         ;; (yellow-aux-aux-fg         (num->color (solarized-palette-yellow-aux-aux-fg         palette)))
         ;; (green-aux-aux-fg          (num->color (solarized-palette-green-aux-aux-fg          palette)))
         ;; (cyan-aux-aux-fg           (num->color (solarized-palette-cyan-aux-aux-fg           palette)))
         ;; (blue-aux-aux-fg           (num->color (solarized-palette-blue-aux-aux-fg           palette)))
         ;; (violet-aux-aux-fg         (num->color (solarized-palette-violet-aux-aux-fg         palette)))
         ;; (magenta-aux-aux-fg        (num->color (solarized-palette-magenta-aux-aux-fg        palette)))

         (red-aux-aux-bg            (num->color (solarized-palette-red-aux-aux-bg            palette)))
         (orange-aux-aux-bg         (num->color (solarized-palette-orange-aux-aux-bg         palette)))
         (yellow-aux-aux-bg         (num->color (solarized-palette-yellow-aux-aux-bg         palette)))
         (green-aux-aux-bg          (num->color (solarized-palette-green-aux-aux-bg          palette)))
         (cyan-aux-aux-bg           (num->color (solarized-palette-cyan-aux-aux-bg           palette)))
         (blue-aux-aux-bg           (num->color (solarized-palette-blue-aux-aux-bg           palette)))
         (violet-aux-aux-bg         (num->color (solarized-palette-violet-aux-aux-bg         palette)))
         (magenta-aux-aux-bg        (num->color (solarized-palette-magenta-aux-aux-bg        palette)))

         ;; (red-aux-aux-aux-fg        (num->color (solarized-palette-red-aux-aux-aux-fg        palette)))
         ;; (orange-aux-aux-aux-fg     (num->color (solarized-palette-orange-aux-aux-aux-fg     palette)))
         ;; (yellow-aux-aux-aux-fg     (num->color (solarized-palette-yellow-aux-aux-aux-fg     palette)))
         ;; (green-aux-aux-aux-fg      (num->color (solarized-palette-green-aux-aux-aux-fg      palette)))
         ;; (cyan-aux-aux-aux-fg       (num->color (solarized-palette-cyan-aux-aux-aux-fg       palette)))
         ;; (blue-aux-aux-aux-fg       (num->color (solarized-palette-blue-aux-aux-aux-fg       palette)))
         ;; (violet-aux-aux-aux-fg     (num->color (solarized-palette-violet-aux-aux-aux-fg     palette)))
         ;; (magenta-aux-aux-aux-fg    (num->color (solarized-palette-magenta-aux-aux-aux-fg    palette)))

         (red-aux-aux-aux-bg        (num->color (solarized-palette-red-aux-aux-aux-bg        palette)))
         (orange-aux-aux-aux-bg     (num->color (solarized-palette-orange-aux-aux-aux-bg     palette)))
         (yellow-aux-aux-aux-bg     (num->color (solarized-palette-yellow-aux-aux-aux-bg     palette)))
         (green-aux-aux-aux-bg      (num->color (solarized-palette-green-aux-aux-aux-bg      palette)))
         (cyan-aux-aux-aux-bg       (num->color (solarized-palette-cyan-aux-aux-aux-bg       palette)))
         (blue-aux-aux-aux-bg       (num->color (solarized-palette-blue-aux-aux-aux-bg       palette)))
         (violet-aux-aux-aux-bg     (num->color (solarized-palette-violet-aux-aux-aux-bg     palette)))
         (magenta-aux-aux-aux-bg    (num->color (solarized-palette-magenta-aux-aux-aux-bg    palette)))

         ;; highlight backgrounds
         (highlight-red-background        red-aux-aux-aux-bg)
         (highlight-orange-background     orange-aux-aux-aux-bg)
         (highlight-yellow-background     yellow-aux-aux-aux-bg)
         (highlight-green-background      green-aux-aux-aux-bg)
         (highlight-cyan-background       cyan-aux-aux-aux-bg)
         (highlight-blue-background       blue-aux-aux-aux-bg)
         (highlight-violet-background     violet-aux-aux-aux-bg)
         (highlight-magenta-background    magenta-aux-aux-aux-bg)

         (box-line-width (eval-when-compile
                           (or (when-emacs-version (>= it 28)
                                 '(-1 . -1))
                               -1)))

         faces)

    (solarized-define-faces-into faces
      (default :foreground ,primary-content :background ,background)
      (shadow :foreground ,emphasized-content)
      (cursor :background ,cyan)
      (ivy-posframe-cursor :foreground ,cyan :bold t)
      (escape-glyph-face :foreground ,red)
      (fringe :foreground ,secondary-content :background ,background-highlights)
      (help-key-binding :box (:line-width ,box-line-width :color ,emphasized-content))
      (highlight :background ,background-highlights)
      (menu :foreground ,primary-content :background ,background-highlights)
      (mode-line :foreground ,emphasized-content
                 :background ,background
                 :box (:line-width ,box-line-width :color ,emphasized-content))
      (mode-line-buffer-id :foreground ,emphasized-content)
      (mode-line-inactive :foreground ,primary-content
                          :background ,background-highlights
                          :box (:line-width ,box-line-width :color ,background-highlights))
      (pulse-highlight-start-face :background ,magenta)
      (pulse-highlight-face :background ,magenta)
      (region :background ,background-highlights
              :underline t)
      (tab-bar-tab :foreground ,magenta :background ,background)
      (tab-bar-tab-inactive :background ,background)
      (tab-bar :foreground ,primary-content
               :background ,background)
      (secondary-selection :background ,background-highlights)
      (trailing-whitespace :background ,magenta)
      (vertical-border :foreground ,primary-content)
      ;; compilation
      (compilation-info :foreground ,green :bold t)
      (compilation-error :inherit error)
      (compilation-warning :foreground ,orange :bold t)
      (eproj-symbnav-file-name :foreground ,yellow :bold t)
      ;; customize
      (custom-button :background ,background-highlights
                     :box (:line-width 2 :style released-button))
      (custom-button-mouse :inherit custom-button :foreground ,emphasized-content)
      (custom-button-pressed :inherit custom-button-mouse
                             :box (:line-width 2 :style pressed-button))
      (custom-comment-tag :background ,background-highlights)
      (custom-documentation :inherit default)
      (custom-group-tag :foreground ,orange :bold t)
      (custom-link :foreground ,violet)
      (custom-state :foreground ,green)
      (custom-variable-tag :foreground ,orange :bold t)
      ;; emacs-wiki
      (emacs-wiki-bad-link-face :foreground ,red :underline t)
      (emacs-wiki-link-face :foreground ,blue :underline t)
      (emacs-wiki-verbatim-face :inherit default :underline t)
      (error :foreground ,red :bold t)
      ;; font-lock
      (font-lock-comment-face :foreground ,secondary-content
                              ;; :italic t
                              )
      (font-lock-warning-face :inherit warning)

      (font-lock-builtin-face :inherit font-lock-keyword-face)
      (font-lock-constant-face :foreground ,cyan)
      (font-lock-doc-face :inherit font-lock-comment-face :bold t)
      (font-lock-function-name-face :inherit default)
      (font-lock-keyword-face :foreground ,violet :bold t)
      (font-lock-negation-char-face :foreground ,magenta)
      (font-lock-preprocessor-face :inherit font-lock-string-face)
      (font-lock-string-face :foreground ,orange)
      (font-lock-type-face :foreground ,yellow)
      (font-lock-variable-name-face :inherit font-lock-function-name-face)
      (homoglyph :foreground ,cyan)

      (bison-rule-name-face :foreground ,yellow)
      (rust-question-mark :weight bold :inherit font-lock-negation-char-face)
      (elisp-shorthand-font-lock-face :inherit font-lock-keyword-face)

      (haskell-operator-face :foreground ,red)
      (haskell-constructor-face :inherit font-lock-type-face)
      (haskell-pragma-face :inherit font-lock-preprocessor-face)
      (haskell-keyword-face :inherit font-lock-keyword-face)
      (haskell-type-face :inherit font-lock-type-face)

      (haskell-ts-haddock-face :inherit font-lock-comment-face :bold t)
      (haskell-ts-comment-face :inherit font-lock-comment-face)
      (haskell-ts-constant-face :inherit font-lock-constant-face)
      (haskell-ts-quasiquote-pipe-face :inherit default)
      (haskell-ts-string-face :inherit font-lock-string-face)
      (haskell-ts-pragma-face :inherit haskell-pragma-face)
      (haskell-ts-keyword-face :inherit haskell-keyword-face)
      (haskell-ts-operator-face :inherit haskell-operator-face)
      (haskell-ts-type-face :inherit haskell-type-face)
      (haskell-ts-constructor-face :inherit haskell-constructor-face)
      (haskell-ts-strictness-face :inherit font-lock-negation-char-face)

      (ghc-face-error :underline (:style wave :color ,red))
      (ghc-face-warn :underline (:style wave :color ,orange))
      ;; ((supports :underline (:style wave)))
      (ghc-face-hole :underline (:style wave :color ,violet))

      (ghc-profiling-expensive-face :underline (:style wave :color ,orange))
      (uuag-field-name-face :foreground ,orange :bold t)

      ;; agda
      (agda2-highlight-bound-variable-face :inherit default)
      (agda2-highlight-catchall-clause-face :underline t)
      (agda2-highlight-coinductive-constructor-face :foreground ,yellow)
      (agda2-highlight-datatype-face :inherit font-lock-type-face)
      (agda2-highlight-dotted-face :inherit default)
      (agda2-highlight-error-face :inherit error)
      (agda2-highlight-field-face :foreground ,magenta)
      (agda2-highlight-function-face :inherit font-lock-function-name-face)
      (agda2-highlight-incomplete-pattern-face :inherit default :underline t)
      (agda2-highlight-inductive-constructor-face :foreground ,green)
      (agda2-highlight-keyword-face :inherit font-lock-keyword-face)
      (agda2-highlight-module-face :foreground ,violet)
      (agda2-highlight-number-face :foreground ,violet)
      (agda2-highlight-operator-face :inherit default)
      (agda2-highlight-postulate-face :foreground ,blue)
      (agda2-highlight-primitive-face :foreground ,blue)
      (agda2-highlight-primitive-type-face :foreground ,blue)
      (agda2-highlight-record-face :foreground ,blue)
      (agda2-highlight-string-face :inherit font-lock-string-face)
      (agda2-highlight-symbol-face :foreground ,yellow)
      (agda2-highlight-termination-problem-face :inherit warning)
      (agda2-highlight-typechecks-face :inherit default :bold t)
      (agda2-highlight-unsolved-constraint-face :inherit warning :bold t)
      (agda2-highlight-unsolved-meta-face :inherit warning :bold t)

      (flycheck-error :underline (:style wave :color ,red :bold t))
      (flycheck-warning :underline (:style wave :color ,orange :bold t))
      (flycheck-info :underline (:style wave :color ,green :bold t))

      (flycheck-error-fix-available :underline (:style line :color ,red :bold t))
      (flycheck-warning-fix-available :underline (:style line :color ,orange :bold t))
      (flycheck-info-fix-available :underline (:style line :color ,green :bold t))

      (evaporate-region-face :foreground ,secondary-content)
      (lsp-lsp-flycheck-warning-unnecessary-face :foreground ,secondary-content
                                                 :inherit flycheck-warning)

      (lsp-face-highlight-textual :box (:line-width ,box-line-width :color ,cyan))
      (lsp-face-highlight-read :inherit lsp-face-highlight-textual)
      (lsp-face-highlight-write :inherit lsp-face-highlight-textual
                                :bold t
                                :underline (:style wave :color ,cyan))

      (lsp-ui-sideline-code-action :foreground ,cyan)
      (lsp-modeline-code-actions-preferred-face :foreground ,yellow)

      (lsp-details-face :foreground ,secondary-content)

      (clojure-constant-face :foreground ,cyan)
      (clojure-java-interop-face :foreground ,yellow)
      (clojure-meta-type-annotation-face :foreground ,green)

      (js2-error :inherit ghc-face-error)
      (js2-warning :inherit ghc-face-warn)
      (js2-function-param :inherit default)
      (js2-external-variable :foreground ,orange)
      (js2-jsdoc-html-tag-name :foreground ,yellow)
      (js2-jsdoc-html-tag-delimiter :foreground ,green)

      ;; ebuf
      (ebuf-group-1-face :inherit default :bold t)
      (ebuf-group-2-face :inherit default :bold t)
      (ebuf-marked-buffer-face :foreground ,orange :bold t)
      (ebuf-regular-buffer-face :foreground ,orange)
      (ebuf-read-only-buffer-face :foreground ,cyan)
      (ebuf-invisible-buffer-face :foreground ,secondary-content)

      ;; info
      (info-xref :foreground ,blue :underline t)
      (info-xref-visited :inherit info-xref :foreground ,magenta)
      ;; org
      (org-hide :foreground ,background)
      (org-todo :foreground ,red :bold t)
      (org-done :foreground ,green :bold t)
      (org-cancelled :foreground ,violet :bold t)
      (org-waiting :foreground ,orange :bold t)
      (org-started :foreground ,blue :bold t)

      (org-agenda-date :foreground ,blue)
      (org-agenda-date-today :foreground ,blue :bold t)
      (org-agenda-date-weekend :foreground ,blue :bold t)
      (org-agenda-restriction-lock :background ,yellow-aux-bg)
      (org-agenda-structure :foreground ,violet)
      (org-date-selected :foreground ,magenta)
      (org-clock-overlay :background ,highlight-yellow-background)
      (org-code :inherit markdown-code-face)
      (org-document-info :foreground ,violet)
      (org-document-title :foreground ,violet :bold t)
      (org-date :foreground ,violet :underline t)
      (org-drawer :foreground ,blue)
      (org-footnote :foreground ,violet :underline t)
      (org-ellipsis :foreground ,yellow :underline t)
      (org-mode-line-clock-overrun :background ,highlight-red-background)
      (org-sexp-date :foreground ,magenta)
      (org-table :foreground ,violet)
      (org-time-grid :foreground ,orange)
      (org-scheduled-today :foreground ,cyan)
      (org-scheduled :foreground ,cyan)
      (org-warning :foreground ,orange)
      ;; outlines, inherited by org mode too
      (outline-1 :foreground ,orange)
      (outline-2 :foreground ,yellow)
      (outline-3 :foreground ,green)
      (outline-4 :foreground ,cyan)
      (outline-5 :foreground ,blue)
      (outline-6 :foreground ,violet)
      (outline-7 :foreground ,magenta)
      (outline-8 :foreground ,red)

      (hydra-face-blue :foreground ,blue)
      (hydra-face-red :foreground ,red)
      (hydra-face-amaranth :foreground ,magenta)
      (hydra-face-pink :foreground ,yellow)
      (hydra-face-teal :foreground ,cyan)

      ;; sunrise commander
      (sr-active-path-face :inherit default :bold t)
      (sr-passive-path-face :inherit default)
      (sr-editing-path-face :inherit sr-active-path-face :underline t)
      (sr-clex-hotchar-face :foreground ,red :bold t)
      (sr-broken-link-face :foreground ,red)
      (sr-symlink-face :foreground ,cyan)
      (sr-symlink-directory-face :foreground ,cyan :bold t)

      (sr-encrypted-face :foreground ,orange)
      (sr-compressed-face :foreground ,violet)
      (sr-packaged-face :foreground ,violet)
      (sr-xml-face :inherit default)
      (sr-log-face :inherit default)
      (sr-html-face :inherit default)

      (sr-alt-marked-file-face :foreground ,magenta :underline t)
      (sr-alt-marked-dir-face :foreground ,magenta :bold t :underline t)
      (sr-marked-file-face :foreground ,magenta)
      (sr-marked-dir-face :foreground ,magenta :bold t)

      ;; diff & ediff
      (diff-added :foreground ,green)
      (diff-changed :foreground ,yellow)
      (diff-file-header :foreground ,violet)
      (diff-header :foreground ,blue)
      (diff-indicator-added :inherit diff-added :bold t)
      (diff-indicator-changed :inherit diff-changed :bold t)
      (diff-indicator-removed :inherit diff-removed :bold t)
      (diff-removed :foreground ,red)
      (diff-refine-added :underline ,green)
      (diff-refine-change :underline ,yellow)
      (diff-refine-removed :underline ,red)

      (ediff-even-diff-Ancestor :box (:line-width ,box-line-width :color ,background-highlights))
      (ediff-even-diff-A :box (:line-width ,box-line-width :color ,background-highlights))
      (ediff-even-diff-B :box (:line-width ,box-line-width :color ,background-highlights))
      (ediff-even-diff-C :box (:line-width ,box-line-width :color ,background-highlights))
      (ediff-odd-diff-Ancestor :box (:line-width ,box-line-width :color ,background-highlights))
      (ediff-odd-diff-A :box (:line-width ,box-line-width :color ,background-highlights))
      (ediff-odd-diff-B :box (:line-width ,box-line-width :color ,background-highlights))
      (ediff-odd-diff-C :box (:line-width ,box-line-width :color ,background-highlights))

      (ediff-current-diff-Ancestor :box (:line-width ,box-line-width :color ,violet))
      (ediff-current-diff-A :box (:line-width ,box-line-width :color ,emphasized-content))
      (ediff-current-diff-B :box (:line-width ,box-line-width :color ,emphasized-content))
      (ediff-current-diff-C :box (:line-width ,box-line-width :color ,emphasized-content))
      (ediff-fine-diff-Ancestor :underline (:style wave :color ,violet))
      (ediff-fine-diff-A :underline (:style wave :color ,magenta))
      (ediff-fine-diff-B :underline (:style wave :color ,magenta))
      (ediff-fine-diff-C :underline (:style wave :color ,magenta))

      ;; eshell
      (eshell-ls-archive :foreground ,blue :bold t)
      (eshell-ls-backup :foreground ,orange)
      (eshell-ls-clutter :foreground ,red)
      (eshell-ls-directory :foreground ,blue)
      (eshell-ls-executable :foreground ,green)
      (eshell-ls-readonly :inherit default :bold t)
      (eshell-ls-unreadable :inherit default :strike-through t)
      (eshell-ls-missing :foreground ,red :bold t)
      (eshell-ls-product :foreground ,violet)
      (eshell-ls-special :foreground ,magenta)
      (eshell-ls-symlink :foreground ,cyan)
      (eshell-prompt :foreground ,violet :bold t)

      (hl-paren-selection-face :underline ,magenta)
      (show-paren-mismatch :background ,red)

      (rainbow-delimiters-depth-1-face :foreground ,primary-content)
      (rainbow-delimiters-depth-2-face :foreground ,red)
      (rainbow-delimiters-depth-3-face :foreground ,orange)
      (rainbow-delimiters-depth-4-face :foreground ,yellow)
      (rainbow-delimiters-depth-5-face :foreground ,green)
      (rainbow-delimiters-depth-6-face :foreground ,cyan)
      (rainbow-delimiters-depth-7-face :foreground ,blue)
      (rainbow-delimiters-depth-8-face :foreground ,violet)
      (rainbow-delimiters-depth-9-face :foreground ,magenta)
      (rainbow-delimiters-unmatched-face :background ,magenta)

      (reb-match-0 :inherit bold)
      (reb-match-1 :inherit search-red-face)
      (reb-match-2 :inherit search-yellow-face)
      (reb-match-3 :inherit search-cyan-face)

      (ansi-color-bold :inherit bold)
      (ansi-color-italic :inherit italic)
      (ansi-color-underline :inherit underline)

      (ansi-color-black :foreground ,primary-content)
      (ansi-color-red :foreground ,red)
      (ansi-color-green :foreground ,green)
      (ansi-color-yellow :foreground ,yellow)
      (ansi-color-blue :foreground ,blue)
      (ansi-color-magenta :foreground ,magenta)
      (ansi-color-cyan :foreground ,cyan)
      (ansi-color-white :foreground ,secondary-content)

      (ansi-color-bright-black :inherit ansi-color-black)
      (ansi-color-bright-red :inherit ansi-color-red)
      (ansi-color-bright-green :inherit ansi-color-green)
      (ansi-color-bright-yellow :inherit ansi-color-yellow)
      (ansi-color-bright-blue :inherit ansi-color-blue)
      (ansi-color-bright-magenta :inherit ansi-color-magenta)
      (ansi-color-bright-cyan :inherit ansi-color-cyan)
      (ansi-color-bright-white :inherit ansi-color-white)

      ;; rainbow delimiters
      ;; (rainbow-delimiters-depth-1-face :foreground ,magenta)
      ;; (rainbow-delimiters-depth-2-face :foreground ,violet)
      ;; (rainbow-delimiters-depth-3-face :foreground ,blue)
      ;; (rainbow-delimiters-depth-4-face :foreground ,cyan)
      ;; (rainbow-delimiters-depth-5-face :foreground ,green)
      ;; (rainbow-delimiters-depth-6-face :foreground ,yellow)
      ;; (rainbow-delimiters-depth-7-face :foreground ,orange)
      ;; (rainbow-delimiters-depth-8-face :foreground ,red)
      ;; (rainbow-delimiters-depth-9-face :foreground ,primary-content)
      ;; (rainbow-delimiters-unmatched-face :background ,magenta)

      ;; nxhtml
      (mumamo-background-chunk-major :background ,background)

      (mumamo-background-chunk-submode1 :underline ,highlight-cyan-background)
      (mumamo-background-chunk-submode2 :underline ,highlight-green-background)
      (mumamo-background-chunk-submode3 :underline ,highlight-yellow-background)
      (mumamo-background-chunk-submode4 :underline ,highlight-orange-background)
      (nxml-glyph :foreground ,primary-content
                  :background ,background
                  :box (:line-width ,box-line-width :color ,primary-content))
      ;; tags themselves
      (nxml-element-local-name :foreground ,orange)
      ;; tag attributes
      (nxml-attribute-local-name :foreground ,green)
      (nxml-attribute-value :foreground ,yellow)

      (web-mode-current-element-highlight-face :inherit show-paren-match-face)
      (web-mode-html-tag-face :inherit nxml-element-local-name)
      (web-mode-html-attr-name-face :inherit nxml-attribute-local-name)
      (web-mode-html-attr-value-face :inherit nxml-attribute-value)
      (web-mode-html-tag-bracket-face :inherit default)
      (web-mode-html-attr-equal-face :inherit default)
      (web-mode-doctype-face :inherit font-lock-comment-face)


      ;; emms
      (emms-playlist-selected-face :foreground ,blue)
      (emms-playlist-track-face :inherit default)

      ;; python
      (py-number-face :foreground ,cyan)
      (py-variable-name-face :inherit font-lock-variable-name-face)
      (py-XXX-tag-face :foreground ,red)

      (python-warnings-and-errors-face :foreground ,orange)

      ;; scheme
      (scheme-predicate-face :foreground ,blue)
      (scheme-mutating-op-face :foreground ,blue)

      ;; ocaml
      (tuareg-font-lock-error-face :inherit error)
      (tuareg-font-lock-interactive-error-face :inherit error)
      (tuareg-font-lock-interactive-output-face :inherit default)
      (tuareg-font-lock-governing-face :foreground ,yellow)
      (tuareg-font-lock-operator-face :foreground ,cyan)
      (tuareg-font-lock-multistage-face :foreground ,blue :bold t)

      (search-highlight-face :inherit search-magenta-face)
      (search-red-face       :box (:line-width ,box-line-width :color ,red) :extend nil)
      (search-orange-face    :box (:line-width ,box-line-width :color ,orange) :extend nil)
      (search-yellow-face    :box (:line-width ,box-line-width :color ,yellow) :extend nil)
      (search-green-face     :box (:line-width ,box-line-width :color ,green) :extend nil)
      (search-cyan-face      :box (:line-width ,box-line-width :color ,cyan) :extend nil)
      (search-blue-face      :box (:line-width ,box-line-width :color ,blue) :extend nil)
      (search-violet-face    :box (:line-width ,box-line-width :color ,violet) :extend nil)
      (search-magenta-face   :box (:line-width ,box-line-width :color ,magenta) :extend nil)

      (search-modeline-highlight-face :foreground ,magenta)
      (search-modeline-red-face       :foreground ,red)
      (search-modeline-orange-face    :foreground ,orange)
      (search-modeline-yellow-face    :foreground ,yellow)
      (search-modeline-green-face     :foreground ,green)
      (search-modeline-cyan-face      :foreground ,cyan)
      (search-modeline-blue-face      :foreground ,blue)
      (search-modeline-violet-face    :foreground ,violet)
      (search-modeline-magenta-face   :foreground ,magenta)

      ;; other faces
      (antlr-font-lock-default-face :inherit default)
      (antlr-font-lock-keyword-face :inherit font-lock-keyword-face)
      (antlr-font-lock-syntax-face :inherit font-lock-preprocessor-face)
      (antlr-font-lock-ruledef-face :inherit font-lock-function-name-face)
      (antlr-font-lock-tokendef-face :inherit font-lock-type-face)
      (antlr-font-lock-ruleref-face :inherit font-lock-variable-name-face)
      (antlr-font-lock-tokenref-face :inherit font-lock-type-face)
      (antlr-font-lock-literal-face :inherit font-lock-constant-face)

      (prolog-redo-face :foreground ,violet)
      (prolog-exit-face :foreground ,green)
      (prolog-exception-face :foreground ,orange)
      (prolog-warning-face :inherit warning)
      (prolog-builtin-face :inherit font-lock-builtin-face)

      (c-annotation-face :foreground ,violet)
      (completions-common-part :inherit match)
      (csv-separator-face :foreground ,magenta)
      (dired-directory :foreground ,blue)
      (dired-warning :inherit warning :bold t)
      (dired-broken-symlink :inherit error :bold t)
      (ert-test-result-expected :background ,green-aux-bg)
      (ert-test-result-unexpected :background ,red-aux-bg)
      (flyspell-duplicate :bold t :foreground ,green :underline t)
      (flyspell-incorrect :bold t :foreground ,orange :underline t)
      (font-latex-bold-face :inherit bold)
      (font-latex-doctex-documentation-face :inherit font-lock-doc-face)
      (font-latex-doctex-preprocessor-face :inherit font-lock-preprocessor-face)
      (font-latex-italic-face :inherit italic)
      (font-latex-math-face :foreground ,cyan)
      (font-latex-sectioning-0-face :inherit font-latex-sectioning-1-face :height 1.05)
      (font-latex-sectioning-1-face :inherit font-latex-sectioning-2-face :height 1.05)
      (font-latex-sectioning-2-face :inherit font-latex-sectioning-3-face :height 1.05)
      (font-latex-sectioning-3-face :inherit font-latex-sectioning-4-face :height 1.05)
      (font-latex-sectioning-4-face :inherit font-latex-sectioning-5-face :height 1.05)
      (font-latex-sectioning-5-face :foreground ,magenta)
      (font-latex-sedate-face :foreground ,yellow)
      (font-latex-slide-title-face :foreground ,magenta)
      (font-latex-string-face :inherit font-lock-string-face)
      (font-latex-verbatim-face :foreground ,violet)
      (font-latex-warning-face :foreground ,violet)
      (header-line :foreground ,primary-content :background ,background-highlights)
      (help-argument-name :inherit default)
      (hexl-address-region :inherit header-line)
      (hexl-ascii-region :background ,background)
      (isearch :inherit search-highlight-face)
      (ispell-highlight-face :inherit flyspell-incorrect)
      (italic :underline t :italic t)

      (ivy-current-match :underline t)
      (ivy-minibuffer-match-face-1 :foreground ,red)
      (ivy-minibuffer-match-face-2 :foreground ,orange)
      (ivy-minibuffer-match-face-3 :foreground ,yellow)
      (ivy-minibuffer-match-face-4 :foreground ,cyan)

      (ivy-match-required-face :foreground ,red)
      (ivy-confirm-face :foreground ,green)

      (ivy-posframe-border :foreground ,cyan :background ,cyan)

      (lazy-highlight :inherit search-highlight-face)
      (link :foreground ,violet :underline t)
      (match :background ,highlight-cyan-background)

      ;; stable magit fontification
      (git-rebase-hash :foreground ,red)
      (magit-bisect-bad :foreground ,red :bold t)
      (magit-bisect-good :foreground ,green :bold t)
      (magit-bisect-skip :foreground ,yellow :bold t)
      (magit-blame-hash :inherit magit-hash)
      (magit-blame-heading :inherit highlight)
      (magit-branch-current :foreground ,red :box t)
      (magit-branch-local :foreground ,blue :box nil)
      (magit-branch-remote :foreground ,yellow :box nil)
      (magit-cherry-equivalent :foreground ,magenta)
      (magit-cherry-unmatched :foreground ,cyan)
      (magit-diff-added :inherit diff-added)
      (magit-diff-added-highlight :inherit (highlight magit-diff-added))
      (magit-diff-context :inherit default)
      (magit-diff-context-highlight :inherit highlight)
      (magit-diff-hunk-heading :inherit highlight)
      (magit-diff-hunk-heading-highlight :inherit magit-diff-hunk-heading)
      (magit-diff-hunk-heading-selection :inherit magit-diff-hunk-heading :underline t)
      (magit-diff-removed :inherit diff-removed)
      (magit-diff-removed-highlight :inherit (highlight magit-diff-removed))
      (magit-diffstat-added :inherit diff-added)
      (magit-diffstat-removed :inherit diff-removed)
      (magit-file-heading :inherit diff-file-header)
      (magit-hash :foreground ,orange)
      (magit-head :inherit magit-branch-current)
      (magit-hunk-heading :inherit diff-header)
      (magit-hunk-heading-highlight :inherit (highlight magit-hunk-heading))
      (magit-log-author :foreground ,orange)
      (magit-log-date :inherit default)
      (magit-log-graph :inherit default)
      (magit-log-sha1 :foreground ,orange)
      (magit-process-ng :foreground ,red)
      (magit-process-ok :foreground ,green)
      (magit-refine-added :inherit diff-refine-added)
      (magit-refine-removed :inherit diff-refine-removed)
      (magit-reflog-amend :foreground ,orange)
      (magit-reflog-checkout :foreground ,cyan)
      (magit-reflog-cherry-pick :foreground ,magenta)
      (magit-reflog-commit :foreground ,green)
      (magit-reflog-merge :foreground ,yellow)
      (magit-reflog-other :background ,background-highlights :box t)
      (magit-reflog-rebase :foreground ,violet)
      (magit-reflog-remote :background ,background-highlights :box t)
      (magit-reflog-reset :foreground ,blue)
      (magit-refname :foreground ,secondary-content :box t)
      (magit-section-heading :inherit bold)
      (magit-section-highlight :inherit highlight)
      (magit-signature-bad :foreground ,red)
      (magit-signature-good :foreground ,green)
      (magit-signature-untrusted :foreground ,cyan)
      (magit-tag :foreground ,cyan :box t)

      (markdown-code-face :foreground ,cyan)
      (markdown-inline-code-face :inherit markdown-code-face)

      (smerge-refined-added :inherit diff-refine-added)
      (smerge-refined-changed :inherit diff-refine-change)
      (smerge-refined-removed :inherit diff-refine-removed)
      (smerge-base :background ,highlight-yellow-background
                   :extend t)
      ;; Also known as ‘mine’.
      (smerge-upper :background ,highlight-red-background
                    :extend t)
      ;; Also known as ‘other’.
      (smerge-lower :background ,highlight-green-background
                    :extend t)
      (smerge-markers :foreground ,orange :bold t)

      (company-tooltip :foreground ,emphasized-content
                       :background ,background-highlights
                       :box (:line-width ,box-line-width :color ,emphasized-content))
      (company-tooltip-selection :underline (:style wave :color ,orange))
      (company-tooltip-common :foreground ,violet)
      (company-tooltip-annotation :foreground ,green)
      (company-scrollbar-fg :background ,emphasized-content)
      (company-scrollbar-bg :foreground ,emphasized-content :background ,background-highlights)
      (company-preview-search :inherit search-highlight-face)
      (company-preview :foreground ,yellow)
      (company-preview-common :foreground ,orange)

      (j-verb-face :foreground ,red)
      (j-adverb-face :foreground ,green)
      (j-conjunction-face :foreground ,magenta)
      (j-other-face :foreground ,violet)

      (minibuffer-prompt :foreground ,violet)
      (navigation-node-face :foreground ,magenta)
      (paren-face-no-match :underline ,yellow)
      (quack-pltish-class-defn-face :foreground ,violet)
      (quack-pltish-defn-face :foreground ,blue)
      (quack-pltish-keyword-face :foreground ,secondary-content)
      (quack-pltish-module-defn-face :foreground ,violet)
      (quack-threesemi-semi-face :foreground ,emphasized-content)
      (quack-threesemi-text-face :foreground ,emphasized-content)
      (query-replace :background ,highlight-red-background)
      (render-formula-formula-face :foreground ,violet)
      (render-formula-regexp-face :foreground ,blue)
      (rng-error :inherit error)
      (sh-heredoc :foreground ,cyan)
      (sh-quoted-exec :foreground ,magenta)
      (show-paren-match :underline ,magenta)
      (show-paren-match-face :inherit show-paren-match)
      (show-paren-mismatch-face :inherit rainbow-delimiters-unmatched-face)
      (success :foreground ,green :bolt t)
      (tabbar-button-face :inherit tabbar-default-face
                          :box (:line-width 2 :color "white" :style released-button)
                          :foreground "dark red")
      (tabbar-default-face :inherit variable-pitch
                           :height 0.8
                           :foreground ,primary-content
                           :background ,background-highlights)
      (tabbar-selected-face :inherit tabbar-default-face
                            :bold t)
      (tabbar-separator-face :inherit tabbar-default-face)
      (tabbar-unselected-face :inherit tabbar-default-face)
      (table-cell nil)
      (tex-math :foreground ,cyan)
      (undo-tree-visualizer-active-branch-face :weight bold)
      (undo-tree-visualizer-current-face :foreground ,red)
      (undo-tree-visualizer-register-face :foreground ,yellow)
      (vim-lazy-highlight-face :inherit search-highlight-face)
      (vim-search-face :inherit search-highlight-face)
      (vim-substitute-face :underline ,magenta)
      (warning :foreground ,orange :bold t)
      (whitespace-line :underline ,red)
      (whitespace-space-after-tab :underline ,red)
      (whitespace-space-before-tab :underline ,red)
      (whitespace-tab :underline ,background-highlights)
      (yas-field-highlight-face :background ,highlight-cyan-background)

      (isar-keyword1-face :inherit font-lock-keyword-face)
      (isar-keyword2-face :inherit font-lock-keyword-face)
      (isar-keyword3-face :inherit font-lock-keyword-face)
      (isar-keyword4-face :inherit font-lock-keyword-face)

      ;; The face used to mark inactive regions.
      (lsp-isar-font-background-unprocessed1 :inherit lsp-isar-font-background-unprocessed)
      (lsp-isar-font-background-unprocessed :foreground ,aux-mid
                                            ;; :background ,magenta
                                            ;; :background ,orange
                                            )
      (lsp-isar-font-background-running1 :background ,yellow)
      (lsp-isar-font-background-bad :underline (:style wave :color ,red))

      (lsp-isar-font-background-intensify :foreground ,orange)
      (lsp-isar-font-background-quoted :inherit default :bold t)
      (lsp-isar-font-background-antiquoted :foreground ,yellow)
      (lsp-isar-font-background-markdown-bullet1 :foreground ,green)
      (lsp-isar-font-background-markdown-bullet2 :foreground ,orange)
      (lsp-isar-font-background-markdown-bullet3 :foreground ,blue)
      (lsp-isar-font-background-markdown-bullet4 :foreground ,magenta)

      ;; Font used inside quotes and cartouches
      (lsp-isar-font-foreground-quoted ;; :background ,background-highlights
       :background unspecified)

      (lsp-isar-font-foreground-antiquoted :italic t)

      (lsp-isar-font-dotted-writeln nil)
      (lsp-isar-font-dotted-information nil)
      (lsp-isar-font-dotted-warning nil)

      ;; This font is not used by LSP. It’s here only to signal
      ;; whether some new font was added to Isabelle.
      (lsp-isar-font-default :background ,magenta)

      (lsp-isar-font-text-main :inherit default)

      (lsp-isar-font-text-keyword1 :inherit isar-keyword1-face)
      (lsp-isar-font-text-keyword2 :inherit isar-keyword2-face)
      (lsp-isar-font-text-keyword3 :inherit isar-keyword3-face)
      (lsp-isar-font-text-quasi_keyword :inherit font-lock-keyword-face)

      ;; (lsp-isar-font-text-keyword1 :foreground ,orange)
      ;; (lsp-isar-font-text-keyword2 :foreground ,green)
      ;; (lsp-isar-font-text-keyword3 :foreground ,cyan)
      ;; (lsp-isar-font-text-quasi_keyword :foreground ,red)

      (lsp-isar-font-text-improper :underline (:style wave :color ,red))

      (lsp-isar-font-text-operator :foreground ,red)

      (lsp-isar-font-text-tfree :foreground ,magenta)
      (lsp-isar-font-text-tvar :foreground ,magenta)
      (lsp-isar-font-text-free :foreground ,blue)
      (lsp-isar-font-text-skolem :foreground ,yellow)
      (lsp-isar-font-text-bound :foreground ,green)
      (lsp-isar-font-text-var :foreground ,blue)

      (lsp-isar-font-text-inner_numeral :inherit font-lock-constant-face)
      (lsp-isar-font-text-inner_quoted :inherit lsp-isar-font-foreground-quoted :bold t)
      (lsp-isar-font-text-inner_cartouche :inherit lsp-isar-font-foreground-quoted :bold t :underline t)
      (lsp-isar-font-text-inner_comment :inherit font-lock-comment-face)

      (lsp-isar-font-text-dynamic :underline (:color ,green))
      (lsp-isar-font-text-class_parameter :underline (:color ,orange))
      (lsp-isar-font-text-antiquote :underline (:color ,orange))

      (lsp-isar-font-text-overview-unprocessed :inherit lsp-isar-font-background-unprocessed)

      (lsp-isar-font-text-overview-running :underline (:style wave :color ,green))

      (lsp-isar-font-text-overview-error :inherit ghc-face-error)
      (lsp-isar-font-text-overview-warning :inherit ghc-face-warn)

      (lsp-isar-font-spell-checker :underline (:style wave :color ,red))

      (lsp-isar-font-nothing :foreground unspecified :background unspecified))

    (solarized--install-frame-params
     `((foreground-color . ,primary-content)
       (background-color . ,background)
       (background-mode  . ,mode)
       (cursor-color     . ,cyan)))

    (solarized--install-faces faces)

    (put 'markdown-code-face 'saved-face t)

    (setf org-drill-new-count-color    blue
          org-drill-done-count-color   green
          org-drill-failed-count-color magenta
          org-drill-mature-count-color orange
          ansi-color-names-vector (vector primary-content red green yellow blue magenta cyan secondary-content)
          ansi-color-map (ansi-color-make-color-map)
          xterm-color-names ansi-color-names-vector
          xterm-color-names-bright xterm-color-names
          fci-rule-color primary-content
          ansi-term-color-vector
          [aux-high-2
           secondary-content
           red
           green
           yellow
           blue
           magenta
           cyan
           primary-content])
    (run-hooks 'solarized-theme-mode-changed-hook))
  (when window-system
    (update-font-scaling)))

(defun solarized--uniquify-alist (old-list)
  "Reduce OLD-LIST.
The resulting list will be newly allocated and will not contain any elements
with duplicate cars.  This will speed the installation of new themes by
only installing unique attributes."
  (let ((added (make-hash-table :test #'eq))
        new-list)
    (dolist (elem old-list)
      (unless (gethash (car elem) added)
        (puthash (car elem) t added)
        (setq new-list (cons elem new-list))))
    new-list))

(defun solarized--install-frame-params (frame-params)
  ;; setup frame params
  (setq default-frame-alist
        (solarized--uniquify-alist
         (append frame-params default-frame-alist))
        minibuffer-frame-alist
        (solarized--uniquify-alist
         (append frame-params minibuffer-frame-alist)))
  (dolist (frame (frame-list))
    (let ((params (if (eq 'only (cdr (assq 'minibuffer (frame-parameters frame))))
                      minibuffer-frame-alist
                    default-frame-alist)))
      (condition-case err
          (modify-frame-parameters frame params)
        (error (message "Error using frame params %S: %S" params err))))))

(defun solarized--install-faces (faces)
  (dolist (face-spec faces)
    (let ((face (first face-spec))
          (spec (second face-spec)))
      (condition-case err
          (progn
            (face-spec-set face spec)
            (put face 'face-defface-spec spec))
        (error (message "Error using face spec %S: %S" spec err))))))


(defvar solarized-theme-mode-changed-hook '()
  "Hook to run when theme changes")

(defun solarized-dark ()
  (interactive)
  (solarized-apply-known-color-theme 'solarized-dark))

(defun solarized-light ()
  (interactive)
  (solarized-apply-known-color-theme 'solarized-light))

(defvar solarized-known-color-themes
  (list
   (list 'solarized-dark                'solarized-light                solarized-dark-color-palette)
   (list 'solarized-light               'solarized-dark                 solarized-light-color-palette)
   (list 'solarized-dark-high-constrast 'solarized-light-high-constrast solarized-dark-high-contrast-palette)
   (list 'solarized-light-high-contrast 'solarized-dark-high-contrast   solarized-light-high-contrast-palette)
   (list 'gruvbox-dark                  'gruvbox-light                  solarized-gruvbox-dark-color-palette)
   (list 'gruvbox-light                 'gruvbox-dark                   solarized-gruvbox-light-color-palette)
   (list 'zenburn                       nil                             solarized-zenburn-color-palette)
   (list 'selenized-dark                'selenized-light                solarized-selenized-dark-color-palette)
   (list 'selenized-black               'selenized-white                solarized-selenized-black-color-palette)
   (list 'selenized-light               'selenized-dark                 solarized-selenized-light-color-palette)
   (list 'selenized-white               'selenized-black                solarized-selenized-white-color-palette))
  "Registered color themes")

(defvar *current-color-theme* nil
  "Type of current solarized color theme, either 'light or 'dark.")

(defun solarized-apply-known-color-theme (theme)
  (let ((palette (caddr (assq theme solarized-known-color-themes))))
    (if palette
        (progn
          (setf *current-color-theme* theme)
          (solarized-apply-palette palette)
          (tab-bar--update-tab-bar-lines)
          (force-mode-line-update))
      (error "Unknown color theme: %s" theme))))

(defun solarized-toggle ()
  "Toggle type of solarized color theme."
  (interactive)
  (solarized-apply-known-color-theme
   (setf *current-color-theme*
         (aif (assq *current-color-theme* solarized-known-color-themes)
             (cadr it)
           (error "Current color theme is not toggleable: %s" *current-color-theme*)))))

(defun solarized-reapply ()
  "Apply currently selected solarized theme once again.

Useful for applying changes made to color theme definition."
  (interactive)
  (solarized-apply-known-color-theme *current-color-theme*))

(defvar solarized-select-theme-history nil)

(defun solarized-select-theme ()
  (interactive)
  (solarized-apply-known-color-theme
   (setf *current-color-theme*
         (string->symbol
          (completing-read "Color theme: "
                           solarized-known-color-themes
                           nil
                           t
                           nil
                           'solarized-select-theme-history)))))

(provide 'solarized)

;; Local Variables:
;; End:

;; solarized.el ends here
