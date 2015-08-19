;; solarived+.el --- -*- lexical-binding: t; -*-

;; Author: Ethan Schoonover, Solarized; Greg Pfeil, Emacs adaptation
;; URL: http://ethanschoonover.com/solarized

;; This file is not (YET) part of GNU Emacs.

;; # Usage

;; 1. Install the color-theme package
;;   (http://www.emacswiki.org/cgi-bin/wiki/ColorTheme)
;; 2. Load this file
;; 3. M-x color-theme-solarized-[dark|light]

(require 'color-theme)

(defconst +solarized-red+     "#dc322f")
(defconst +solarized-orange+  "#cb4b16")
(defconst +solarized-yellow+  "#b58900")
(defconst +solarized-green+   "#859900")
(defconst +solarized-cyan+    "#2aa198")
(defconst +solarized-blue+    "#268bd2")
(defconst +solarized-violet+  "#6c71c4")
(defconst +solarized-magenta+ "#d33682")

(defun color-theme-solarized+ (mode)
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized.
Futher modified by Sergey Vinokurov."
  (interactive "Slight or dark? ")
  (let* ((base03  "#002b36")
         (base02  "#073642")
         ;; optional emphasized content
         (base01  "#586e75")
         ;; body text/default code/primary content
         (base00  "#657b83")
         ;; ?
         (base0   "#839496")
         ;; comments/secondary content
         (base1   "#93a1a1")
         ;; background highlights
         (base2   "#eee8d5")
         ;; background
         (base3   "#fdf6e3")
         (red     +solarized-red+)
         (orange  +solarized-orange+)
         (yellow  +solarized-yellow+)
         ;; (green   "#719f00")
         (green   +solarized-green+)
         (cyan    +solarized-cyan+)
         (blue    +solarized-blue+)
         (violet  +solarized-violet+)
         (magenta +solarized-magenta+)

         ;; highlight backgrounds
         (light-orange-background     "#652700")
         (light-yellow-background     light-orange-background)
         (light-green-background      "#004800")
         (light-cyan-background       "#0059e9")
         (light-blue-background       "#0000ea")
         (light-violet-background     "#dd00eb")
         (light-cyan-green-background "#006152")
         (light-pink-background       "#e70000")
         (light-red-background        "#910000"))
    (when (eq 'light mode)
      (rotatef base03 base3)
      (rotatef base02 base2)
      (rotatef base01 base1)
      (rotatef base00 base0)

      (setf light-orange-background     "#f9ea7c"
            light-yellow-background     "#fbee00"
            light-green-background      "#e2f500"
            light-cyan-background       "#98f9eb"
            light-blue-background       "#ceeef1"
            light-violet-background     "#f2e3fd"
            light-cyan-green-background "#a2fe8e"
            light-pink-background       "#fddfff"
            light-red-background        light-pink-background))

    (when (= (display-color-cells) 8)
      (setf base03  "black"
            base02  "black"
            base2   "white"
            base3   "white"
            red     "red"
            orange  "red"
            yellow  "yellow"
            green   "green"
            cyan    "cyan"
            blue    "blue"
            violet  "magenta"
            magenta "magenta")
      (if (eq 'light mode)
        (setf base01                      "black"
              base00                      "black"
              base0                       "black"
              base1                       "black"
              light-orange-background     "white"
              light-yellow-background     "white"
              light-green-background      "white"
              light-cyan-background       "white"
              light-blue-background       "white"
              light-violet-background     "white"
              light-cyan-green-background "white"
              light-pink-background       "white"
              light-red-background        "white")
        (setf base01                      "white"
              base00                      "white"
              base0                       "white"
              base1                       "white"
              light-orange-background     "black"
              light-yellow-background     "black"
              light-green-background      "black"
              light-cyan-background       "black"
              light-blue-background       "black"
              light-violet-background     "black"
              light-cyan-green-background "black"
              light-pink-background       "black"
              light-red-background        "black")))
    (color-theme-install
     `(color-theme-solarized+
       ((foreground-color . ,base0)
        (background-color . ,base03)
        (background-mode  . ,mode)
        (cursor-color     . ,cyan))
       ;; basic
       (default                      ((t (:foreground ,base0))))
       ;; (cursor                       ((((class color) (min-colors 16777216))
       ;;                                 (:foreground ,magenta :background nil)
       ;;                                 ;; (:foreground ,base03 :background ,base0)
       ;;                                 )
       ;;                                (t
       ;;                                 (:inverse-video nil))))
       (escape-glyph-face            ((t (:foreground ,red))))
       (fringe                       ((t (:foreground ,base01 :background ,base02))))
       (highlight                    ((((class color) (min-colors 16777216))
                                       (:background ,base02))
                                      (t :inverse-video t)))
       (menu                         ((t (:foreground ,base0 :background ,base02))))
       (minibuffer-prompt            ((t (:foreground ,blue))))
       (mode-line                    ((t (:foreground ,base1 :background ,base02
                                                      :box (:line-width 1 :color ,base1)))))
       (mode-line-buffer-id          ((t (:foreground ,base1))))
       (mode-line-inactive           ((t (:foreground ,base0  :background ,base02
                                                      :box (:line-width 1 :color ,base02)))))
       (region                       ((t (:background ,base02 :underline t))))
       (secondary-selection          ((t (:background ,base02))))
       (trailing-whitespace          ((t (:background ,magenta))))
       (vertical-border              ((t (:foreground ,base0))))
       ;; compilation
       (compilation-info             ((t (:foreground ,green :bold t))))
       (compilation-error            ((t (:inherit error))))
       (compilation-warning          ((t (:foreground ,orange :bold t))))
       ;; customize
       (custom-button                ((t (:background ,base02 :box (:line-width 2 :style released-button)))))
       (custom-button-mouse          ((t (:inherit custom-button :foreground ,base1))))
       (custom-button-pressed        ((t (:inherit custom-button-mouse
                                                   :box (:line-width 2 :style pressed-button)))))
       (custom-comment-tag           ((t (:background ,base02))))
       (custom-comment-tag           ((t (:background ,base02))))
       (custom-documentation         ((t (:inherit default))))
       (custom-group-tag             ((t (:foreground ,orange :bold t))))
       (custom-link                  ((t (:foreground ,violet))))
       (custom-state                 ((t (:foreground ,green))))
       (custom-variable-tag          ((t (:foreground ,orange :bold t))))
       ;; emacs-wiki
       (emacs-wiki-bad-link-face     ((t (:foreground ,red :underline t))))
       (emacs-wiki-link-face         ((t (:foreground ,blue :underline t))))
       (emacs-wiki-verbatim-face     ((t (:foreground ,base00 :underline t))))
       (error                        ((t (:foreground ,red :bold t))))
       ;; font-lock
       (font-lock-comment-face       ((t (:foreground ,base01 ;; :italic t
                                                      ))))
       (font-lock-warning-face       ((t (:inherit warning))))

       (font-lock-builtin-face       ((t (:foreground ,cyan))))
       (font-lock-constant-face      ((t (:foreground ,yellow))))
       (font-lock-doc-face           ((t (:foreground ,green))))
       (font-lock-function-name-face ((t (:foreground ,blue))))
       (font-lock-keyword-face       ((t (:foreground ,cyan))))
       (font-lock-negation-char-face ((t (:foreground ,magenta))))
       (font-lock-preprocessor-face  ((t (:foreground ,orange))))
       (font-lock-string-face        ((t (:foreground ,orange))))
       (font-lock-type-face          ((t (:foreground ,yellow))))
       (font-lock-variable-name-face ((t (:foreground ,blue))))

       (haskell-operator-face        ((t (:foreground ,blue))))
       (haskell-interactive-face-compile-error ((t (:foreground ,red))))
       (ghc-face-error               ((((supports :underline (:style wave)))
                                       (:underline (:style wave :color ,red)))
                                      (t (:inherit error))))
       (ghc-face-warn                ((((supports :underline (:style wave)))
                                       (:underline (:style wave :color ,orange)))
                                      (t (:inherit warning))))
       (ghc-face-hole                ((((supports :underline (:style wave)))
                                       (:underline (:style wave :color ,violet)))
                                      (t (:inherit bold))))

       ;; agda
       (agda2-highlight-bound-variable-face          ((t (:inherit default))))
       (agda2-highlight-coinductive-constructor-face ((t (:foreground ,yellow))))
       (agda2-highlight-datatype-face                ((t (:inherit font-lock-type-face))))
       (agda2-highlight-dotted-face                  ((t (:inherit default))))
       (agda2-highlight-error-face                   ((t (:inherit error))))
       (agda2-highlight-field-face                   ((t (:foreground ,magenta))))
       (agda2-highlight-function-face                ((t (:inherit font-lock-function-name-face))))
       (agda2-highlight-incomplete-pattern-face      ((t (:inherit default :underline t))))
       (agda2-highlight-inductive-constructor-face   ((t (:foreground ,green))))
       (agda2-highlight-keyword-face                 ((t (:inherit font-lock-keyword-face))))
       (agda2-highlight-module-face                  ((t (:foreground ,violet))))
       (agda2-highlight-number-face                  ((t (:foreground ,violet))))
       (agda2-highlight-operator-face                ((t (:inherit default))))
       (agda2-highlight-postulate-face               ((t (:foreground ,blue))))
       (agda2-highlight-primitive-face               ((t (:foreground ,blue))))
       (agda2-highlight-primitive-type-face          ((t (:foreground ,blue))))
       (agda2-highlight-record-face                  ((t (:foreground ,blue))))
       (agda2-highlight-string-face                  ((t (:inherit font-lock-string-face))))
       (agda2-highlight-symbol-face                  ((t (:foreground ,yellow))))
       (agda2-highlight-termination-problem-face     ((t (:inherit warning))))
       (agda2-highlight-typechecks-face              ((t (:inherit defalut :bold t))))
       (agda2-highlight-unsolved-constraint-face     ((t (:inherit warning :bold t))))
       (agda2-highlight-unsolved-meta-face           ((t (:inherit warning :bold t))))

       ;; idris
       (idris-semantic-module-face                   ((t (:inherit default))))
       (idris-semantic-namespace-face                ((t (:inherit default))))

       (idris-semantic-type-face                     ((t (:foreground ,yellow))))
       (idris-semantic-data-face                     ((t (:foreground ,yellow))))
       (idris-semantic-function-face                 ((t (:foreground ,blue))))
       (idris-semantic-bound-face                    ((t (:inherit default))))
       (idris-operator-face                          ((t (:foreground ,blue))))

       (shm-current-face             ((t (:background ,base02))))
       (shm-quarantine-face          ((((supports :underline (:style wave)))
                                       (:underline (:style wave :color ,base01)))
                                      (t (:underline ,base01))))

       (clojure-constant-face        ((t (:foreground ,cyan))))
       (clojure-java-interop-face    ((t (:foreground ,yellow))))
       (clojure-meta-type-annotation-face ((t (:foreground ,green))))

       (js2-error                    ((t (:inherit ghc-face-error))))
       (js2-warning                  ((t (:inherit ghc-face-warn))))
       (js2-function-param           ((t (:inherit default))))
       (js2-external-variable        ((t (:foreground ,orange))))

       ;; info
       (info-xref                    ((t (:foreground ,blue :underline t))))
       (info-xref-visited            ((t (:inherit info-xref :foreground ,magenta))))
       ;; org
       (org-hide                     ((t (:foreground ,base03))))
       (org-todo                     ((t (:foreground ,red :bold t))))
       (org-done                     ((t (:foreground ,green :bold t))))
       (org-cancelled                ((t (:foreground ,violet :bold t))))
       (org-waiting                  ((t (:foreground ,orange :bold t))))
       (org-started                  ((t (:foreground ,blue :bold t))))

       (org-agenda-date              ((t (:foreground ,blue))))
       (org-agenda-date-today        ((t (:foreground ,blue :bold t))))
       (org-agenda-date-weekend      ((t (:foreground ,blue :bold t))))
       (org-agenda-restriction-lock  ((t (:background ,light-yellow-background))))
       (org-agenda-structure         ((t (:foreground ,violet))))
       (org-date-selected            ((t (:foreground ,magenta))))
       (org-clock-overlay            ((t (:background ,light-yellow-background))))
       (org-document-info            ((t (:foreground ,violet))))
       (org-document-title           ((t (:foreground ,violet :bold t))))
       (org-date                     ((t (:foreground ,violet :underline t))))
       (org-drawer                   ((t (:foreground ,blue))))
       (org-footnote                 ((t (:foreground ,violet :underline t))))
       (org-ellipsis                 ((t (:foreground ,yellow :underline t))))
       (org-mode-line-clock-overrun  ((t (:background ,light-red-background))))
       (org-sexp-date                ((t (:foreground ,magenta))))
       (org-table                    ((t (:foreground ,violet))))
       (org-time-grid                ((t (:foreground ,orange))))
       (org-scheduled-today          ((t (:foreground ,cyan))))
       (org-scheduled                ((t (:foreground ,cyan))))
       (org-warning                  ((t (:foreground ,orange))))
       (org-drill-hidden-cloze-face  ((t (:foreground ,base1))))
       (org-drill-visible-cloze-face ((t (:foreground ,green))))
       (org-drill-visible-cloze-hint-face ((t (:foreground ,violet))))
       ;; outlines, inherited by org mode too
       (outline-1                    ((t (:foreground ,orange))))
       (outline-2                    ((t (:foreground ,yellow))))
       (outline-3                    ((t (:foreground ,green))))
       (outline-4                    ((t (:foreground ,cyan))))
       (outline-5                    ((t (:foreground ,blue))))
       (outline-6                    ((t (:foreground ,violet))))
       (outline-7                    ((t (:foreground ,magenta))))
       (outline-8                    ((t (:foreground ,red))))

       ;; sunrise commander
       (sr-active-path-face          ((t (:inherit default :bold t))))
       (sr-passive-path-face         ((t (:inherit default))))
       (sr-editing-path-face         ((t (:inherit sr-active-path-face :underline t))))
       (sr-clex-hotchar-face         ((t (:foreground ,red :bold t))))
       (sr-broken-link-face          ((t (:foreground ,red))))
       (sr-symlink-face              ((t (:foreground ,cyan))))
       (sr-symlink-directory-face    ((t (:foreground ,cyan :bold t))))

       (sr-encrypted-face            ((t (:foreground ,orange))))
       (sr-compressed-face           ((t (:foreground ,violet))))
       (sr-packaged-face             ((t (:foreground ,violet))))
       (sr-xml-face                  ((t (:inherit default))))
       (sr-log-face                  ((t (:inherit default))))
       (sr-html-face                 ((t (:inherit default))))

       (sr-alt-marked-file-face      ((t (:foreground ,magenta :underline t))))
       (sr-alt-marked-dir-face       ((t (:foreground ,magenta :bold t :underline t))))
       (sr-marked-file-face          ((t (:foreground ,magenta))))
       (sr-marked-dir-face           ((t (:foreground ,magenta :bold t))))

       ;; diff & ediff
       (diff-added                           ((t (:foreground ,green))))
       (diff-changed                         ((t (:foreground ,yellow))))
       (diff-file-header                     ((t (:foreground ,violet))))
       (diff-header                          ((t (:foreground ,blue))))
       (diff-indicator-removed               ((t (:inherit diff-removed))))
       (diff-removed                         ((t (:foreground ,red))))
       (diff-refine-added                    ((t (:underline ,green))))
       (diff-refine-change                   ((t (:underline ,yellow))))
       (diff-refine-removed                  ((t (:underline ,red))))
       (ediff-current-diff-Ancestor          ((t (:underline ,violet))))
       (ediff-current-diff-A                 ((t (:background ,yellow))))
       (ediff-current-diff-B                 ((t (:background ,yellow))))
       (ediff-current-diff-C                 ((t (:background ,yellow))))
       (ediff-fine-diff-Ancestor             ((t (:background ,orange))))
       (ediff-fine-diff-A                    ((t (:background ,green))))
       (ediff-fine-diff-B                    ((t (:background ,green))))
       (ediff-fine-diff-C                    ((t (:background ,green))))

       (rainbow-delimiters-depth-1-face      ((t (:foreground ,base0))))
       (rainbow-delimiters-depth-2-face      ((t (:foreground ,red))))
       (rainbow-delimiters-depth-3-face      ((t (:foreground ,orange))))
       (rainbow-delimiters-depth-4-face      ((t (:foreground ,yellow))))
       (rainbow-delimiters-depth-5-face      ((t (:foreground ,green))))
       (rainbow-delimiters-depth-6-face      ((t (:foreground ,cyan))))
       (rainbow-delimiters-depth-7-face      ((t (:foreground ,blue))))
       (rainbow-delimiters-depth-8-face      ((t (:foreground ,violet))))
       (rainbow-delimiters-depth-9-face      ((t (:foreground ,magenta))))
       (rainbow-delimiters-unmatched-face    ((t (:background ,magenta))))

       ;; rainbow delimiters
       ;; (rainbow-delimiters-depth-1-face      ((t (:foreground ,magenta))))
       ;; (rainbow-delimiters-depth-2-face      ((t (:foreground ,violet))))
       ;; (rainbow-delimiters-depth-3-face      ((t (:foreground ,blue))))
       ;; (rainbow-delimiters-depth-4-face      ((t (:foreground ,cyan))))
       ;; (rainbow-delimiters-depth-5-face      ((t (:foreground ,green))))
       ;; (rainbow-delimiters-depth-6-face      ((t (:foreground ,yellow))))
       ;; (rainbow-delimiters-depth-7-face      ((t (:foreground ,orange))))
       ;; (rainbow-delimiters-depth-8-face      ((t (:foreground ,red))))
       ;; (rainbow-delimiters-depth-9-face      ((t (:foreground ,base0))))
       ;; (rainbow-delimiters-unmatched-face    ((t (:background ,magenta))))

       ;; nxhtml
       (mumamo-background-chunk-major        ((t (:background ,base03))))

       (mumamo-background-chunk-submode1     ((t (:underline ,light-cyan-green-background))))
       (mumamo-background-chunk-submode2     ((t (:underline ,light-green-background))))
       (mumamo-background-chunk-submode3     ((t (:underline ,light-yellow-background))))
       (mumamo-background-chunk-submode4     ((t (:underline ,light-cyan-background))))
       (nxml-glyph                           ((t (:foreground ,base0 :background ,base03
                                                              :box (:line-width 1 :color ,base0)))))
       ;; tags themselves
       (nxml-element-local-name              ((t (:foreground ,orange))))
       ;; tag attributes
       (nxml-attribute-local-name            ((t (:foreground ,green))))
       (nxml-attribute-value                 ((t (:foreground ,yellow))))

       (web-mode-current-element-highlight-face ((t (:inherit show-paren-match-face))))
       (web-mode-html-tag-face               ((t (:inherit nxml-element-local-name))))
       (web-mode-html-attr-name-face         ((t (:inherit nxml-attribute-local-name))))
       (web-mode-html-attr-value-face        ((t (:inherit nxml-attribute-value))))
       (web-mode-html-tag-bracket-face       ((t (:inherit default))))
       (web-mode-html-attr-equal-face        ((t (:inherit default))))
       (web-mode-doctype-face                ((t (:inherit font-lock-comment-face))))


       ;; emms
       (emms-playlist-selected-face          ((t (:foreground ,blue))))
       (emms-playlist-track-face             ((t (:inherit default))))

       ;; python
       (py-number-face                       ((t (:foreground ,cyan))))
       (py-variable-name-face                ((t (:inherit font-lock-variable-name-face))))
       (py-XXX-tag-face                      ((t (:foreground ,red))))


       ;; scheme
       (scheme-predicate-face                ((t (:foreground ,blue))))
       (scheme-mutating-op-face              ((t (:foreground ,blue))))

       ;; ocaml
       (tuareg-font-lock-error-face              ((t (:inherit error))))
       (tuareg-font-lock-interactive-error-face  ((t (:inherit error))))
       (tuareg-font-lock-interactive-output-face ((t (:inherit default))))
       (tuareg-font-lock-governing-face          ((t (:foreground ,yellow))))
       (tuareg-font-lock-operator-face           ((t (:foreground ,cyan))))
       (tuareg-font-lock-multistage-face         ((t (:foreground ,blue :bold t))))

       (search-red-face                          ((t (:foreground ,base03 :background ,red))))
       (search-orange-face                       ((t (:foreground ,base03 :background ,orange))))
       (search-yellow-face                       ((t (:foreground ,base03 :background ,yellow))))
       (search-green-face                        ((t (:foreground ,base03 :background ,green))))
       (search-cyan-face                         ((t (:foreground ,base03 :background ,cyan))))
       (search-blue-face                         ((t (:foreground ,base03 :background ,blue))))
       (search-violet-face                       ((t (:foreground ,base03 :background ,violet))))
       (search-magenta-face                      ((t (:foreground ,base03 :background ,magenta))))

       ;; other faces
       (antlr-font-lock-default-face             ((t (:inherit default))))
       (antlr-font-lock-keyword-face             ((t (:inherit font-lock-keyword-face))))
       (antlr-font-lock-syntax-face              ((t (:inherit font-lock-preprocessor-face))))
       (antlr-font-lock-ruledef-face             ((t (:inherit font-lock-function-name-face))))
       (antlr-font-lock-tokendef-face            ((t (:inherit font-lock-type-face))))
       (antlr-font-lock-ruleref-face             ((t (:inherit font-lock-variable-name-face))))
       (antlr-font-lock-tokenref-face            ((t (:inherit font-lock-type-face))))
       (antlr-font-lock-literal-face             ((t (:inherit font-lock-constant-face))))

       (prolog-redo-face                         ((t (:foreground ,violet))))
       (prolog-exit-face                         ((t (:foreground ,green))))
       (prolog-exception-face                    ((t (:foreground ,orange))))
       (prolog-warning-face                      ((t (:inherit warning))))
       (prolog-builtin-face                      ((t (:inherit font-lock-builtin-face))))

       (c-annotation-face                    ((t (:foreground ,violet))))
       (completions-common-part              ((t (:inherit match))))
       (csv-separator-face                   ((t (:foreground ,magenta))))
       (dired-directory                      ((t (:foreground ,blue))))
       (dired-warning                        ((t (:inherit warning :bold t))))
       (ert-test-result-expected             ((t (:background ,light-green-background))))
       (ert-test-result-unexpected           ((t (:background ,light-red-background))))
       (flyspell-duplicate                   ((t (:bold t :foreground ,green :underline t))))
       (flyspell-incorrect                   ((t (:bold t :foreground ,orange :underline t))))
       (font-latex-bold-face                 ((t (:inherit bold))))
       (font-latex-doctex-documentation-face ((t (:inherit font-lock-doc-face))))
       (font-latex-doctex-preprocessor-face  ((t (:inherit font-lock-preprocessor-face))))
       (font-latex-italic-face               ((t (:inherit italic))))
       (font-latex-math-face                 ((t (:foreground ,cyan))))
       (font-latex-sectioning-0-face         ((t (:inherit font-latex-sectioning-1-face :height 1.05))))
       (font-latex-sectioning-1-face         ((t (:inherit font-latex-sectioning-2-face :height 1.05))))
       (font-latex-sectioning-2-face         ((t (:inherit font-latex-sectioning-3-face :height 1.05))))
       (font-latex-sectioning-3-face         ((t (:inherit font-latex-sectioning-4-face :height 1.05))))
       (font-latex-sectioning-4-face         ((t (:inherit font-latex-sectioning-5-face :height 1.05))))
       (font-latex-sectioning-5-face         ((t (:foreground ,magenta))))
       (font-latex-sedate-face               ((t (:foreground ,yellow))))
       (font-latex-slide-title-face          ((t (:foreground ,magenta))))
       (font-latex-string-face               ((t (:inherit font-lock-string-face))))
       (font-latex-verbatim-face             ((t (:foreground ,violet))))
       (font-latex-warning-face              ((t (:foreground ,violet))))
       (header-line                          ((t (:foreground ,base0 :background ,base02))))
       (help-argument-name                   ((t (:inherit default))))
       (hexl-address-region                  ((t (:inherit header-line))))
       (hexl-ascii-region                    ((t (:background ,base03))))
       (ido-indicator                        ((t (:foreground ,blue :width condensed))))
       (ido-only-match                       ((t (:foreground ,cyan))))
       (ido-subdir                           ((t (:foreground ,orange))))
       (isearch                              ((t (:inherit lazy-highlight))))
       (ispell-highlight-face                ((t (:inherit flyspell-incorrect))))
       (italic                               ((t (:underline t :italic t))))
       (lazy-highlight                       ((t (:background ,light-cyan-green-background))))
       (link                                 ((t (:foreground ,violet :underline t))))
       (match                                ((t (:background ,light-cyan-green-background))))

       ;; stable magit fontification
       (git-rebase-hash                      ((t (:foreground ,red))))
       (magit-log-graph                      ((t (:foreground ,magenta))))
       (magit-log-sha1                       ((t (:foreground ,orange))))
       (magit-diff-hunk-header               ((t (:inherit highlight))))
       (magit-log-head-label-bisect-bad      ((t (:foreground ,red))))
       (magit-log-head-label-bisect-good     ((t (:foreground ,green))))
       (magit-log-head-label-default         ((t (:foreground ,base01 :box t))))
       (magit-log-head-label-local           ((t (:foreground ,blue :box t))))
       (magit-log-head-label-remote          ((t (:foreground ,yellow :box t))))
       (magit-log-head-label-tags            ((t (:foreground ,cyan :box t))))
       (magit-log-reflog-label-other         ((t (:background ,base02 :box t))))
       (magit-log-reflog-label-remote        ((t (:background ,base02 :box t))))
       ;; fontification for new magit which I don't use
       (magit-log-graph                      ((t (:foreground ,magenta :background nil))))
       (magit-log-sha1                       ((t (:foreground ,orange :background nil))))
       (magit-section-highlight              ((t (:inherit highlight))))
       (magit-diff-context                   ((t (:inherit default))))
       (magit-diff-context-highlight         ((t (:inherit highlight))))
       (magit-diff-added                     ((t (:inherit diff-added))))
       (magit-diff-removed                   ((t (:inherit diff-removed))))
       (magit-diff-added-highlight           ((t (:inherit (highlight magit-diff-added)))))
       (magit-diff-removed-highlight         ((t (:inherit (highlight magit-diff-removed)))))
       (magit-diffstat-added                 ((t (:inherit diff-added))))
       (magit-diffstat-removed               ((t (:inherit diff-removed))))
       (magit-refine-added                   ((t (:underlite ,green))))
       (magit-refine-removed                 ((t (:underlite ,red))))
       (magit-hunk-heading                   ((t (:inherit diff-header))))
       (magit-hunk-heading-highlight         ((t (:inherit (highlight diff-header)))))
       (magit-section-heading                ((t (:inherit bold))))
       (magit-file-heading                   ((t (:inherit diff-file-header))))
       (magit-hash                           ((t (:foreground ,orange :background nil))))
       (magit-head                           ((t (:foreground ,orange :background nil))))
       (magit-branch-local                   ((t (:foreground ,blue :box t))))
       (magit-branch-remote                  ((t (:foreground ,yellow :box t))))
       (magit-tag                            ((t (:foreground ,cyan :box t))))
       (magit-bisect-bad                     ((t (:foreground ,red :background nil))))
       (magit-bisect-good                    ((t (:foreground ,green :background nil))))
       (magit-bisect-skip                    ((t (:foreground ,yellow :background nil))))
       (magit-log-graph                      ((t (:inherit default))))
       (magit-log-author                     ((t (:inherit orange))))
       (magit-log-date                       ((t (:inherit default))))
       (magit-refname                        ((t (:foreground ,base01 :box t))))
       (magit-reflog-commit                  ((t (:foreground ,green))))
       (magit-reflog-amend                   ((t (:foreground ,orange))))
       (magit-reflog-merge                   ((t (:foreground ,yellow))))
       (magit-reflog-checkout                ((t (:foreground ,cyan))))
       (magit-reflog-reset                   ((t (:foreground ,blue))))
       (magit-reflog-rebase                  ((t (:foreground ,violet))))
       (magit-reflog-cherry-pick             ((t (:foreground ,magenta))))
       (magit-reflog-remote                  ((t (:background ,base02 :box t))))
       (magit-reflog-other                   ((t (:background ,base02 :box t))))
       (magit-process-ok                     ((t (:foreground ,green))))
       (magit-process-ng                     ((t (:foreground ,red))))
       (magit-signature-good                 ((t (:foreground ,green))))
       (magit-signature-bad                  ((t (:foreground ,red))))
       (magit-signature-untrusted            ((t (:foreground ,cyan))))
       (magit-cherry-unmatched               ((t (:foreground ,cyan))))
       (magit-cherry-equivalent              ((t (:foreground ,magenta))))

       (minibuffer-prompt                    ((t (:foreground ,violet))))
       (navigation-node-face                 ((t (:foreground ,magenta))))
       (paren-face-no-match                  ((t (:underline ,yellow))))
       (quack-pltish-class-defn-face         ((t (:foreground ,violet))))
       (quack-pltish-defn-face               ((t (:foreground ,blue))))
       (quack-pltish-keyword-face            ((t (:foreground ,base01))))
       (quack-pltish-module-defn-face        ((t (:foreground ,violet))))
       (quack-threesemi-semi-face            ((t (:foreground ,base1))))
       (quack-threesemi-text-face            ((t (:foreground ,base1))))
       (query-replace                        ((t (:background ,light-pink-background))))
       (render-formula-formula-face          ((t (:foreground ,violet))))
       (render-formula-regexp-face           ((t (:foreground ,blue))))
       (rng-error                            ((t (:inherit error))))
       (sh-heredoc                           ((t (:foreground ,cyan))))
       (sh-quoted-exec                       ((t (:foreground ,magenta))))
       (show-paren-match                     ((t (:underline ,magenta))))
       (show-paren-match-face                ((t (:inherit show-paren-match))))
       (show-paren-mismatch-face             ((t (:inherit rainbow-delimiters-unmatched-face))))
       (tabbar-button-face                   ((t (:inherit tabbar-default-face
                                                           :box (:line-width 2
                                                                             :color "white"
                                                                             :style released-button)
                                                           :foreground "dark red"))))
       (tabbar-default-face                  ((t (:inherit variable-pitch
                                                           :height 0.8
                                                           :foreground ,base0
                                                           :background ,base02))))
       (tabbar-selected-face                 ((t (:inherit tabbar-default-face
                                                           :bold t ;; :foreground ,magenta
                                                           ))))
       (tabbar-separator-face                ((t (:inherit tabbar-default-face))))
       (tabbar-unselected-face               ((t (:inherit tabbar-default-face))))
       (table-cell                           ((t nil)))
       (tex-math                             ((t (:foreground ,cyan))))
       (undo-tree-visualizer-active-branch-face ((t (:weight bold))))
       (undo-tree-visualizer-current-face    ((t (:foreground ,red))))
       (undo-tree-visualizer-register-face   ((t (:foreground ,yellow))))
       (vim:lazy-highlight                   ((t (:inherit lazy-highlight))))
       (vim:search                           ((t (:background ,light-cyan-green-background))))
       (vim:substitute                       ((t (:underline ,magenta))))
       (warning                              ((t (:foreground ,orange))))
       (whitespace-line                      ((t (:underline ,red))))
       (whitespace-tab                       ((t (:underline ,green))))
       (yas-field-highlight-face             ((t (:background ,light-cyan-green-background))))))
    (setf frame-background-mode        mode
          *color-theme-solarized-type* mode
          org-drill-new-count-color    blue
          org-drill-done-count-color   green
          org-drill-failed-count-color magenta
          org-drill-mature-count-color orange
          ansi-color-names-vector (vector base0 red green yellow blue magenta cyan base01)
          ansi-color-map (ansi-color-make-color-map)
          fci-rule-color base0)
    (run-hooks 'color-theme-solarized+-theme-changed-hook)))

(defparameter color-theme-solarized+-theme-changed-hook '()
  "Hook to run when theme changes")

(defun color-theme-solarized+-dark ()
  (interactive)
  (color-theme-solarized+ 'dark))

(defun color-theme-solarized+-light ()
  (interactive)
  (color-theme-solarized+ 'light))

(defparameter *color-theme-solarized-type* nil
  "Type of current solarized color theme, either 'light or 'dark.")

(defun solarized+-toggle ()
  "Toggle type of solarized color theme."
  (interactive)
  (case *color-theme-solarized-type*
    (light (color-theme-solarized+-dark))
    (dark (color-theme-solarized+-light))))

(defun solarized+-reapply ()
  "Apply currently selected solarized+ theme once again.

Useful for applying changes made to color theme definition."
  (interactive)
  (case *color-theme-solarized-type*
    (dark (color-theme-solarized+-dark))
    (light (color-theme-solarized+-light))))

(add-to-list 'color-themes
             '(color-theme-solarized+-light
               "Solarized Light"
               "Ethan Schoonover & Greg Pfeil <greg@technomadic.org>"))
(add-to-list 'color-themes
             '(color-theme-solarized+-dark
               "Solarized Dark"
               "Ethan Schoonover & Greg Pfeil <greg@technomadic.org>"))

(provide 'solarized+)

;; Local Variables:
;; End:

;; solarized+.el ends here
