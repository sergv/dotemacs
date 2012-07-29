;;; Author: Ethan Schoonover, Solarized; Greg Pfeil, Emacs adaptation
;;; URL: http://ethanschoonover.com/solarized

;;; This file is not (YET) part of GNU Emacs.

;;; # Usage

;;; 1. Install the color-theme package
;;;   (http://www.emacswiki.org/cgi-bin/wiki/ColorTheme)
;;; 2. Load this file
;;; 3. M-x color-theme-solarized-[dark|light]

(require 'color-theme)

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
         (red     "#dc322f")
         (orange  "#cb4b16")
         (yellow  "#b58900")
         ;; (green   "#719f00")
         (green   "#859900")
         (cyan    "#2aa198")
         (blue    "#268bd2")
         (violet  "#6c71c4")
         (magenta "#d33682")

         ;; highlight backgrounds
         (light-orange-background "#652700")
         (light-yellow-background light-orange-background)
         (light-green-background "#004800")
         (light-cyan-background "#0059e9")
         (light-blue-background "#0000ea")
         (light-violet-background "#dd00eb")
         (light-cyan-green-background "#006152")
         (light-pink-background "#e70000")
         (light-red-background "#910000"))
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
      (setf base03 "black"
            base02 "black"
            base2 "white"
            base3 "white"
            red "red"
            orange "red"
            yellow "yellow"
            green "green"
            cyan "cyan"
            blue "blue"
            violet "magenta"
            magenta "magenta")
      (if (eq 'light mode)
        (setf base01 "black"
              base00 "black"
              base0 "black"
              base1 "black"
              light-orange-background "white"
              light-yellow-background "white"
              light-green-background "white"
              light-cyan-background "white"
              light-blue-background "white"
              light-violet-background "white"
              light-cyan-green-background "white"
              light-pink-background "white"
              light-red-background "white")
        (setf base01 "white"
              base00 "white"
              base0 "white"
              base1 "white"
              light-orange-background "black"
              light-yellow-background "black"
              light-green-background "black"
              light-cyan-background "black"
              light-blue-background "black"
              light-violet-background "black"
              light-cyan-green-background "black"
              light-pink-background "black"
              light-red-background "black")))
    (color-theme-install
     `(color-theme-solarized+
       ((foreground-color . ,base0)
        (background-color . ,base03)
        (background-mode . ,mode)
        (cursor-color . ,base0))
       ;; basic
       (default                      ((t (:foreground ,base0))))
       (cursor                       ((t (:foreground ,base03 :background ,base0))))
       (escape-glyph-face            ((t (:foreground ,red))))
       (fringe                       ((t (:foreground ,base01 :background ,base02))))
       (highlight                    ((t (:background ,base02))))
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

       (font-lock-builtin-face       ((t (:foreground ,yellow))))
       (font-lock-constant-face      ((t (:foreground ,cyan))))
       (font-lock-doc-face           ((t (:foreground ,yellow))))
       (font-lock-function-name-face ((t (:foreground ,orange))))
       (font-lock-keyword-face       ((t (:foreground ,yellow))))
       (font-lock-negation-char-face ((t (:foreground ,magenta))))
       (font-lock-preprocessor-face  ((t (:foreground ,orange))))
       (font-lock-string-face        ((t (:foreground ,green))))
       (font-lock-type-face          ((t (:foreground ,cyan))))
       (font-lock-variable-name-face ((t (:foreground ,orange))))

       (ansi-lisp-constant-face      ((t (:inherit font-lock-constant-face))))
       (ansi-lisp-keyword-face       ((t (:inherit font-lock-keyword-face))))
       (ansi-lisp-warning-face       ((t (:inherit font-lock-warning-face))))
       (ansi-lisp-global-variable-face ((t (:inherit font-lock-variable-name-face))))
       (ansi-lisp-declaration-face   ((t (:foreground ,green))))
       (ansi-lisp-type-face          ((t (:inherit font-lock-type-face))))
       (ansi-lisp-expression-face    ((t (:inherit font-lock-builtin-face))))
       (ansi-lisp-special-form-face  ((t (:inherit font-lock-builtin-face))))
       (ansi-lisp-macro-face         ((t (:inherit font-lock-builtin-face))))
       (ansi-lisp-generic-function-face ((t (:inherit font-lock-builtin-face))))
       (ansi-lisp-function-face      ((t (:inherit font-lock-builtin-face))))

       (ansi-lisp-predicate-face     ((t (:foreground ,blue))))
       (ansi-lisp-mutating-op-face   ((t (:foreground ,blue))))

       (ansi-lisp-format-directive-face ((t (:inherit font-lock-negation-char-face))))
       (ansi-lisp-symbols-without-home-package-face ((t (:inherit ansi-lisp-constant-face))))
       (ansi-lisp-defined-name-face  ((t (:inherit font-lock-function-name-face))))
       (ansi-lisp-defined-data-name-face ((t (:inherit font-lock-function-name-face)
                                             ;; (:foreground ,(if (eq 'light mode) orange yellow))
                                             )))
       (ansi-lisp-doc-face           ((t (:inherit font-lock-doc-face))))
       (ansi-lisp-exported-symbols-face ((t (:foreground ,cyan))))

       ;; (ansi-lisp-loop-keyword-face  ((t (:foreground ,cyan))))

       ;; slime & swank
       (sldb-detailed-frame-line-face        ((t nil)))
       (sldb-frame-label-face                ((t (:foreground ,base2))))
       (sldb-restartable-frame-line-face     ((t (:foreground ,cyan))))
       (sldb-section-face                    ((t (:foreground ,base2))))
       (slime-error-face                     ((t (:underline ,red :bold t))))
       (slime-note-face                      ((t (:underline ,green :bold t))))
       (slime-reader-conditional-face        ((t nil ;; (:foreground ,violet)
                                                 )))
       (slime-repl-input-face                ((t (:bold t))))
       (slime-repl-output-face               ((t (:inherit default))))
       (slime-repl-prompt-face               ((t (:foreground ,blue))))
       (slime-repl-result-face               ((t (:inherit default))))
       (slime-style-warning-face             ((t (:underline ,yellow :bold t))))
       (slime-warning-face                   ((t (:underline ,orange :bold t))))

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
       (org-clock-overlay            ((t (:background ,light-yellow-background))))
       (org-date                     ((t (:foreground ,violet :underline t))))
       (org-drawer                   ((t (:foreground ,blue))))
       (org-footnote                 ((t (:foreground ,violet :underline t))))
       (org-ellipsis                 ((t (:foreground ,yellow :underline t))))
       (org-mode-line-clock-overrun  ((t (:background ,light-red-background))))
       (org-sexp-date                ((t (:foreground ,magenta))))
       (org-table                    ((t (:foreground ,violet))))
       (org-time-grid                ((t (:foreground ,orange))))
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
       (ediff-current-diff-A                 ((t (:background ,light-yellow-background))))
       (ediff-current-diff-B                 ((t (:background ,light-yellow-background))))
       (ediff-current-diff-C                 ((t (:background ,light-yellow-background))))
       (ediff-current-diff-Ancestor          ((t (:background ,light-violet-background))))
       (ediff-fine-diff-Ancestor             ((t (:background ,light-cyan-background))))
       (ediff-fine-diff-A                    ((t (:background ,light-cyan-background))))
       (ediff-fine-diff-B                    ((t (:background ,light-cyan-background))))
       (ediff-fine-diff-C                    ((t (:background ,light-cyan-background))))

       ;; eshell
       (eshell-ls-archive                    ((t (:foreground ,violet))))
       (eshell-ls-directory                  ((t (:foreground ,blue))))
       (eshell-ls-executable                 ((t (:foreground ,green))))
       (eshell-ls-missing                    ((t (:foreground ,red))))
       (eshell-ls-special                    ((t (:foreground ,magenta))))
       (eshell-ls-symlink                    ((t (:foreground ,cyan))))
       (eshell-prompt                        ((t (:foreground ,magenta))))

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

       ;; other faces
       (completions-common-part              ((t (:inherit match))))
       (csv-separator-face                   ((t (:foreground ,magenta))))
       (dired-directory                      ((t (:foreground ,blue))))
       (dired-warning                        ((t (:inherit warning :bold t))))
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
       (icicle-Completions-instruction-1     ((t (:foreground ,blue))))
       (icicle-Completions-instruction-2     ((t (:foreground ,magenta))))
       (icicle-common-match-highlight-Completions ((t (:underline t))))
       (icicle-complete-input                ((t (:foreground ,green))))
       (icicle-completion                    ((t (:foreground ,red))))
       (icicle-current-candidate-highlight   ((t (:inherit lazy-highlight))))
       (icicle-extra-candidate               ((t (:underline "black"))))
       (icicle-historical-candidate          ((t (:underline ,green))))
       (icicle-match-highlight-Completions   ((t (:background ,base02))))
       (icicle-mode-line-help                ((t (:foreground ,blue))))
       (icicle-multi-command-completion      ((t (:weight bold))))
       (icicle-mustmatch-completion          ((t (:weight bold))))
       (icicle-saved-candidate               ((t (:slant italic))))
       (icicle-search-context-level-1        ((t (:underline ,red))))
       (icicle-search-context-level-2        ((t (:underline ,orange))))
       (icicle-search-context-level-3        ((t (:underline ,yellow))))
       (icicle-search-context-level-4        ((t (:underline ,green))))
       (icicle-search-context-level-5        ((t (:underline ,cyan))))
       (icicle-search-context-level-6        ((t (:underline ,blue))))
       (icicle-search-context-level-7        ((t (:underline ,violet))))
       (icicle-search-context-level-8        ((t (:underline ,base01))))
       (icicle-search-current-input          ((t (:underline ,magenta))))
       (icicle-search-main-regexp-others     ((t (:background ,base02))))
       (icicle-whitespace-highlight          ((t (:background ,magenta))))
       (ido-indicator                        ((t (:foreground ,blue :width condensed))))
       (ido-only-match                       ((t (:foreground ,cyan))))
       (ido-subdir                           ((t (:foreground ,orange))))
       (imaxima-latex-error-face             ((t (:inherit error))))
       (isearch                              ((t (:inherit lazy-highlight))))
       (ispell-highlight-face                ((t (:inherit flyspell-incorrect))))
       (italic                               ((t (:underline t :italic t))))
       (lazy-highlight                       ((t (:background ,light-cyan-green-background))))
       (link                                 ((t (:foreground ,violet :underline t))))
       (match                                ((t (:background ,light-cyan-green-background))))
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
       (yas/field-highlight-face             ((t (:background ,light-cyan-green-background))))))
    (setf imaxima-equation-color base0
          imaxima-label-color cyan
          frame-background-mode mode
          *color-theme-solarized-type* mode)))

;; version 1.0
;; (defun color-theme-solarized+ (mode)
;;   "Color theme by Ethan Schoonover, created 2011-03-24.
;; Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized.
;; Futher modified by Sergey Vinokurov."
;;   (interactive "Slight or dark? ")
;;   (let* ((base03  "#002b36")
;;          (base02  "#073642")
;;          ;; optional emphasized content
;;          (base01  "#586e75")
;;          ;; body text/default code/primary content
;;          (base00  "#657b83")
;;          ;; ?
;;          (base0   "#839496")
;;          ;; comments/secondary content
;;          (base1   "#93a1a1")
;;          ;; background highlights
;;          (base2   "#eee8d5")
;;          ;; background
;;          (base3   "#fdf6e3")
;;          (red     "#dc322f")
;;          (orange  "#cb4b16")
;;          (yellow  "#b58900")
;;          (green   "#859900")
;;          (cyan    "#2aa198")
;;          (blue    "#268bd2")
;;          (violet  "#6c71c4")
;;          (magenta "#d33682")
;;
;;          ;; highlight backgrounds
;;          (light-orange-background "#652700")
;;          (light-yellow-background light-orange-background)
;;          (light-green-background "#004800")
;;          (light-cyan-background "#0059e9")
;;          (light-blue-background "#0000ea")
;;          (light-violet-background "#dd00eb")
;;          (light-cyan-green-background "#006152")
;;          (light-pink-background "#e70000")
;;          (light-red-background "#910000"))
;;     (when (eq 'light mode)
;;       (rotatef base03 base3)
;;       (rotatef base02 base2)
;;       (rotatef base01 base1)
;;       (rotatef base00 base0)
;;
;;       (setf light-orange-background     "#f9ea7c"
;;             light-yellow-background     "#fbee00"
;;             light-green-background      "#e2f500"
;;             light-cyan-background       "#98f9eb"
;;             light-blue-background       "#ceeef1"
;;             light-violet-background     "#f2e3fd"
;;             light-cyan-green-background "#a2fe8e"
;;             light-pink-background       "#fddfff"
;;             light-red-background        light-pink-background))
;;
;;     (when (= (display-color-cells) 8)
;;       (setf base03 "black"
;;             base02 "black"
;;             base2 "white"
;;             base3 "white"
;;             red "red"
;;             orange "red"
;;             yellow "yellow"
;;             green "green"
;;             cyan "cyan"
;;             blue "blue"
;;             violet "magenta"
;;             magenta "magenta")
;;       (if (eq 'light mode)
;;         (setf base01 "black"
;;               base00 "black"
;;               base0 "black"
;;               base1 "black"
;;               light-orange-background "white"
;;               light-yellow-background "white"
;;               light-green-background "white"
;;               light-cyan-background "white"
;;               light-blue-background "white"
;;               light-violet-background "white"
;;               light-cyan-green-background "white"
;;               light-pink-background "white"
;;               light-red-background "white")
;;         (setf base01 "white"
;;               base00 "white"
;;               base0 "white"
;;               base1 "white"
;;               light-orange-background "black"
;;               light-yellow-background "black"
;;               light-green-background "black"
;;               light-cyan-background "black"
;;               light-blue-background "black"
;;               light-violet-background "black"
;;               light-cyan-green-background "black"
;;               light-pink-background "black"
;;               light-red-background "black")))
;;     (color-theme-install
;;      `(color-theme-solarized+
;;        ((foreground-color . ,base0)
;;         (background-color . ,base03)
;;         (background-mode . ,mode)
;;         (cursor-color . ,base0))
;;        ;; basic
;;        (default                      ((t (:foreground ,base0))))
;;        (cursor                       ((t (:foreground ,base03 :background ,base0))))
;;        (escape-glyph-face            ((t (:foreground ,red))))
;;        (fringe                       ((t (:foreground ,base01 :background ,base02))))
;;        (highlight                    ((t (:background ,base02))))
;;        (menu                         ((t (:foreground ,base0 :background ,base02))))
;;        (minibuffer-prompt            ((t (:foreground ,blue))))
;;        (mode-line                    ((t (:foreground ,base1 :background ,base02
;;                                           :box (:line-width 1 :color ,base1)))))
;;        (mode-line-buffer-id          ((t (:foreground ,base1))))
;;        (mode-line-inactive           ((t (:foreground ,base0  :background ,base02
;;                                           :box (:line-width 1 :color ,base02)))))
;;        (region                       ((t (:background ,base02 :underline t))))
;;        (secondary-selection          ((t (:background ,base02))))
;;        (trailing-whitespace          ((t (:background ,magenta))))
;;        (vertical-border              ((t (:foreground ,base0))))
;;        ;; compilation
;;        (compilation-info             ((t (:foreground ,green :bold t))))
;;        (compilation-error            ((t (:foreground ,red :bold t))))
;;        (compilation-warning          ((t (:foreground ,orange :bold t))))
;;        ;; customize
;;        (custom-button                ((t (:background ,base02 :box (:line-width 2 :style released-button)))))
;;        (custom-button-mouse          ((t (:inherit custom-button :foreground ,base1))))
;;        (custom-button-pressed        ((t (:inherit custom-button-mouse
;;                                           :box (:line-width 2 :style pressed-button)))))
;;        (custom-comment-tag           ((t (:background ,base02))))
;;        (custom-comment-tag           ((t (:background ,base02))))
;;        (custom-documentation         ((t (:inherit default))))
;;        (custom-group-tag             ((t (:foreground ,orange :bold t))))
;;        (custom-link                  ((t (:foreground ,violet))))
;;        (custom-state                 ((t (:foreground ,green))))
;;        (custom-variable-tag          ((t (:foreground ,orange :bold t))))
;;        ;; emacs-wiki
;;        (emacs-wiki-bad-link-face     ((t (:foreground ,red :underline t))))
;;        (emacs-wiki-link-face         ((t (:foreground ,blue :underline t))))
;;        (emacs-wiki-verbatim-face     ((t (:foreground ,base00 :underline t))))
;;        ;; font-lock
;;        (font-lock-comment-face       ((t (:foreground ,base01 ;; :italic t
;;                                                       ))))
;;        (font-lock-warning-face       ((t (:inherit warning))))
;;
;;        (font-lock-builtin-face       ((t (:foreground ,green))))
;;        (font-lock-constant-face      ((t (:foreground ,cyan))))
;;        (font-lock-doc-face           ((t (:foreground ,violet))))
;;        (font-lock-function-name-face ((t (:foreground ,blue))))
;;        (font-lock-keyword-face       ((t (:foreground ,green))))
;;        (font-lock-negation-char-face ((t (:foreground ,cyan))))
;;        (font-lock-preprocessor-face  ((t (:foreground ,orange))))
;;        (font-lock-string-face        ((t (:foreground ,yellow))))
;;        (font-lock-type-face          ((t (:foreground ,yellow))))
;;        (font-lock-variable-name-face ((t (:foreground ,blue))))
;;
;;        (ansi-lisp-defined-data-name-face ((t (:foreground ,(if (eq 'light mode) orange yellow)))))
;;        (ansi-lisp-declaration-face   ((t (:foreground ,cyan))))
;;        (ansi-lisp-type-face          ((t (:foreground ,cyan))))
;;        ;; (ansi-lisp-loop-keyword-face  ((t (:foreground ,cyan))))
;;
;;        ;; ;; standard color scheme
;;        ;; (font-lock-comment-face       ((t (:foreground ,base01 :italic t))))
;;        ;; (font-lock-builtin-face       ((t (:foreground ,green))))
;;        ;; (font-lock-constant-face      ((t (:foreground ,orange))))
;;        ;; (font-lock-doc-face           ((t (:foreground ,violet))))
;;        ;; (font-lock-function-name-face ((t (:foreground ,blue))))
;;        ;; (font-lock-keyword-face       ((t (:foreground ,green))))
;;        ;; (font-lock-string-face        ((t (:foreground ,orange))))
;;        ;; (font-lock-type-face          ((t (:foreground ,yellow))))
;;        ;; (font-lock-variable-name-face ((t (:foreground ,cyan))))
;;
;;        ;; slime & swank
;;        (sldb-detailed-frame-line-face        ((t nil)))
;;        (sldb-frame-label-face                ((t (:foreground ,base2))))
;;        (sldb-restartable-frame-line-face     ((t (:foreground ,cyan))))
;;        (sldb-section-face                    ((t (:foreground ,base2))))
;;        (slime-error-face                     ((t (:underline ,red))))
;;        (slime-note-face                      ((t (:underline ,green))))
;;        (slime-reader-conditional-face        ((t (:foreground ,violet))))
;;        (slime-repl-input-face                ((t (:bold t))))
;;        (slime-repl-output-face               ((t (:inherit default))))
;;        (slime-repl-prompt-face               ((t (:foreground ,blue))))
;;        (slime-repl-result-face               ((t (:inherit default))))
;;        (slime-style-warning-face             ((t (:underline ,yellow))))
;;        (slime-warning-face                   ((t (:underline ,orange))))
;;
;;        ;; info
;;        (info-xref                    ((t (:foreground ,blue :underline t))))
;;        (info-xref-visited            ((t (:inherit info-xref :foreground ,magenta))))
;;        ;; org
;;        (org-hide                     ((t (:foreground ,base03))))
;;        (org-todo                     ((t (:foreground ,red :bold t))))
;;        (org-done                     ((t (:foreground ,green :bold t))))
;;        (org-cancelled                ((t (:foreground ,violet :bold t))))
;;
;;        (org-agenda-date              ((t (:foreground ,blue))))
;;        (org-agenda-date-today        ((t (:foreground ,blue :bold t))))
;;        (org-agenda-date-weekend      ((t (:foreground ,blue :bold t))))
;;        (org-agenda-restriction-lock  ((t (:background ,light-yellow-background))))
;;        (org-clock-overlay            ((t (:background ,light-yellow-background))))
;;        (org-date                     ((t (:foreground ,violet :underline t))))
;;        (org-drawer                   ((t (:foreground ,blue))))
;;        (org-footnote                 ((t (:foreground ,violet :underline t))))
;;        (org-ellipsis                 ((t (:foreground ,yellow :underline t))))
;;        (org-mode-line-clock-overrun  ((t (:background ,light-red-background))))
;;        (org-sexp-date                ((t (:foreground ,violet))))
;;        (org-table                    ((t (:foreground ,blue))))
;;        (org-time-grid                ((t (:foreground ,orange))))
;;        ;; outlines, inherited by org mode too
;;        (outline-1                    ((t (:foreground ,blue))))
;;        (outline-2                    ((t (:foreground ,cyan))))
;;        (outline-3                    ((t (:foreground ,green))))
;;        (outline-4                    ((t (:foreground ,yellow))))
;;        (outline-5                    ((t (:foreground ,orange))))
;;        (outline-6                    ((t (:foreground ,red))))
;;        (outline-7                    ((t (:foreground ,magenta))))
;;        (outline-8                    ((t (:foreground ,violet))))
;;
;;        ;; sunrise commander
;;        (sr-active-path-face          ((t (:inherit default :bold t))))
;;        (sr-passive-path-face         ((t (:inherit default))))
;;        (sr-editing-path-face         ((t (:inherit sr-active-path-face :underline t))))
;;        (sr-clex-hotchar-face         ((t (:foreground ,red :bold t))))
;;        (sr-broken-link-face          ((t (:foreground ,red))))
;;        (sr-symlink-face              ((t (:foreground ,cyan))))
;;        (sr-symlink-directory-face    ((t (:foreground ,cyan :bold t))))
;;
;;        (sr-encrypted-face            ((t (:foreground ,orange))))
;;        (sr-compressed-face           ((t (:foreground ,violet))))
;;        (sr-packaged-face             ((t (:foreground ,violet))))
;;        (sr-xml-face                  ((t (:inherit default))))
;;        (sr-log-face                  ((t (:inherit default))))
;;        (sr-html-face                 ((t (:inherit default))))
;;
;;        (sr-alt-marked-file-face      ((t (:foreground ,magenta :underline t))))
;;        (sr-alt-marked-dir-face       ((t (:foreground ,magenta :bold t :underline t))))
;;        (sr-marked-file-face          ((t (:foreground ,magenta))))
;;        (sr-marked-dir-face           ((t (:foreground ,magenta :bold t))))
;;
;;        ;; diff & ediff
;;        (diff-added                           ((t (:foreground ,green))))
;;        (diff-changed                         ((t (:foreground ,yellow))))
;;        (diff-file-header                     ((t (:foreground ,violet))))
;;        (diff-header                          ((t (:foreground ,blue))))
;;        (diff-indicator-removed               ((t (:inherit diff-removed))))
;;        (diff-removed                         ((t (:foreground ,red))))
;;        (ediff-current-diff-A                 ((t (:background ,light-yellow-background))))
;;        (ediff-current-diff-B                 ((t (:background ,light-yellow-background))))
;;        (ediff-current-diff-C                 ((t (:background ,light-yellow-background))))
;;        (ediff-current-diff-Ancestor          ((t (:background ,light-violet-background))))
;;        (ediff-fine-diff-Ancestor             ((t (:background ,light-cyan-background))))
;;        (ediff-fine-diff-A                    ((t (:background ,light-cyan-background))))
;;        (ediff-fine-diff-B                    ((t (:background ,light-cyan-background))))
;;        (ediff-fine-diff-C                    ((t (:background ,light-cyan-background))))
;;
;;        ;; eshell
;;        (eshell-ls-archive                    ((t (:foreground ,violet))))
;;        (eshell-ls-directory                  ((t (:foreground ,blue))))
;;        (eshell-ls-executable                 ((t (:foreground ,green))))
;;        (eshell-ls-missing                    ((t (:foreground ,red))))
;;        (eshell-ls-special                    ((t (:foreground ,magenta))))
;;        (eshell-ls-symlink                    ((t (:foreground ,cyan))))
;;        (eshell-prompt                        ((t (:foreground ,magenta))))
;;
;;        ;; rainbow delimiters
;;        (rainbow-delimiters-depth-1-face      ((t (:foreground ,base0))))
;;        (rainbow-delimiters-depth-2-face      ((t (:foreground ,red))))
;;        (rainbow-delimiters-depth-3-face      ((t (:foreground ,orange))))
;;        (rainbow-delimiters-depth-4-face      ((t (:foreground ,yellow))))
;;        (rainbow-delimiters-depth-5-face      ((t (:foreground ,green))))
;;        (rainbow-delimiters-depth-6-face      ((t (:foreground ,cyan))))
;;        (rainbow-delimiters-depth-7-face      ((t (:foreground ,blue))))
;;        (rainbow-delimiters-depth-8-face      ((t (:foreground ,violet))))
;;        (rainbow-delimiters-depth-9-face      ((t (:foreground ,magenta))))
;;        (rainbow-delimiters-unmatched-face    ((t (:background ,magenta))))
;;
;;        ;; nxhtml
;;        (mumamo-background-chunk-major        ((t (:background ,base03))))
;;
;;        (mumamo-background-chunk-submode1     ((t (:box (:line-width 1 :color ,light-cyan-green-background)))))
;;        (mumamo-background-chunk-submode2     ((t (:box (:line-width 1 :color ,light-green-background)))))
;;        (mumamo-background-chunk-submode3     ((t (:box (:line-width 1 :color ,light-yellow-background)))))
;;        (mumamo-background-chunk-submode4     ((t (:box (:line-width 1 :color ,light-cyan-background)))))
;;        (nxml-glyph                           ((t (:foreground ,base0 :background ,base03
;;                                                   :box (:line-width 1 :color ,base0)))))
;;        ;; emms
;;        (emms-playlist-selected-face          ((t (:foreground ,blue))))
;;        (emms-playlist-track-face             ((t (:inherit default))))
;;
;;        ;; other faces
;;        (completions-common-part              ((t (:inherit match))))
;;        (csv-separator-face                   ((t (:foreground ,cyan))))
;;        (flyspell-duplicate                   ((t (:bold t :foreground ,green :underline t))))
;;        (flyspell-incorrect                   ((t (:bold t :foreground ,orange :underline t))))
;;        (font-latex-bold-face                 ((t (:inherit bold))))
;;        (font-latex-doctex-documentation-face ((t (:inherit font-lock-doc-face))))
;;        (font-latex-doctex-preprocessor-face  ((t (:inherit font-lock-preprocessor-face))))
;;        (font-latex-italic-face               ((t (:inherit italic))))
;;        (font-latex-math-face                 ((t (:foreground ,cyan))))
;;        (font-latex-sectioning-0-face         ((t (:inherit font-latex-sectioning-1-face :height 1.05))))
;;        (font-latex-sectioning-1-face         ((t (:inherit font-latex-sectioning-2-face :height 1.05))))
;;        (font-latex-sectioning-2-face         ((t (:inherit font-latex-sectioning-3-face :height 1.05))))
;;        (font-latex-sectioning-3-face         ((t (:inherit font-latex-sectioning-4-face :height 1.05))))
;;        (font-latex-sectioning-4-face         ((t (:inherit font-latex-sectioning-5-face :height 1.05))))
;;        (font-latex-sectioning-5-face         ((t (:foreground ,magenta))))
;;        (font-latex-sedate-face               ((t (:foreground ,yellow))))
;;        (font-latex-slide-title-face          ((t (:foreground ,magenta))))
;;        (font-latex-string-face               ((t (:inherit font-lock-string-face))))
;;        (font-latex-verbatim-face             ((t (:foreground ,violet))))
;;        (font-latex-warning-face              ((t (:foreground ,violet))))
;;        (header-line                          ((t (:foreground ,base0 :background ,base02))))
;;        (help-argument-name                   ((t (:inherit default))))
;;        (hexl-address-region                  ((t (:inherit header-line))))
;;        (hexl-ascii-region                    ((t (:background ,base03))))
;;        (icicle-Completions-instruction-1     ((t (:foreground ,blue))))
;;        (icicle-Completions-instruction-2     ((t (:foreground ,magenta))))
;;        (icicle-common-match-highlight-Completions ((t (:underline t))))
;;        (icicle-complete-input                ((t (:foreground ,green))))
;;        (icicle-completion                    ((t (:foreground ,red))))
;;        (icicle-current-candidate-highlight   ((t (:inherit lazy-highlight))))
;;        (icicle-extra-candidate               ((t (:underline "black"))))
;;        (icicle-historical-candidate          ((t (:underline ,green))))
;;        (icicle-match-highlight-Completions   ((t (:background ,base02))))
;;        (icicle-mode-line-help                ((t (:foreground ,blue))))
;;        (icicle-multi-command-completion      ((t (:weight bold))))
;;        (icicle-mustmatch-completion          ((t (:weight bold))))
;;        (icicle-saved-candidate               ((t (:slant italic))))
;;        (icicle-search-context-level-1        ((t (:underline ,red))))
;;        (icicle-search-context-level-2        ((t (:underline ,orange))))
;;        (icicle-search-context-level-3        ((t (:underline ,yellow))))
;;        (icicle-search-context-level-4        ((t (:underline ,green))))
;;        (icicle-search-context-level-5        ((t (:underline ,cyan))))
;;        (icicle-search-context-level-6        ((t (:underline ,blue))))
;;        (icicle-search-context-level-7        ((t (:underline ,violet))))
;;        (icicle-search-context-level-8        ((t (:underline ,base01))))
;;        (icicle-search-current-input          ((t (:underline ,magenta))))
;;        (icicle-search-main-regexp-others     ((t (:background ,base02))))
;;        (icicle-whitespace-highlight          ((t (:background ,magenta))))
;;        (ido-indicator                        ((t (:foreground ,blue :width condensed))))
;;        (ido-only-match                       ((t (:foreground ,cyan))))
;;        (ido-subdir                           ((t (:foreground ,orange))))
;;        (imaxima-latex-error-face             ((t (:inherit error))))
;;        (isearch                              ((t (:inherit lazy-highlight))))
;;        (ispell-highlight-face                ((t (:inherit flyspell-incorrect))))
;;        (italic                               ((t (:underline t :italic t))))
;;        (lazy-highlight                       ((t (:background ,light-cyan-green-background))))
;;        (link                                 ((t (:foreground ,violet :underline t))))
;;        (match                                ((t (:background ,light-cyan-green-background))))
;;        (minibuffer-prompt                    ((t (:foreground ,violet))))
;;        (navigation-node-face                 ((t (:foreground ,magenta))))
;;        (paren-face-no-match                  ((t (:underline ,yellow))))
;;        (py-number-face                       ((t (:foreground ,cyan))))
;;        (py-variable-name-face                ((t (:inherit font-lock-variable-name-face))))
;;        (py-XXX-tag-face                      ((t (:foreground ,red))))
;;        (quack-pltish-class-defn-face         ((t (:foreground ,violet))))
;;        (quack-pltish-defn-face               ((t (:foreground ,blue))))
;;        (quack-pltish-keyword-face            ((t (:foreground ,base01))))
;;        (quack-pltish-module-defn-face        ((t (:foreground ,violet))))
;;        (quack-threesemi-semi-face            ((t (:foreground ,base1))))
;;        (quack-threesemi-text-face            ((t (:foreground ,base1))))
;;        (query-replace                        ((t (:background ,light-pink-background))))
;;        (sh-heredoc                           ((t (:foreground ,cyan))))
;;        (sh-quoted-exec                       ((t (:foreground ,magenta))))
;;        (show-paren-match                     ((t (:underline ,magenta))))
;;        (show-paren-match-face                ((t (:inherit show-paren-match))))
;;        (show-paren-mismatch-face             ((t (:inherit rainbow-delimiters-unmatched-face))))
;;        (tabbar-button-face                   ((t (:inherit tabbar-default-face
;;                                                   :box (:line-width 2
;;                                                         :color "white"
;;                                                         :style released-button)
;;                                                   :foreground "dark red"))))
;;        (tabbar-default-face                  ((t (:inherit variable-pitch
;;                                                   :height 0.8
;;                                                   :foreground ,base0
;;                                                   :background ,base02))))
;;        (tabbar-selected-face                 ((t (:inherit tabbar-default-face
;;                                                   :bold t ;; :foreground ,magenta
;;                                                   ))))
;;        (tabbar-separator-face                ((t (:inherit tabbar-default-face))))
;;        (tabbar-unselected-face               ((t (:inherit tabbar-default-face))))
;;        (table-cell                           ((t nil)))
;;        (tex-math                             ((t (:foreground ,cyan))))
;;        (undo-tree-visualizer-active-branch-face ((t (:weight bold))))
;;        (undo-tree-visualizer-current-face    ((t (:foreground ,red))))
;;        (undo-tree-visualizer-register-face   ((t (:foreground ,yellow))))
;;        (vim:lazy-highlight                   ((t (:inherit lazy-highlight))))
;;        (vim:search                           ((t (:background ,light-cyan-green-background))))
;;        (vim:substitute                       ((t (:underline ,magenta))))
;;        (warning                              ((t (:foreground ,orange))))
;;        (whitespace-line                      ((t (:underline ,red))))
;;        (whitespace-tab                       ((t (:underline ,green))))
;;        (yas/field-highlight-face             ((t (:background ,light-cyan-green-background))))
;;        ))
;;     (setf imaxima-equation-color base0
;;           imaxima-label-color cyan)))

(defun color-theme-solarized+-dark ()
  (interactive)
  (color-theme-solarized+ 'dark))

(defun color-theme-solarized+-light ()
  (interactive)
  (color-theme-solarized+ 'light))

(defvar *color-theme-solarized-type* nil
  "Type of current solarized color theme, either 'light or 'dark.")

(defun solarized+-toggle ()
  "Toggle type of solarized color theme."
  (interactive)
  (case *color-theme-solarized-type*
    (light (color-theme-solarized+-dark))
    (dark (color-theme-solarized+-light))))

(add-to-list 'color-themes
             '(color-theme-solarized+-light
               "Solarized Light"
               "Ethan Schoonover & Greg Pfeil <greg@technomadic.org>"))
(add-to-list 'color-themes
             '(color-theme-solarized+-dark
               "Solarized Dark"
               "Ethan Schoonover & Greg Pfeil <greg@technomadic.org>"))


;; (color-theme-initialize)
;;
;; (defun my-theme-set-default () ; Set the first row
;;   (interactive)
;;   (setq theme-current color-themes)
;;   (funcall (caar theme-current)))
;;
;; (defvar theme-current color-themes)
;;
;; (defun my-describe-theme () ; Show the current theme
;;   (interactive)
;;   (message "%s" (caar theme-current)))
;;
;; ;; Set the next theme (fixed by Chris Webber - tanks)
;; (defun my-theme-cycle ()
;;   (interactive)
;;   (setq theme-current (cdr theme-current))
;;   (if (null theme-current)
;;     (setq theme-current color-themes))
;;   (funcall (caar theme-current))
;;   (message "%S" (caar theme-current)))
;;
;; (global-set-key (kbd "C-<f1>") 'my-theme-cycle)

(provide 'solarized+)
