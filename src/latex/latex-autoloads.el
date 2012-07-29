;;; latex-autoloads.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 28 August 2011
;; Keywords:
;; Requirements:
;; Status:


;; (require 'tex-site)
;; (require 'preview-latex)

(add-to-list 'load-path (concat +emacs-standalone-path+ "/auctex"))
(add-to-list 'load-path (concat +emacs-standalone-path+ "/auctex/auctex"))
(add-to-list 'load-path (concat +emacs-standalone-path+ "/auctex/auctex/style"))


(require 'tex-site)

(setq auto-mode-alist
      (remove '("\\.[tT]e[xX]\\'" . tex-mode) auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.[tT][eE][xX]\\'" . LaTeX-mode))


(autoload 'latex-setup "latex-setup")

(autoload 'latex-compile
  "latex-compilation"
  "Start compilation of LaTeX file."
  t)

(add-hook 'latex-mode-hook #'latex-setup)
(add-hook 'LaTeX-mode-hook #'latex-setup)

(setf *elisp-do-not-move-files*
      (append *elisp-do-not-move-files*
              '("alltt.el"
                "alphanum.el"
                "amsart.el"
                "amsbook.el"
                "amsbsy.el"
                "amsmath.el"
                "amsopn.el"
                "amstex.el"
                "amstext.el"
                "amsthm.el"
                "article.el"
                "auctex.el"
                "austrian.el"
                "babel.el"
                "beamer.el"
                "bib-cite.el"
                "book.el"
                "booktabs.el"
                "bulgarian.el"
                "captcont.el"
                "CJK.el"
                "CJKutf8.el"
                "comment.el"
                "context.el"
                "context-en.el"
                "context-nl.el"
                "csquotes.el"
                "czech.el"
                "danish.el"
                "dinbrief.el"
                "dk-bib.el"
                "dk.el"
                "doc.el"
                "dutch.el"
                "emp.el"
                "epsf.el"
                "fancyref.el"
                "flashcards.el"
                "foils.el"
                "font-latex.el"
                "francais.el"
                "frenchb.el"
                "german.el"
                "graphics.el"
                "graphicx.el"
                "harvard.el"
                "hyperref.el"
                "icelandic.el"
                "index.el"
                "inputenc.el"
                "italian.el"
                "j-article.el"
                "jarticle.el"
                "j-book.el"
                "jbook.el"
                "j-report.el"
                "jreport.el"
                "jsarticle.el"
                "jsbook.el"
                "jurabib.el"
                "jura.el"
                "latex.el"
                "latexinfo.el"
                "letter.el"
                "listings.el"
                "ltx-base.el"
                "ltxdoc.el"
                "makeidx.el"
                "mdwlist.el"
                "MinionPro.el"
                "multido.el"
                "multind.el"
                "multi-prompt.el"
                "natbib.el"
                "naustrian.el"
                "ngerman.el"
                "nicefrac.el"
                "nomencl.el"
                "paralist.el"
                "pdfsync.el"
                "plfonts.el"
                "plhb.el"
                "polish.el"
                "polski.el"
                "preview.el"
                "preview-latex.el"
                "prosper.el"
                "prv-emacs.el"
                "psfig.el"
                "pst-grad.el"
                "pst-node.el"
                "pst-plot.el"
                "pstricks.el"
                "pst-slpe.el"
                "report.el"
                "ruby.el"
                "scrartcl.el"
                "scrbase.el"
                "scrbook.el"
                "scrlttr2.el"
                "scrpage2.el"
                "scrreprt.el"
                "shortvrb.el"
                "slides.el"
                "slovak.el"
                "subfigure.el"
                "swedish.el"
                "tabularx.el"
                "tex-bar.el"
                "tex-buf.el"
                "tex.el"
                "tex-fold.el"
                "tex-font.el"
                "tex-fptex.el"
                "tex-info.el"
                "tex-jp.el"
                "texmathp.el"
                "tex-mik.el"
                "tex-site.el"
                "tex-style.el"
                "toolbar-x.el"
                "units.el"
                "url-style.el"
                "varioref.el"
                "verbatim.el"
                "virtex.el")))

;;; latex-autoloads.el ends here
