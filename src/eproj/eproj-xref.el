;; eproj-xref.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  7 November 2021
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'eieio)

(require 'eproj-symbnav)

(defun eproj-xref-symbnav-show-xrefs--impl (fetcher _aux-info enable-shortcut?)
  (let ((proj nil)
        (xrefs (funcall fetcher)))
    (eproj-symbnav/choose-location-to-jump-to
     "some item"
     ;; NB no tag name is available, see ‘eproj-xref-symbnav--xref->eproj-tag-quadruple’.
     (lambda (_proj _tag-name tag _mode)
       (cl-assert (null _tag-name))
       (eproj-xref-symbnav--tag->string tag))
     #'lsp-symbnav--tag-kind
     (eproj-symbnav-get-file-name)
     proj
     (eproj-symbnav-current-home-entry)
     (-map #'eproj-xref-symbnav--xref->eproj-tag-quadruple xrefs)
     enable-shortcut?
     "Choose\n\n")))

;;;###autoload
(defun eproj-xref-symbnav--tag->string (tag)
  "Simplified generic tag->string procedure for situations when there’s no tag name to show."
  (cl-assert (eproj-tag-p tag))
  (concat (propertize (eproj-tag/file tag) 'face 'eproj-symbnav-file-name)
          ":"
          (number->string (eproj-tag/line tag))
          "\n"
          (aif (eproj-tag/get-prop 'summary tag)
              it
            (eproj/extract-tag-line nil tag))
          "\n"))

;;;###autoload
(defun eproj-xref-symbnav-show-xrefs-with-shortcut (fetcher aux-info)
  (eproj-xref-symbnav-show-xrefs--impl fetcher aux-info t))

;;;###autoload
(defun eproj-xref-symbnav-show-xrefs (fetcher aux-info)
  (eproj-xref-symbnav-show-xrefs--impl fetcher aux-info nil))

(defun eproj-xref-symbnav--xref->eproj-tag-quadruple (xref)
  (with-slots (summary location) xref
    (let ((name nil)
          (proj nil)
          (tag (with-slots (file line column) location
                 (make-eproj-tag file
                                 line
                                 nil
                                 t
                                 (list (cons 'column column)
                                       (cons 'summary summary))))))
      (list name
            tag
            proj
            ;; No mode for disambiguation.
            nil))))

(provide 'eproj-xref)

;; Local Variables:
;; End:

;; eproj-xref.el ends here
