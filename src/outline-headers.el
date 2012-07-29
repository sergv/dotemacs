;; setup-outline.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday,  9 May 2012
;; Description:


(defvar-loc +outline-headers-min-header-length+ nil
  "Minimum number of `+outline-headers-section-symbol+''s allowed in header.")

(defvar-loc +outline-headers-max-header-length+ nil
  "Maximum number of `+outline-headers-section-symbol+''s allowed in header.")




(defvar-loc +outline-headers-section-start+
  "^"
  "Beginning part of `+outline-headers-header-re+'.")

(defvar-loc +outline-headers-section-symbol+
  nil
  "Main part of `+outline-headers-header-re+' that defines headers of different length.")

(defvar-loc +outline-headers-section-end+
    "\\(?: \\|$\\)"
  "End part of `+outline-headers-header-re+'.")



(defvar-loc +outline-headers-header-re+
  nil
  "Regular expression that defines headers")

;;;;

(defun outline-headers-count-header-symbols ()
  "Return number of `+outline-headers-section-symbol+' symbols in header
at point."
  (assert (looking-at-p +outline-headers-header-re+))
  (save-excursion
   (save-match-data
    (if (looking-at +outline-headers-section-start+)
      (let ((match-length 0))
        (goto-char (match-end 0))
        (while (and (looking-at-p +outline-headers-section-symbol+)
                    (not (looking-at-p +outline-headers-section-end+)))
          (forward-char 1)
          (incf match-length))
        match-length)
      0))))

(defun outline-headers-outline-level ()
  "Calculate header nesting level."
  (max (- +outline-headers-max-header-length+
          (outline-headers-count-header-symbols))
       0))

(defun outline-headers-hide-all (&optional count)
  "Find out maximum length of buffer headings and hide all those toplevel
headings."
  (interactive "P")
  (save-excursion
   (save-match-data
    (let (header-re)
      (unless count
        (setf count +outline-headers-min-header-length+)
        (goto-char (point-min))
        (while (re-search-forward +outline-headers-header-re+ nil t)
          (goto-char (match-beginning 0))
          (setf count (max count
                           (outline-headers-count-header-symbols)))
          (goto-char (match-end 0))))
      (setf header-re
            (format "%s%s\\{%d\\}%s"
                    +outline-headers-section-start+
                    +outline-headers-section-symbol+
                    count
                    +outline-headers-section-end+))
      (goto-char (point-min))
      (while (re-search-forward header-re nil t)
        (when (hs-already-hidden-p)
          ;; if we're in hideshow-hidden block then show it
          (save-excursion
           ;; since hs-show-block repositions point to the beginning of
           ;; the block we need to surround it with save-excursion in order
           ;; to retain our position of outline heading matched by header-re
           (hs-show-block)))
        (hide-subtree)
        (forward-line 1)
        (beginning-of-line))))))





(defun* setup-outline-headers (&key
                               (header-start "^")
                               (header-symbol nil)
                               (header-end "\\(?: \\|$\\)")
                               (length-min 3)
                               (length-max 10))
  (unless header-symbol
    (setf header-symbol
          (assoc 'one-line
                 (assoc major-mode
                        +comment-util-comment-format-alist+)))
    (when (< 1 (length header-symbol))
      (error "setup-outline-headers: error: fetched header-symbol from comment-util but it's length is greater than 1: \"%s\" and no other header-symbol was provided" header-symbol)))
  (assert (and (stringp header-symbol)
               (= 1 (length header-symbol)))
          nil
          "header-symbol must be string of length 1")
  (assert (stringp header-start)
          nil
          "header-start must me a string")
  (assert (stringp header-end)
          nil
          "header-end must me a string")
  (assert (and (integerp length-min)
               (>= length-min 1))
          nil
          "length-min must be integer >= 1")
  (assert (and (integerp length-max)
               (>= length-max length-min))
          nil
          "length-max must be integer >= length-min")

  (setf +outline-headers-section-start+     header-start
        +outline-headers-section-symbol+    (regexp-quote header-symbol)
        +outline-headers-section-end+       header-end
        +outline-headers-min-header-length+ length-min
        +outline-headers-max-header-length+ length-max)

  (setf +outline-headers-header-re+
        (format "%s%s\\{%d,%d\\}%s"
                +outline-headers-section-start+
                +outline-headers-section-symbol+
                +outline-headers-min-header-length+
                +outline-headers-max-header-length+
                +outline-headers-section-end+))

  (setq buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table
                          'selective-display
                          (string-to-vector " ..."))

  (set (make-local-variable 'outline-regexp)
       +outline-headers-header-re+)
  ;; (set (make-local-variable 'outline-heading-end-regexp)
  ;;      "\n")
  (set (make-local-variable 'outline-heading-end-regexp)
       (concat "\\(?:"
               +outline-headers-header-re+
               ".*\n\\)+"))

  (set (make-local-variable 'outline-level) #'outline-headers-outline-level)
  (outline-minor-mode t)

  (def-keys-for-map2 vim:normal-mode-local-keymap
    ("z F" outline-headers-hide-all)
    ("z f" hide-subtree)
    ("z U" show-all)
    ("z u" show-subtree)))



(provide 'outline-headers)


;; Local Variables:
;; lexical-binding: t
;; End:

;; setup-outline.el ends here
