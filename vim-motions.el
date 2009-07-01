;; TODO: bad insert-mode hack in vim:adjust-point

(provide 'vim-motions)

(defvar vim:current-row nil)
(defvar vim:current-col nil)

(defstruct (vim:coord
            (:constructor vim:make-coord))
  row
  col)


(defun vim:coord-to-pos (row-col)
  (save-excursion
    (goto-line (vim:coord-row row-col))
    (beginning-of-line)
    (max (point)
         (min (1- (line-end-position))
              (+ (point) (vim:coord-col row-col))))))

(defun vim:make-coord-from-pos (pos)
  (save-excursion
    (goto-char pos)
    (vim:make-coord :row (line-number-at-pos)
                    :col (- pos (line-beginning-position)))))


(defun vim:range-p (motion)
  (consp motion))

(defun vim:project-coord (coord)
  (if (vim:coord-p coord)
      (let ((min-row (line-number-at-pos (point-min)))
            (max-row (line-number-at-pos (point-max))))
        (vim:make-coord :row (max min-row (min max-row (vim:coord-row coord)))
                        :col (max 0 (vim:coord-col coord))))
    coord))

(defun vim:project-motion (motion)
  (if (consp motion)
      (cons (vim:project-coord (car motion))
            (vim:project-coord (cdr motion)))
    (vim:project-coord motion)))


(defun vim:adjust-end-of-line-position (pos)
  "If pos is an end-of-line returns pos - 1 and pos otherwise."
  (save-excursion
    (goto-char pos)
    (if (eolp)
        (1- pos)
      pos)))

(defun vim:adjust-point ()
  "Adjust the pointer after a command."
  (when (and (eolp) (not (bolp))
             (not (eq vim:active-mode vim:insert-mode)))
    (backward-char)))

(defun vim:motion-left (count)
  "Move the cursor count characters left."
  (max (line-beginning-position)
       (- (point) (or count 1))))

(defun vim:motion-right (count)
  "Move the cursor count characters right."
  (min (line-end-position)
       (+ (point) (or count 1))))

(defun vim:motion-up (count)
  "Move the cursor count lines up."
  (vim:make-coord :row (- (vim:coord-row vim:current-coord) (or count 1))
                  :col (vim:coord-col vim:current-coord)))

(defun vim:motion-down (count)
  "Move the cursor count lines down."
  (vim:make-coord :row (+ (vim:coord-row vim:current-coord) (or count 1))
                  :col (vim:coord-col vim:current-coord)))


(defun vim:motion-beginning-of-line (count)
  "Move the cursor to the beginning of the current line."
  (line-beginning-position))

(defun vim:motion-first-non-blank (count)
  "Move the cursor to the first non-blank character of the current line."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[^[:space:]]\\|$" )
    (match-beginning 0)))

(defun vim:motion-end-of-line (count)
  "Move the cursor to the end of the current line."
  (line-end-position))

(defun vim:motion-last-non-blank (count)
  "Move the cursor to the last non-blank charactor of the current line."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[[:space:]]*$")
    (max (line-beginning-position)
         (1- (match-beginning 0)))))

(defun vim:motion-go-to-first-non-blank-beg (count)
  "Moves the cursor to the first non-blank charactor of line count."
  (save-excursion
    (if count
        (goto-line count)
      (goto-char (point-min)))
    (vim:motion-first-non-blank 1)))

(defun vim:motion-go-to-first-non-blank-end (count)
  "Moves the cursor to the first non-blank charactor of line count."
  (save-excursion
    (if count
        (goto-line count)
      (goto-char (point-max)))
    (vim:motion-first-non-blank 1)))

(defun vim:motion-fwd-word (count)
  "Moves the cursor beginning of the next word."
  (save-excursion
    (forward-word (or count 1))
    (point)))


(defun vim:motion-find (count arg)
  "Move the cursor to the next count'th occurrence of arg."
  (save-excursion
    (when (search-forward (char-to-string arg) nil t (or count 1))
      (1- (point)))))


