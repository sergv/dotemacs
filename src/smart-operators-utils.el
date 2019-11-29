;; smart-operators-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 29 November 2019
;; Description:

(defun smart-operators--point-surrounded-by2 (before2 before1 after1 after2)
  "Check if previous 2 characters before point are BEFORE2 and BEFORE1 and
that next 2 characters are AFTER1 and AFTER2."
  (let* ((pt-before    (save-excursion
                         (skip-syntax-backward " ")
                         (point)))
         (pt-after     (save-excursion
                         (skip-syntax-forward " ")
                         (point)))
         (real-before2 (char-before (- pt-before 1)))
         (real-before1 (char-before pt-before))
         (real-after1  (char-after pt-after))
         (real-after2  (char-after (+ pt-after 1))))
    (list
     pt-before
     pt-after
     (and real-before2
          real-before1
          real-after1
          real-after2
          (char-equal real-before2 before2)
          (char-equal real-before1 before1)
          (char-equal real-after1  after1)
          (char-equal real-after2  after2)))))

(defun smart-operators--point-surrounded-by (before after)
  "Check if point is surrounded by BEFORE and AFTER characters."
  (let* ((pt-before   (save-excursion
                        (skip-syntax-backward " ")
                        (point)))
         (pt-after    (save-excursion
                        (skip-syntax-forward " ")
                        (point)))
         (real-before (char-before pt-before))
         (real-after  (char-after pt-after))
         (is-before? (and real-before
                          (char-equal real-before before)))
         (is-after? (and real-after
                         (char-equal real-after after))))
    (list pt-before
          pt-after
          is-before?
          is-after?
          (and is-before?
               is-after?))))

(provide 'smart-operators-utils)

;; Local Variables:
;; End:

;; smart-operators-utils.el ends here
