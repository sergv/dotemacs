;; structured-fontification.lisp ---

;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 24 March 2012


(defpackage :life
  (:use :cl)
  (:export
   :game
   :make-game
   :game-height
   :game-width

   :print-area
   :clear-game
   :get-cell
   :cell-alivep
   :make-step
   :make-step-dont-track-changes
   :next-game

   :*random-gen*
   :random-game

   :+life-glider+
   :+life-blinker+
   :+life-gosper-glider-gun+
   :+life-explosion+
   :+life-quasaar+
   :+life-oscillator-achim-p16+
   :+life-oscillator-worker-bee+
   :+life-oscillator-16-two-pre-l-hasslers+
   :+life-oscillator-pentadecathlon-on-snacker+
   :+life-oscillator-p156-hans-leo-hassler+
   :+life-eater-1+
   :+life-dragon-ship+
   :+life-seal-ship+
   :+life-spider-ship+

   :create-game-with-lifeforms))


(declaim ;; (optimize (speed 0) (safety 3) (debug 3))
 (optimize (speed 3) (safety 0) (debug 0)))

(defun get-cell (game height width)
  (let ((wrapped-height
          (wrap-in-interval 0 (game-height game) height))
        (wrapped-width
          (wrap-in-interval 0 (game-width game) width)))
    (aref (game-field game) wrapped-height wrapped-width)))

(defstruct (game
            (:constructor make--game (field height width))
            (:print-function
             (lambda (game stream level)
               (declare (ignore level))
               (loop
                 for i below (game-height game)
                 do  (loop
                       for j below (game-width game)
                       do  (format stream "~d" (get-cell game i j))
                       finally (format stream "~%"))))))
  field
  height
  width)


(let ((maximum (bind ((maximum (maximum 1 2 3)))))))

(let ((maximum (maximum))))

(sum 5 2 3)


