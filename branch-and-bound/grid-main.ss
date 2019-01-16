; grid-main.ss

(define num-col-row 20)
(define pause-num 1000)
(define size (floor (/ 700 num-col-row)))
(define obstacle-density 30)
(load "grid-class.ss")
(load "grid-draw.ss")
(load "grid-make.ss")
(load "grid-queue.ss")
(load "grid-bestfs.ss")


(define grid0 (make-grid num-col-row)) 
(draw-obstacles grid0)
(define grid (convert-grid grid0))

(load "grid-new.ss")


(set-goal grid)
(set-start grid)
(draw-start)
(draw-goal)
(draw-robot)
(show canvas)
(search grid 20000)