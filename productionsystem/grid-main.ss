(define num-col-row 20)
(define pause-num 1000000)
(define size (floor (/ 650 num-col-row)))
(define obstacle-density 40)
(define high-density 20)
(define stable-density 20)
(load "grid-class.ss")
(load "grid-draw.ss")
(load "grid-make.ss")
(define grid0 (make-grid num-col-row))
(draw-obstacles grid0)
(define grid (convert-grid grid0))
(load "grid-new.ss")
(load "grid-ProductionSystem.ss")
(set-goal grid)
(set-start grid)
(draw-start)
(draw-goal)
(draw-robot)
(show canvas)
(search)