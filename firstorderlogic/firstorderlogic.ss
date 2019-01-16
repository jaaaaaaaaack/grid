; firstOrderLogicML.ss
; COM-316 AI, 2017-10-27
; Marcus Vinicius Pinto Pereira Junior
; Jack Beal


(define adjacentsList '())

(define facts
  '((goal (0 0))(visited (2 2))(obst (0 1))(height (0 1) medium)
    (stability (0 1) stable)(obst (1 0))(height (1 0) high)(stability (1 0) stable)))


(define getAdjacents
  (lambda (p)
    (let ((x (car p))
          (y (cadr p)))
      (append
        ; Will be all possible adjacent spaces regardless of characteristics
        (if (< y 1) '() (list (list x (- y 1))))
        (if (< x 1) '() (list (list (- x 1) y)))
        (if (>= y (- 3 1)) '() (list (list x (+ y 1))))
        (if (>= x (- 3 1)) '() (list (list (+ x 1) y)))))))

(define search
  (lambda (counter)
    (cond ((<= counter 0)
           (display "goal was not found"))
      	(else (let ((firstFact (car facts)))
            	(if (eq? (car firstFact) 'visited)
          	  		(let* ((p (cadr firstFact)) (adjacents (getAdjacents p)) (result (getNewLocation p adjacents)))
               			(if (null? result)
                     		(backtrack (- counter 1)) ; For when it gets stuck with no unvisited adjacent spaces
                     		(begin
                         		(set! facts (append (list (list 'visited result)) facts))
                         		(let ((goalResult (checkGoal result)))
                             		(if (eq? goalResult #t)
                                   		(display " goal was found")
                                     	(search (- counter 1)))))))
  				(begin
    				(set! facts (append (cdr facts) (list firstFact)))
        			(search (- counter 1)))))))))


(define getNewLocation
  (lambda (p adjacents)
    (if (null? adjacents) ; No points left to check
        '()
        (let ((adjacentSpot (car adjacents)))
      	 	(cond ( (and (spaceIsFree adjacentSpot) (checkVisited adjacentSpot)) ; If it's an unvisited free space
               	 	(addFacts p (cdr adjacents))
 				 	adjacentSpot
               	  )

              	  ( (and (not (spaceIsFree adjacentSpot))  ; If it's an unvisited low stable obstacle
                         (checkVisited adjacentSpot)
                         (checkheight adjacentSpot)
                         (checkStability adjacentSpot))
               	 	(addFacts p (cdr adjacents))
 				 	adjacentSpot
      		  	  )

              	  (else (getNewLocation p (cdr adjacents))) ; Otherwise don't do anything
              )))))


(define checkVisited ; returns true if space is not visited
  (lambda (p)
    (if (eq? (member (list 'visited p) facts) #f)
        #t
        #f)))

(define spaceIsFree ; returns true if the space is not an obstacle
  (lambda (p)
    (if (eq? (member (list 'obst p) facts) #f)
        #t
        #f)))


(define checkStability ; returns true if the space is not unstable
  (lambda (p)
    (if (eq? (member (list 'stability p 'unstable) facts) #f)
        #t
        #f)))

(define checkheight ; returns true if the space is high
  (lambda (p)
    (if (eq? (member (list 'height p 'high) facts) #f)
        #t
        #f)))


(define addFacts ; adds facts for adjacent spaces
  (lambda (p lista)
    (if (null? lista)
        '()
        (let* ((adj1 (car lista)) (listItem (list 'adjacent p adj1)) (result (member listItem adjacentsList)))
          (if (eq? result #f)
              (set! adjacentsList (append adjacentsList (list (list 'adjacent p adj1)))))
          (addFacts p (cdr lista))))))


(define backtrack ; backtracks if we run out of adjacent spaces
  (lambda (counter)
    (if (null? adjacentsList)
        (display "goal was not found")
        (let* ( (top (car adjacentsList)) (result (getNewLocation (cadr top) (list (caddr top)))) )
            (set! adjacentsList (cdr adjacentsList))
          	(if (null? result)
                (backtrack (- counter 1))
                (begin
                  (set! facts (append (list (list 'visited result)) facts))
                  (let ((goalResult (checkGoal result)))
                    	(if (eq? goalResult #t)
                         	(display "goal was found")
                          	(search (- counter 1))))))))))


(define checkGoal ; returns true if space is a goal
  (lambda (p)
    (if (eq? (member (list 'goal p) facts) #f)
        #f
        #t)))







