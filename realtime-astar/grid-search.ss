;set an empty list for the path
(define path-lst '())

;
(define goalHeuristic
  (lambda (block1)
    (+ (abs (- (car block1) (car goal)))  (abs (- (cadr block1) (cadr goal))))))

;gets the frontier nodes of a given point
(define expand 
  (lambda (point)
    (let ((lst (adjacentv point)))
      (add-to-path-lst lst point)
      (addAdjacentsToQueue lst point))))

;adds the adjacent points to the queue
(define addAdjacentsToQueue
  (lambda (lst point)
    (if (not (null? lst))
        (let ( (startH  (+ 1  (block-status point)))   (goalH (goalHeuristic (car lst))) ) 
          (enqueue (list (+ startH goalH)  (car lst)))
          (block-set! (car lst) startH )
          (draw-pt-frontier (car lst))
          (addAdjacentsToQueue (cdr lst) point )))))

;adds nodes on the path to a list
(define add-to-path-lst
  (lambda (lst point)
    (if (not (null? lst))
       (let ((child-parent (list (car lst) point)))
         (set! path-lst (cons child-parent path-lst))
         (add-to-path-lst (cdr lst) point)))))
  
;draws frontier nodes
(define draw-pt-frontier
  (lambda (pt)
    (draw-frontier (car pt) (cadr pt))))


(define dobfs
  (lambda (grid stop-count)
    (block-set! start visited) ;sets start as visited
    (set! path-lst (list (list start '()))) ;adds it as the 1st node on the path
    (search2 grid 1 stop-count)))

(define dobfs2
  (lambda (grid count stop-count)
    (pause pause-num)
    (expand robot)
    (if (not (null? queue ));if there are free frontiers
      (let ((next-robot  (cadr (front)) )) ;checks next node in queue
        (cond
          [(equal? next-robot goal) ;if it finds the goal
           (set! robot (cadr (dequeue))) 
           (draw-moved-robot (robot-x) (robot-y)) 
           (display "Found")
           (newline)
            (let ((path (get-path goal))) ;gets and draws path
              (draw-path path)
              (display path))
            (newline)]
          ((>= count stop-count)
            (display "Took too long")
            (newline))
          (else
            (draw-visited (car robot) (cadr robot));draws nodes as visited
            (set! robot (cadr (dequeue)))
            (draw-moved-robot (robot-x) (robot-y))
            (search2 grid (+ count 1) stop-count))));call search on next node
       (begin
         (display "Cannot reach the goal") (newline)) )));no path to the goal
    
;gets the path to the goal
(define get-path
  (lambda (last-node)
    (if (equal? last-node start)
      (list start)
      (let ((next-node (cadr (assoc last-node path-lst))))
        (append (get-path next-node) (list last-node))))))
      
;draws path to the goal
(define draw-path
  (lambda (path)
    (cond 
      ((not (null? path))
         (draw-pt-path-node (car path))
         (draw-path (cdr path))))))
       
(define draw-pt-path-node
  (lambda (point)
    (draw-path-node (car point) (cadr point))))
