; Jack Beal
; Jigar Dhimar
; Marcus Pinto
; Mike Riley
; Final Project - Predator 
; Due: 12/11/17

(define get-next-robot 
  (lambda (point)
    (let* ((lst1 (cons point (adjacento point)))
           (lst0 (randomize lst1))
           (flst (gcalculate-h-goal lst0))
           (lst (map list flst lst0))) 
      (set! queue '())
      (enqueue lst)
          
         (cond
           [(eq? #t (r-dist-determine)) 
            (let  ((rPoint (r-minimax)))
                (if (null? rPoint) point 
                    (cond 
                      ((eq? 100000 (block-status rPoint)) (mike-blast (list rPoint)) point)
                      (else rPoint))))]))))


(define r-dist-determine
  (lambda ()
    (cond
      [ (< 0.1 (r-dist robot goal)) #t])))


(define r-dist
    (lambda (pt1 pt2)
      (let* ((x1 (car pt1))
            (y1 (cadr pt1))
            (x2 (car pt2))
            (y2 (cadr pt2)))
         (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))))


(define mike-blast
  (lambda (lst)
    (if (not (null? lst))
      (let* ((pt (car lst))
             (x (car pt))
             (y (cadr pt)))
        (cond ((= (get-node grid x y) obstacle)
          (set-node! grid x y free)
          (send canvas make-now-free x y)))
        (mike-blast (cdr lst))))))

(define mike-build
  (lambda (lst)
    (if (not (null? lst))
      (let* (
             (x (car lst))
             (y (cadr lst)))
        (cond 
          ((= (get-node grid x y) free)
            (set-node! grid x y obstacle)
            (send canvas make-obstacle x y))
          )))))


; Helps avoid choosing blocks where 3/4 adjacent nodes are obstacles
(define r-noCrevices
  (lambda (block)
    (let* ( (adjacentBlocks (adjacent block)) )
         (map block-status adjacentBlocks))))

; Helper function for r-noCrevices
(define r-sumList
  (lambda (lst)
    (if (null? lst) 
        0
        (+ (car lst) (r-sumList (cdr lst))))))

; Heuristic Function 
; Considers 1) manhattan distance to the goal 
; 2) Avoiding crevices
; 3) Adds some small random value for each node
(define r-h
  (lambda (start1 inGoal)
    (let* ( (manhattanDistance  (+ (abs (- (car start1) (car inGoal))) (abs (- (cadr start1) (cadr inGoal)))))
           (var (random 1.0))
           (obstacles (r-noCrevices start1)) (obstaclesSum (r-sumList obstacles)))
      (if (= obstaclesSum 30000) 
          (+ var (+ manhattanDistance 100000))
          (+ manhattanDistance var)))))

; Main minimax function
(define r-minimax
  (lambda ()
    (let ( (robotMoves (adjacent robot)) (goalMoves (adjacent goal)) )
      ; (set! robotMoves (append robotMoves (list robot)))
      ; (set! goalMoves (append goalMoves (list goal)))
      (caadr (r-getMax (r-iterateLists (r-pairs robotMoves goalMoves)))))))

; Goes through leaf nodes of tree and gets the min choices 
(define r-iterateLists
  (lambda (lst)
    (if (null? lst) 
        '()
        (let ( (best (r-getMin (car lst))) (result (r-iterateLists (cdr lst))))
          (append (cdr best) result)))))

; Goes through a list of nodes (each with a goodness) and chooses the node with the highest goodness
(define r-getMin
  (lambda (lst)
    (if (null? lst)
        (list -10000000 '())
        (let ( (temp (list (r-h (caar lst) (cadar lst)) (car lst))) (result (r-getMin (cdr lst))) )  
          (if (< (car temp) (car result)) 
              result
              temp)))))

; Goes through a list of nodes (each with a goodness) and chooses the node with the lowest goodness
(define r-getMax
  (lambda (lst)
    (if (null? lst)
        (list 10000000 '())
        (let ( (temp (list (r-h (caar lst) (cadar lst)) (car lst))) (result (r-getMax (cdr lst))) )  
          (if (< (car temp) (car result)) 
              temp
              result)))))

; Generates two ply's of moves
(define r-pairs
  (lambda (robotList goalList)
    (if (null? robotList)
        '()
        (let ( (lst (r-pairs2 (car robotList) goalList)) (result (r-pairs (cdr robotList) goalList)) )
          (append (list lst) result)))))

; Helper function for r-pairs
(define r-pairs2
  (lambda (x goalMoves) 
    (if (null? goalMoves) 
        '()
        (let ( (pair (list x (car goalMoves))) (result (r-pairs2 x (cdr goalMoves)))  )
          (append (list pair) result)))))

