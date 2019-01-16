; Jack Beal
; Jigar Dhimar
; Marcus Pinto
; Mike Riley
; Final Project - Goal 
; Due: 12/11/17


(define get-next-goal 
  (lambda (point)
    (let* ((lst1 (cons point (adjacento point)))
           (lst0 (randomize lst1))
           (flst (gcalculate-h-goal lst0))
           (lst (map list flst lst0))) 
      (set! queue '())
      (enqueue lst)
         (cond
           [(eq? (car (g-dist robot goal))  #t) 
            (let  ((rPoint (g-2minimax robot goal)))
                (if (null? rPoint) point 
                    (cond 
                      ((eq? 100000 (block-status rPoint)) (marcus-taketh (list rPoint)) point)
                      (else rPoint))))]
           [ (eq? #f (car (g-dist robot goal)) )
              (let ((obsPoint (g-minimax)))
                  ; (display obsPoint) 
                  (if (null? obsPoint) point 
                      (cond
                        [ (not (eq? 100000 (block-status obsPoint))) (marcus-giveth obsPoint) point]
                        [else point])))]
           [else point]))))


(define sqr
  (lambda (x)
    (* x x)))

(define g-dist
    (lambda (pt1 pt2)
      (let* ((x1 (car pt1))
            (y1 (cadr pt1))
            (x2 (car pt2))
            (y2 (cadr pt2)))
           (NN (list (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))) ))
                   
                   )))

(define marcus-taketh
  (lambda (lst)
    (if (not (null? lst))
      (let* ((pt (car lst))
             (x (car pt))
             (y (cadr pt)))
        (cond ((= (get-node grid x y) obstacle)
          (set-node! grid x y free)
          (send canvas make-now-free x y)))
        (marcus-taketh (cdr lst))))))

(define marcus-giveth
  (lambda (lst)
    ; (display "outside")
    (if (not (null? lst))
      (let* (
             (x (car lst))
             (y (cadr lst)))
          ; (display "inside")
        (cond 
          ((= (get-node grid x y) free)
            (set-node! grid x y obstacle)
            (send canvas make-obstacle x y))
          )))))




;-------------------------- MINIMAX for running away from predator  --------------------------

(define gcalculate-h-goal
  (lambda (lst)
    (map gh-goal lst)))

(define gh-goal
  (lambda (point)
    (+ (abs (- (car point) (car robot)))
       (abs (- (cadr point) (cadr robot))))))   

(define calculate-h-goal
  (lambda (lst)
    (map h-goal lst)))

(define g-mandist
  (lambda (p1 p2)
    (let* ((manhattan (+ (abs (- (car p1) (car p2)))(abs (- (cadr p1) (cadr p2))))) (sidecong (g-sidecongestion (adjacent p1) 0)))
    (+ (random 1.0) (+ manhattan sidecong)))))  

; Permute a list across another list
(define makepairs
  (lambda (l1 l2)
    (cond
      ((null? l1) '())
      (else
        (let ((lst (permut (car l1) l2)) (result (makepairs (cdr l1) l2)))
          (append (list lst) result))))))

; Permute a list across a point
(define permut
  (lambda (p lst)
    (cond
      ((null? lst) '())
      (else
        (let ((pair (list p (car lst))) (result (permut p (cdr lst))))
          (append (list pair) result))))))

; Get the static evaluation on list of pairs of pairs e.g. ( ((1 2)(3 4)) ((5 6)(7 8)) )
(define g-steval
  (lambda (statelist h-function)
    (if 
      (null? statelist) '()
    ;else
      (cons (h-function (caar statelist) (cdar statelist)) 
            (g-steval (cdr statelist) h-function)))))

; Iterate g-getmin over the bottom of the list, and g-getmax on the results
(define g-iterlist
  (lambda (lst)
    (if (null? lst)
      '()
      (let ((best (g-getmax (car lst))) (result (g-iterlist (cdr lst))))
        (append (cdr best) result)))))

(define g-sidecongestion
  (lambda(listadj congestion)
    (cond
      ((null? listadj) congestion)
      ((eq? (block-status(car listadj)) 10000) (g-sidecongestion (cdr listadj) (+ congestion 1)))
      (else (g-sidecongestion (cdr listadj) congestion)))))

; Want to minimize distance gained by robot
(define g-getmin
  (lambda (lst)
    (if (null? lst)
          (list -1000000 '())
          (let ((temp (list (g-mandist (caar lst) (cadar lst)) (car lst))) (result (g-getmin (cdr lst))))
            (if (< (car temp) (car result))
              result
              temp)))))

(define g-getmax
  (lambda (lst)
    (if (null? lst)
          (list 1000000 '())
          (let ((temp (list (g-mandist (caar lst) (cadar lst)) (car lst))) (result (g-getmax (cdr lst))))
            (if (> (car temp) (car result))
              result
              temp)))))

(define g-2minimax
  (lambda (stpoint glpoint)
    (let ((goalstates (adjacent goal)) (robotstates (adjacent robot)))
      (if (or (null? goalstates) (null? robotstates)) '() (caadr (g-getmin (g-iterlist (makepairs goalstates robotstates)))) ))))






;------------------ Perceptron Learning for deciding when to build obstacles  ------------------
(define threshold-weights '(((-3.5 -1))))

(define NN
  (lambda (lst)
    (NN2 lst threshold-weights)))

(define NN2
  (lambda (lst tw)
    (if (null? tw)
        lst
    ;else
      (let ((next-level (get-next-level lst (car tw))))
         (NN2 next-level (cdr tw))))))

(define get-next-level
  (lambda (lst twl)
     (if (null? twl)
       '()
     ;else
       (cons (get-nodeNN lst (car twl)) (get-next-level lst (cdr twl))))))

(define get-nodeNN
  (lambda (lst twn)
    (let ((threshold (car twn))
          (weights (cdr twn)))
      (< (s (+ (get-activations lst weights) (- threshold))) 0.6) )))

(define get-activations
  (lambda (lst w)
    (if (null? lst)
       0
    ;else
       (+ (* (car lst) (car w)) (get-activations (cdr lst) (cdr w))))))

(define s
  (lambda (x)
    (/ 1 (+ 1 (expt 2.71828 (- 0 x))))))







;---------- Different minimax for building obstacles when predator is close ----------
; Helps avoid choosing blocks where 3/4 adjacent nodes are obstacles
(define g-noCrevices
  (lambda (block)
    (let* ( (adjacentBlocks (adjacent block)) )
         (map block-status adjacentBlocks))))

; Helper function for r-noCrevices
(define g-sumList
  (lambda (lst)
    (if (null? lst) 
        0
        (+ (car lst) (g-sumList (cdr lst))))))

; Heuristic Function 
; Considers 1) manhattan distance to the goal 
; 2) Avoiding crevices
; 3) Adds some small random value for each node
(define g-h
  (lambda (start1 inGoal)
    (let* ( (manhattanDistance  (+ (abs (- (car start1) (car inGoal))) (abs (- (cadr start1) (cadr inGoal)))))
           (var (random 1.0))
           (obstacles (g-noCrevices start1)) (obstaclesSum (g-sumList obstacles)))
      (if (= obstaclesSum 30000) 
          (+ var (+ manhattanDistance 100000))
          (+ manhattanDistance var)))))

; Main minimax function
(define g-minimax
  (lambda ()
    (let ( (robotMoves (adjacent goal)) (goalMoves (adjacent robot)) )
      ; (set! robotMoves (append robotMoves (list robot)))
      ; (set! goalMoves (append goalMoves (list goal)))
      (caadr (g-getMax (g-iterateLists (g-pairs robotMoves goalMoves)))))))

; Goes through leaf nodes of tree and gets the min choices 
(define g-iterateLists
  (lambda (lst)
    (if (null? lst) 
        '()
        (let ( (best (g-getMin (car lst))) (result (g-iterateLists (cdr lst))))
          (append (cdr best) result)))))

; Goes through a list of nodes (each with a goodness) and chooses the node with the highest goodness
(define g-getMin
  (lambda (lst)
    (if (null? lst)
        (list -10000000 '())
        (let ( (temp (list (g-h (caar lst) (cadar lst)) (car lst))) (result (g-getMin (cdr lst))) )  
          (if (< (car temp) (car result)) 
              result
              temp)))))

; Goes through a list of nodes (each with a goodness) and chooses the node with the lowest goodness
(define g-getMax
  (lambda (lst)
    (if (null? lst)
        (list 10000000 '())
        (let ( (temp (list (g-h (caar lst) (cadar lst)) (car lst))) (result (g-getMax (cdr lst))) )  
          (if (< (car temp) (car result)) 
              temp
              result)))))

; Generates two ply's of moves
(define g-pairs
  (lambda (robotList goalList)
    (if (null? robotList)
        '()
        (let ( (lst (g-pairs2 (car robotList) goalList)) (result (g-pairs (cdr robotList) goalList)) )
          (append (list lst) result)))))

; Helper function for r-pairs
(define g-pairs2
  (lambda (x goalMoves) 
    (if (null? goalMoves) 
        '()
        (let ( (pair (list x (car goalMoves))) (result (g-pairs2 x (cdr goalMoves)))  )
          (append (list pair) result)))))

