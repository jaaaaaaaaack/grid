(define get-next-goal
  (lambda (point)
    (g-2minimax start robot)))

(define get-next-goal-old
  (lambda (point)
    (let* ((lst1 (cons point (adjacento point)))
           (lst0 (randomize lst1))
           (flst (calculate-h-goal lst0))
           (lst (map list flst lst0))) 
      (set! queue '())
      (enqueue lst)
      (set! queue (reverse queue))
      (let ((num (random 10))
            (len (length lst0))
            (best (front)))
         (cond 
           ((= num 0)
               (list-ref lst0 (random len))) 
            (else
               best))))))

(define calculate-h-goal
  (lambda (lst)
    (map h-goal lst)))

(define h-goal
  (lambda (point)
    (+ (abs (- (car point) (car robot)))
       (abs (- (cadr point) (cadr robot))))))   


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

; Manhattan distance on two coordinate pairs
(define g-mandist
  (lambda (p1 p2)
    (+ (abs (- (car p1) (car p2)))
       (abs (- (cadr p1) (cadr p2))))))

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
    ; get our next moves (next turn) and the opponent's possible moves (second turn)
    (let ((goalstates (adjacentv goal)) (robotstates (adjacentv robot)))
      (caadr (g-getmin (g-iterlist (makepairs goalstates robotstates)))))))