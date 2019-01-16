(define queue '())

;returns node at the front of the queue
(define front
  (lambda ()
    (if (null? queue)
        '()
        (car queue))))

;deletes the node at the top of the queue
(define dequeue
  (lambda ()
    (if (null? queue)
        '()
        (let ((temp (front)))
           (set! queue (cdr queue))
           temp))))

;adds node to the correct place in the queue, depending on the goodness of 
; the node
(define enqueue
  (lambda (x)
     (set! queue (enqueue2 x queue))))

(define enqueue2
  (lambda (x lst)
    (cond 
      [(null? lst)  (list x) ]  
      [(<= (car x) (caar lst)) (cons x lst)]
      [else  (cons (car lst) (enqueue2 x (cdr lst)) )] )))

