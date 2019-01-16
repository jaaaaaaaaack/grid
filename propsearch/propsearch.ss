;Marcus Vinicius and Jack Beal 
;Propositional Search for a 3x3 grid 


;List of facts about the grid
(define facts
  '(goal22 visited10 obstacle00 obstacle21))


;List of rules including all the possibilities for a 3x3 grid
(define rules
  '( ((visited00 goal00) (finished))
     ((visited01 goal01) (finished))
     ((visited02 goal02) (finished))
     ((visited10 goal10) (finished))
     ((visited11 goal11) (finished))
     ((visited12 goal12) (finished))
     ((visited20 goal20) (finished))
     ((visited21 goal21) (finished))
     ((visited22 goal22) (finished))
     ((visited00 (not obstacle01)) (visited01))
     ((visited00 (not obstacle10)) (visited10))
     ((visited01 (not obstacle00)) (visited00))
     ((visited01 (not obstacle02)) (visited02))
     ((visited01 (not obstacle11)) (visited11))
     ((visited02 (not obstacle01)) (visited01))
     ((visited02 (not obstacle12)) (visited12))
     ((visited10 (not obstacle20)) (visited20))
     ((visited10 (not obstacle11)) (visited11))
     ((visited10 (not obstacle00)) (visited00))
     ((visited11 (not obstacle01)) (visited01))
     ((visited11 (not obstacle10)) (visited10))
     ((visited11 (not obstacle12)) (visited12))
     ((visited11 (not obstacle21)) (visited21))
     ((visited12 (not obstacle02)) (visited02))
     ((visited12 (not obstacle11)) (visited11))
     ((visited12 (not obstacle22)) (visited22))
     ((visited20 (not obstacle10)) (visited10))
     ((visited20 (not obstacle21)) (visited21))
     ((visited21 (not obstacle20)) (visited20))
     ((visited21 (not obstacle22)) (visited22))
     ((visited21 (not obstacle11)) (visited11))
     ((visited22 (not obstacle21)) (visited21))
     ((visited22 (not obstacle12)) (visited12))))

;chech the rules to update the list of facts 
(define ModusPonens
  (lambda (rule)
    (ModusPonens2 (car rule) (cadr rule))))

(define ModusPonens2
  (lambda (b a)
    (if (null? b)
        a
        (let* ( (1stantecedent (car b)) (listResult (list? 1stantecedent)) (2ndantecedent (cdr b)))
            (if (eq? listResult #f)
                (if (member 1stantecedent facts)
                    (ModusPonens2 2ndantecedent a)
                    '())
                (if (member (cadr 1stantecedent) facts)
                    '()
                    (ModusPonens2 2ndantecedent  a)))))))

;search through the grid taking in consideration the grid rules. call Modus Pones to update the list
;of facts 
(define search
  (lambda (count)
    (cond
      ((member 'finished facts)
          (display "goal found")
          (newline))
      ((<= count 0)
        (display "not found")
        (newline))
      (else
        (let* ((firstRule (car rules))
               (remainingRules (append (cdr rules) (list firstRule)))
               (newFact (ModusPonens firstRule)))
          (set! rules remainingRules)
          (if (not (null? newFact))
              (let ((result (member (car newFact) facts)))
                (if (eq? result #f)
                    (set! facts (append newFact facts)))))
          (search (- count 1)))))))
