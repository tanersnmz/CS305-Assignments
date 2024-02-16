(define define-stmt? (lambda (e)
    (and (list? e) (= (length e) 3) (eq? (car e) 'define) (symbol? (cadr e)) (exprCheck? (caddr e)))))
  
(define ifCheck?
  (lambda (e)
    (if (list? e)
        (if (= (length e) 4)
            (if (eq? (car e) 'if)
                #t
                #f)
            #f)
        #f)
    )
  )

(define get-operator 
  (lambda (op)
    (cond
      ((eq? op '+) +) 
      ((eq? op '*) *) 
      ((eq? op '/) /) 
      ((eq? op '-) -) 
      (else ((display "cs305: ERROR \n\n") (repl env)))
    )
  )
)

(define lambdaCheck? 
  (lambda (e)
    (if (list? e)
        (if (eq? 'lambda (car e))
            (if (ListCheckUserDefined? (cadr e))
                (if (exprCheck? (caddr e))
                    (if (not (define-stmt? (caddr e)))
                        #t
                        #f)
                    #f)
                #f)
            #f)
        #f)
    )
  )



(define operatorCheck?
  (lambda (e)
    (if (list? e)
        (if (> (length e) 2)
            (if (member (car e) '(- + / *))
                #t
                #f)
            #f)
        #f)
    )
  )


(define get-value 
  (lambda (var old-env new-env)
    (cond
      ((null? new-env) (display "cs305: ERROR \n\n") (repl old-env))
      ((equal? (caar new-env) var) (cdar new-env))
      (else (get-value var old-env (cdr new-env)))
    )
  )
)

(define extend-env 
  (lambda (var val old-env)
    (cons (cons var val) old-env)
  )
)


(define s7 (lambda (e env)
  (if (exprCheck? e)
      (cond
        ((number? e) e)
        ((and (symbol? e) (SymbolCheckUserDefined? e env)) (get-value e env env))
        ((and (not (procedureCheck? e)) (not (list? e))) 
          (begin 
            (display "cs305: ERROR \n\n") 
            (repl env)
          )
        )
        ((null? e) e)
        ((procedureCheck? e) e)
        ((ifCheck? e) 
          (let ((val 
                  (if (not (= (s7 (cadr e) env) 0))
                      (s7 (caddr e) env)
                      (s7 (cadddr e) env))
                )
               )
            val
          )
         )
        ((letCheck? e) (letCreate e env))
        ((lambdaCheck? e) e)
        ((lambdaCheck? (car e)) (lambdaCreate e env))
        ((operatorCheck? e) (applyOperator e env))
        (else (s7 (cons (get-value (car e) env env) (cdr e)) env))
      )
      ((display "cs305: ERROR \n\n") (repl env))
  )
))








(define variableListEnv? 
  (lambda (e)
    (if (or (eq? e '())
            (if (and (= (length (car e)) 2)
                     (symbol? (caar e)))
                (if (> (length e) 1)
                    (variableListEnv? (cdr e))
                    #t)
                #f))
        #t
        #f)
    )
  )


(define letCheck?
  (lambda (e)
    (if (list? e)
        (if (= (length e) 3)
            (if (eq? (car e) 'let)
                (if (variableListEnv? (cadr e))
                    #t
                    #f)
                #f)
            #f)
        #f)
    )
  )



(define exprCheck? 
  (lambda (e)(or (number? e)(symbol? e)(and (list? e) (or (ifCheck? e) (letCheck? e) (lambdaCheck? e) 
          (lambdaCheck? (car e))
          (symbol? (car e)) 
          (operatorCheck? e) )))))







(define procedureCheck?
  (lambda (e)
    (if (member e '(* - + /))
        #t
        #f)
  )
)


(define ListCheckUserDefined? 
  (lambda (e)
    (if (symbol? (car e))
        (if (list? e)
            (if (or (null? (cdr e))
                    (ListCheckUserDefined? (cdr e)))
                #t
                #f)
            #f)
        #f)
    )
  )



(define SymbolCheckUserDefined?
  (lambda (e env)
    (if (null? env)
        #f
        (if (eq? (caar env) e)
            #t
            (SymbolCheckUserDefined? e (cdr env))
        )
    )
  )
)



(define letCreate
  (lambda (e env)
    (let ((bindings (cadr e)))
      (let ((vars (map car bindings)))
        (let ((vals (map (lambda (val) (s7 val env)) (map cadr bindings))))
          (cond ((letValid? vars) "ERROR")
                (else (s7 (caddr e) (append (map cons vars vals) env)))
          )
        )
      )
    )
  )
)

(define letValid?
  (lambda (lst)
    (define (loop lst)
      (if (null? lst)
          #f
          (if (member (car lst) (cdr lst))
              #t
              (loop (cdr lst)))))
    (loop lst)
  )
)

(define applyOperator
  (lambda (e env)
    (let ((operands (map s7 (cdr e) (make-list (length (cdr e)) env)))
        (operator (get-operator (car e))))
        (apply operator operands)
    )
  )
)

(define lambdaCreate
  (lambda (e env)
    (if (= (length (cadar e)) (length (cdr e)))
      ((lambda (parameters)
         ((lambda (new-env)
            (s7 (caddar e) new-env))
          (append (map cons (cadar e) parameters) env)))
       (map s7 (cdr e) (make-list (length (cdr e)) env)))
      ((display "cs305: ERROR \n\n") (repl env))
    )
  )
)


(define repl
  (lambda (env)
    (let* (
      (dummy1 (display "cs305> "))
      (expr (read))
      (new-env 
        (cond ((define-stmt? expr) 
               (extend-env (cadr expr) (s7 (caddr expr) env) env))
              (else env))
      )
      (val 
        (cond ((define-stmt? expr)
               (cadr expr))
              (else
               (let ((result (s7 expr env)))
                 (cond ((or (lambdaCheck? result) (procedureCheck? result)) "[PROCEDURE]")
                       (else result)))))
      )
      (dummy2 (display "cs305: "))
      (dummy3 (display val))
      (dummy4 (newline))
      (dummy4 (newline)))
    (repl new-env)
  )
)
)

(define cs305 (lambda () (repl '())))







