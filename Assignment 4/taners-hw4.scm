(define twoOperatorCalculator
  (lambda (inputList)
    (if (null? inputList)
        '() 
        (if (eq? (length inputList) 1)
            (car inputList)  
            (let ((first-num (car inputList))
                  (operator (cadr inputList))
                  (second-num (car (cddr inputList)))
                  (rest-of-expr (cdr (cddr inputList))))
              (let ((partial-result
                     (if (eq? operator '+)
                         (+ first-num second-num)
                         (- first-num second-num))))
                (if (null? rest-of-expr)
                    partial-result
                    (twoOperatorCalculator (cons partial-result rest-of-expr)))))))))



(define fourOperatorCalculator
  (lambda (inputList)
    (if (null? inputList)
        '()
        (if (eq? (length inputList) 1)
            inputList
            (let ((lead-term (car inputList))
                  (operation (cadr inputList))
                  (tail-terms (cddr inputList)))
              (cond
                ((eq? operation '*)
                 (if (eq? (length tail-terms) 1)
                     (list (* lead-term (car tail-terms)))
                     (fourOperatorCalculator 
                      (cons (* lead-term (car tail-terms)) 
                            (if (> (length tail-terms) 1) 
                                (cons (cadr tail-terms) (cddr tail-terms)) 
                                '())))))
                ((eq? operation '/)
                 (if (eq? (length tail-terms) 1)
                     (list (/ lead-term (car tail-terms)))
                     (fourOperatorCalculator 
                      (cons (/ lead-term (car tail-terms)) 
                            (if (> (length tail-terms) 1) 
                                (cons (cadr tail-terms) (cddr tail-terms)) 
                                '())))))
                (else
                 (cons lead-term (fourOperatorCalculator (cdr inputList))))))))))


(define (calculatorNested inputList)
  (cond ((not (checkOperators inputList)) #f)
        (else (calculateNested inputList))))

(define (calculateNested inputList)
  (if (null? inputList)
      '()
      (let ((currentElement (car inputList)))
        (cons (if (list? currentElement)
                  (twoOperatorCalculator
                   (fourOperatorCalculator
                    (calculatorNested currentElement)))
                  currentElement)
              (calculateNested (cdr inputList))))))



(define (checkOperators inputList)
  (cond ((or (null? inputList) (not (list? inputList))) #f)
        ((and (number? (car inputList)) (null? (cdr inputList))) #t)
        ((and (list? (car inputList)) (null? (cdr inputList))) (checkOperators (car inputList)))
        ((number? (car inputList)) (cond ((eq? '+ (cadr inputList)) (checkOperators (cddr inputList)))
                                         ((eq? '- (cadr inputList)) (checkOperators (cddr inputList)))
                                         ((eq? '* (cadr inputList)) (checkOperators (cddr inputList)))
                                         ((eq? '/ (cadr inputList)) (checkOperators (cddr inputList)))
                                         (else #f)))
        ((list? (car inputList)) (cond ((eq? '+ (cadr inputList)) (and (checkOperators (car inputList)) (checkOperators (cddr inputList))))
                                       ((eq? '- (cadr inputList)) (and (checkOperators (car inputList)) (checkOperators (cddr inputList))))
                                       ((eq? '* (cadr inputList)) (and (checkOperators (car inputList)) (checkOperators (cddr inputList))))
                                       ((eq? '/ (cadr inputList)) (and (checkOperators (car inputList)) (checkOperators (cddr inputList))))
                                       (else #f)))
        (else #f)))

(define (calculator inputList)
  (cond ((checkOperators inputList) (twoOperatorCalculator (fourOperatorCalculator (calculatorNested inputList))))
        (else #f)))
