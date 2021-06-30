#lang racket
(provide (all-defined-out))

; Question 1: expr-compare

(define (create-yList a b)
  (let ([first (if (not(empty? a)) (car a) #t)]
        [second (if (not(empty? a)) (cdr a) #t)]
        [third (if (not(empty? b)) (car b) #t)]
        [fourth (if (not(empty? b)) (cdr b) #t)])

    (cond
      [(and (empty? b) (empty? a))  (hash)]
      [(not (eqv? third first)) 
        (hash-set (create-yList second fourth) third (string->symbol (string-append (symbol->string first) "!" (symbol->string third))))]
      [(eqv? third first)  
        (hash-set (create-yList second fourth) third third)]
    )
  )
)


(define (switcheroo a xList top)
  (let ([first (if (not(empty? a)) (car a) #t)]
        [second (if (not(empty? a)) (cdr a) #t)]
        [string "Not Found1"]
        [mt '()])

    (cond
      [(empty? a)  mt]
      [(eqv? first 'quote)  a]
      [(boolean? first)  
        (cons first (switcheroo second xList #f))]
      [(list? first)  
        (cons (switcheroo first xList #t) (switcheroo second xList #f))]
      [(and top (eqv? 'if first ))  
        (cons first (switcheroo second xList #f))]
      [(and top (or (eqv? 'λ first) (eqv? 'lambda first)))
        (cons first (cons (car second) (switcheroo (cdr second) (cons (create-xList (car second) (car second)) xList) #f)))]
      [else (cons (if (eqv? string (most-recent first xList)) first (most-recent first xList)) (switcheroo second xList #f))]
    )
  )
)


(define (create-xList a b)
  (let ([first (if (not(empty? a)) (car a) #t)]
        [second (if (not(empty? a)) (cdr a) #t)]
        [third (if (not(empty? b)) (car b) #t)]
        [fourth (if (not(empty? b)) (cdr b) #t)])

    (cond 
      [(and (empty? b) (empty? a))  (hash)]
      [(not (eqv? third first)) 
        (hash-set (create-xList second fourth) first (string->symbol (string-append (symbol->string first) "!" (symbol->string third))))]
      [(eqv? third first)  
        (hash-set (create-xList second fourth) first first)]
    )
  )
)


(define (most-recent a dict-list)
  (let ([first (if (not(empty? dict-list)) (car dict-list) #t)]
        [second (if (not(empty? dict-list)) (cdr dict-list) #t)]
        [string "Not Found1"])

    (cond
      [(empty? dict-list)   string]
      [(not (eqv? string (hash-ref first a string)))  
        (hash-ref first a string)]
      [(eqv? string (hash-ref first a string))   
        (most-recent a second)]
    )
  )
)


(define (compare-same-length x y)
  (let ([first (if (not(empty? x)) (car x) #t)]
        [second (if (not(empty? x)) (cdr x) #t)]
        [third (if (not(empty? y)) (car y) #t)]
        [fourth (if (not(empty? y)) (cdr y) #t)]
        [fifth (list 'if '% x y)])

    (cond
      [(or (eqv? third 'quote) (eqv? first 'quote))  
        fifth]
      [(or (and (or (and (eqv? third 'λ) (eqv? first 'lambda)) (and (eqv? third 'lambda) (eqv? first 'λ))) (= (length (car fourth)) (length (car second))))
       (and (and (eqv? third 'λ) (eqv? first 'λ)) (= (length (car fourth)))))
         (lambda1 second fourth 'λ '() '())]
      [(or (and (or (and (eqv? third 'λ) (eqv? first 'lambda)) (and (eqv? third 'lambda) (eqv? first 'λ))) (not(= (length (car fourth)) (length (car second)))))
       (and (and (eqv? third 'λ) (eqv? first 'λ)) (not(= (length (car fourth)))))
       (and (and (eqv? third 'lambda) (eqv? first 'lambda)) (not(= (length (car fourth)) (length (car second))))))
         fifth]
      [(and (and (eqv? third 'lambda) (eqv? first 'lambda)) (= (length (car fourth)) (length (car second))))
        (lambda1 second fourth 'lambda '() '())]
      [(or (or (eqv? third 'λ) (eqv? third 'lambda)) (or (eqv? first 'λ) (eqv? first 'lambda))) 
        fifth]
      [(and (eqv? third 'if) (eqv? first 'if))  
        (normal-same-length x y)]
      [(or (eqv? third 'if) (eqv? first 'if))  
        fifth]
      [else (normal-same-length x y)]
    )
  )
)


(define (lambda-same-length x y xList yList)
  (let ([xReplace (switcheroo x xList #t)]
        [first (if (not(empty? x)) (car x) #t)]
        [second (if (not(empty? x)) (cdr x) #t)]
        [yReplace (switcheroo y yList #t)]
        [third (if (not(empty? y)) (car y) #t)]
        [fourth (if (not(empty? y)) (cdr y) #t)])

    (cond
      [(or (eqv? third 'quote) (eqv? first 'quote))
        (if (eqv? y x)  
          x (list 'if '% xReplace yReplace))]
      [(or (and(or (and (eqv? third 'λ) (eqv? first 'lambda)) (and (eqv? third 'lambda) (eqv? first 'λ))) (= (length (car fourth)) (length (car second))))
        (and (and (eqv? third 'λ) (eqv? first 'λ)) (= (length (car fourth)) (length (car second)))))
          (lambda1 second fourth 'λ xList yList)]
      [(or (and(or (and (eqv? third 'λ) (eqv? first 'lambda)) (and (eqv? third 'lambda) (eqv? first 'λ))) (not(= (length (car fourth)) (length (car second)))))
        (and (and (eqv? third 'λ) (eqv? first 'λ)) (not(= (length (car fourth)) (length (car second))))))
          (list 'if '% xReplace yReplace)]
      [(and (and (eqv? third 'lambda) (eqv? first 'lambda)) (= (length (car fourth)) (length (car second))))
        (lambda1 second fourth 'lambda xList yList)]
      [(and (and (eqv? third 'lambda) (eqv? first 'lambda)) (not(= (length (car fourth)) (length (car second)))))
        (list 'if '% xReplace yReplace)]
      [(or (or (eqv? third 'λ) (eqv? third 'lambda)) (or (eqv? first 'λ) (eqv? first 'lambda)))  
        (list 'if '% xReplace yReplace)]
      [(and (equal? third 'if) (equal? first 'if))  
        (cons 'if (lambda-processor second fourth xList yList))]
      [(or (equal? third 'if) (equal? first 'if))  
        (list 'if '% xReplace yReplace)]
      [else (lambda-processor x y xList yList)]
    )
  )
)


(define (normal-same-length x y)
  (let ([first (if (not(empty? x)) (car x) #t)]
        [second (if (not(empty? x)) (cdr x) #t)]
        [third (if (not(empty? y)) (car y) #t)]
        [fourth (if (not(empty? y)) (cdr y) #t)]
        [mt '()])

    (cond
      [(and (empty? y) (empty? x))  mt]
      [(and (boolean? third) (boolean? first))  
        (cons (if first '% '(not %)) (normal-same-length second fourth))]
      [(eqv? third first)  
        (cons first (normal-same-length second fourth))]
      [else 
        (cond
          [(and (list? third) (list? first) (= (length third) (length first)))
            (cons (compare-same-length first third) (normal-same-length second fourth))]
          [(and (list? third) (list? first))  
            (cons (list 'if '% first third) (normal-same-length second fourth))]
          [else (cons (list 'if '% first third) (normal-same-length second fourth))]
        )
      ]
    )
  )
)


(define (lambda-processor x y xList yList)
  (if (and (empty? x) (empty? y))  '()
    (let ([first (if (not(empty? x)) (car x) #t)]
          [second (if (not(empty? x)) (cdr x) #t)]
          [xNew (if (eqv? (most-recent (car x) xList) "Not Found1") (car x) (most-recent (car x) xList))]
          [third (if (not(empty? y)) (car y) #t)]
          [yNew (if (eqv? (most-recent (car y) yList) "Not Found1") (car y) (most-recent (car y) yList))]
          [fourth (if (not(empty? y)) (cdr y) #t)])

      (cond
        [(and (boolean? third) (boolean? first))   
          (cons (if first '% '(not %)) (lambda-processor second fourth xList yList))]
        [(eqv? yNew xNew)  
          (cons xNew (lambda-processor second fourth xList yList))]
        [(and (and (list? yNew) (list? xNew)) (not(= (length third) (length first))))
          (cons (list 'if '% (switcheroo first xList #t) (switcheroo third yList #t)) (lambda-processor second fourth xList yList))]
        [(and (and (list? yNew) (list? xNew)) (= (length third) (length first)))
          (cons (lambda-same-length first third xList yList) (lambda-processor second fourth xList yList))]
        [(or (list? xNew) (list? yNew))  
          (list 'if '% (if (list? x) (switcheroo x xList #t) xNew) (if (list? y) (switcheroo y yList #t) yNew))]
        [else (cons (list 'if '% xNew yNew) (lambda-processor second fourth xList yList))]
      )
    )
  )
)


(define (lambda1 x y lambda xList yList)
  (let ([first (car (cdr x))]
        [second (car (cdr y))]
        [third (cons (create-xList (car x) (car y)) xList)]
        [fourth (cons (create-yList (car x) (car y)) yList)]
        [fifth (if (not(empty? x)) (car x) #t)]
        [sixth (if (not(empty? y)) (car y) #t)])

    (list lambda (lambda3 fifth sixth) (lambda2 first second third fourth))
  )
)


(define (lambda2 x y xList yList)
  (let ([yNew (if (eqv? (most-recent y yList) "Not Found1")  y (most-recent y yList))]
        [xNew (if (eqv? (most-recent x xList) "Not Found1")  x (most-recent x xList))])

    (cond
      [(and (boolean? y) (boolean? x))  
        (if x '% '(not %))]
      [(eqv? yNew xNew) yNew]
      [(and (list? y) (list? x) (not (= (length y) (length x))))  
        (list 'if '% (switcheroo x xList #t) (switcheroo y yList #t))]
      [(and (list? y) (list? x) (= (length y) (length x)))  
        (lambda-same-length x y xList yList)]
      [(or (not (list? y)) (not (list? x)))
        (list 'if '% (if (list? x)  (switcheroo x xList #t) xNew) (if (list? y)  (switcheroo y yList #t) yNew))
      ]
    )
  )
)


(define (lambda3 x y)
  (let ([first (if (not(empty? x)) (car x) #t)]
        [second (if (not(empty? x)) (cdr x) #t)]
        [third (if (not(empty? y)) (car y) #t)]
        [fourth (if (not(empty? y)) (cdr y) #t)]
        [mt '()])

    (cond
      [(and (empty? y) (empty? x))  mt]
      [(not (eqv? third first))  
        (cons (string->symbol (string-append (symbol->string first) "!" (symbol->string third))) (lambda3 second fourth))]
      [else (cons first (lambda3 second fourth))]
    )
  )
)


(define (expr-compare x y)
  (let ([first (list 'if '% x y)])

    (cond 
      [(eqv? y x)  x]
      [(and (list? y) (list? x) (= (length y) (length x)))  
        (compare-same-length x y)]
      [(and (boolean? y) (boolean? x))  
        (if x '% '(not %))]
      [(and (list? y) (list? x) (not (= (length y) (length x))))  first]
      [(or (not (list? y)) (not (list? x)))  first]
    )
  )
)


; Question 2: test-expr-compare

(define (test-expr-compare x y)
  (and (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y)))) (equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y)))))
)


; Question 3: test-expr-x and test-expr-y

(define test-expr-x `(cons ((lambda (s) (+ 3 s)) 5) '(if (eqv? d (g λ)) 88 8)))
(define test-expr-y `(cons ((lambda (m) (* 4 m)) 6) '(if (eqv? d (g lambda)) 1 11)))