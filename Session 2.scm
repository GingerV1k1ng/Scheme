
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define square (lambda (x) (* x x)))

(define incr (lambda (x) (+ x 1)))

(define (fac n) (if (= n 0) 1 (* n (fac (- n 1)))))

(define (self-compose-2 f)
  (lambda (x)
    (f (f x))))

(define (reduce-right f lst)
  (if (null? (cdr lst))
      (car)))

(define (self-compose* f n)
  (cond ((= n 1) f)
        ((> n 1) (lambda (x) (f ((self-compose* f (-n 1)) x))))
        (else (error "error"))))

(define (self-compose* f n)
  (if (= n 0)
      (lambda(x) x)
      (compose f (self-compose* f (- n 1)))))

(define (compose* f-list)
  (cond ((= (length f-list) 1)
         (car f-list))
        ((> (length f-list) 1)
         (lambda (x) ((car f-list) ((compose* (cdr f-list)) x))))
        (else (error "error"))))

(define (replicate-to-length lst n)
  (replicate-to-length-iter lst lst n))

(define (replicate-to-length-iter lst len count)
  (cond ((= n 0) '())
        ((null? lst-rmn) (replicate-to-length-impl lst lst n))
        (else (cons (car lst-rmn) (replicate-to-length-impl (cdr lst-rmn) lst (- n 1))))))
      
(define (make-comparator x y)
  (lambda (x y)
    (if (f x y)
        -1
        (if (g x y)
            1
            0))))
  

(define (lt x y)
  (if (< x y)
  #t))

(define (gt x y)
  (if (> x y)
      #t))

(define (make-le-qe-gt cmp)
  (list
   (lambda (x y) (= -1 (cmp x y)))
   (lambda (x y) (= 0  (cmp x y)))
   (lambda (x y) (= 1  (cmp x y)))
   )
  )

(define (derivative f)
  (lambda (x)
    (let ((h 0.000001))
      (/ (- (f (+ x h)) (f x)) h)
      )
    ))

(define (for-all lst p)
  (cond [(null? lst) #t]
        [(p (car lst)) (forall (cdr lst) p)]
        [else #f]))
   
       
         