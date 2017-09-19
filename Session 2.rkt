
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
  (if null? (cdr last)
      
       
           