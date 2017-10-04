(define(return x) (tag 'done x))
(define(bounce x) (tag 'doing x))
(define(call thunk) (thunk))
(define(tag label thing) (cons label thing))
(define tag-of car)
(define tag-value cdr)
(define (sum lst res) (if (null? lst) (return res) (bounce (lambda () (sum (cdr lst) (+ (car lst) res)))))) 

(define(tag label thunk) (cons label thunk))
  
(define(seesaw thread-1 thread-2)
  (cond ((eqv? 'done (tag-of thread-1))
         (tag-value thread-1))
        ((eqv? 'doing (tag-of thread 1))
         (seesaw thread-2 (call (tag-value thread-1))))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define head car)

(define (tail stream) (force (cdr stream)))

(define empty-stream? null?)

(define the-empty-stream '())


(define (stream-section n stream)
  (cond ((= n 0) '())
        (else (cons (head stream)
              (stream-section 
                (- n 1)
               (tail stream))))))

(define (add-streams s1 s2)
 (let ((h1 (head s1))
       (h2 (head s2)))
   (cons-stream 
    (+ h1 h2)
    (add-streams (tail s1) (tail s2)))))


(define facts
  (cons-stream 1
               (combine-streams (tail nat-nums) facts)))

(define (combine-streams s1 s2)
  (let ((h1 (head s1))
        (h2 (head s2)))
    (cons-stream
     (* h1 h2)
     (add-streams (tail s1) (tail s2)))))

(define (map-stream f stream)
    (cond ((empty-stream? stream) the-empty-stream)
          (else (cons-stream (f (head stream)) (map-stream f (tail stream))))))

(define (improve-sqrt-guess guess x)
  (/ (+ guess (/ x guess)) 2))

