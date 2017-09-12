
(define (test? x) (+ 1 x))

(define l (list 1 2 3))
(define li (list a . b))

(define (myList? x) (cdr((myList?(x)))))

;;;Test if it is a proper list.
(define (my-proper-list? lst)
  (if (null? lst)
      #t
      (if (pair? lst)
          (my-proper-list? (cdr lst))
          #f)))
;;;Test if it is a proper list, but a more neat solution.
(define (proper-list? lst)
  (or (null? lst)
      (and (pair? lst) (proper-list?(cdr lst)))))

;;;Gives you every second element of a list
(define (every-second-element lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (list(car lst)))
        (else (cons (car lst) (every-second-element(cdr (cdr lst)))))))

