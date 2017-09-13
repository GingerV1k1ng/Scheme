
(define (test? x) (+ 1 x))

(define l (list 1 2 3))
(define lst (list a . b))

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
(define (my-every-second-element lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (list(car lst)))
        (else (cons (car lst) (my-every-second-element(cdr (cdr lst)))))))

;;;Gives a specific number of tails, from at list, depending on what 'n' is
(define (my-list-tail lst n)
  (if(= n 0)lst
     (my-list-tail(cdr lst) (- n 1))))
;;Gives you a specific number of the first element of a list. 
(define (my-list-prefix lst n)
  (if( or (null? lst) (= n 0)) '()
     (cons (car lst) (my-list-prefix(cdr lst) (- n 1)))))

