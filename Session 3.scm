(define (point x y)
  (letrec ((getx    (lambda () x))
           (gety    (lambda () y))
           (add     (lambda (p) 
                      (point 
                       (+ x (send 'getx p))
                       (+ y (send 'gety p)))))
           (type-of (lambda () 'point))
           (move (lambda (dx dy)
                   (point
                    (+ x dx)
                    (+ y dy))))
          )
    (lambda (message)
      (cond ((eq? message 'getx) getx)
            ((eq? message 'gety) gety)
            ((eq? message 'add)  add)
            ((eq? message 'type-of) type-of)
            ((eq? message 'move) move)
            (else (error "Message not understood"))))))

(define (rect p1 p2)
  (letrec (
           (type-of (lambda () 'rect))
           (move    (lambda (p10 p20)
                      (rect
                       (point
                        (+ (send 'getx p1) (send 'getx p10))
                        (+ (send 'gety p1) (send 'gety p10))
                        (point
                         (+ (send 'getx p2) (send 'getx p20))
                         (+ (send 'gety p2) (send 'gety p20))
                         ))))))))

(define (send message obj . par)
  (let ((method (obj message)))
    (apply method par)))


(define p1(point 1 3))