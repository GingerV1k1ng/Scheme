;Made by Kurt Nørmark
(load "pp-standard-functions.scm")
(load "pp-standard-higher-order-functions.scm")
(load "music-basis-r5rs.scm")

;music element, that can either be a note, pause, sequential music element or a parallel music element.
(define(musicElement? element)
  (or(note? element)
     (pause? element)
     (sequentialMusicElement element)
     (parallelMusicElement element)))

;Got help by Charlie Byrdam to make the instrument part of note
(define instrument '(piano organ guitar violin flute trumpet helicopter telephone))

;A note in the system, it has the values pitch, duration and channel.
(define(note pitch duration channel)
  (cond  ((not (pitch? pitch)) (error "not a valid pitch"))
         ((not (duration? duration)) (error "not a valid duration"))
         ((not (channel? channel)) (error "not a valid intrument")))
  (list 'note channel pitch duration))

;Defines the pause.
(define(pause duration)
  (if (not (duration? duration))
      (error "Not a valid duration")
      (list 'pause duration)))

;Checks if the element is a pause
(define(pause? lst)
  (if (not (pair? lst))
      #f
      (eq? (car lst) 'pause)))

;Checks if the element is a pitch
(define(pitch? value)
  (and (integer? value) (>= value 0) (<= value 127)))

;Checks if the element is a duration
(define(duration? value)
  (and (integer? value) (>= value 0)))

;Checks if the element is a note
(define(note? lst)
  (if (not (pair? lst))
      #f
      (eq? (car lst) 'note)))

;find-in-list is made by Kurt Nørmark
(define(channel? value)
  (if (find-in-list (lambda(x) (eq? x value)) instrument)
      #t
      #f))

(define(sequentialMusicElement . listOfMusicElements)
  (cond
    ((find-in-list
      (lambda(x)
        (not (musicElement? x))) listOfMusicElements)
     (error "List contains invalid music elements")))
  (list 'sequentialMusicElement listOfMusicElements))

(define(parallelMusicElement . listOfMusicElements)
  (cond
    ((find-in-list
      (lambda(x)
        (not (musicElement? x))) listOfMusicElements)
     (error "List contains invalid music elements")))
  (list 'parallelMusicElement listOfMusicElements))

(define(sequentialMusicElement? lst)
  (if (not (pair? lst))
      #f
      (eq? (car lst) 'sequentialMusicElement)))

(define(parallelMusicElement? lst)
  (if (not (pair? lst))
      #f
      (eq? (car lst) 'parallelMusicElement)))

;Got help from Daniel Bolhuis with making of the scale, transposes and re-instrument functions
(define(scaling element val)
  (cond
    ((pause? element) (pause (+(get-duration element) val)))
    ((note? element) (note(get-pitch element) (*(get-duration element) val) (get-instrument element)))
    ((sequentialMusicElement? element) (apply sequentialMusicElement(scale-helper(cadr element) val )))
    ((parallelMusicElement? element) (apply parallelMusicElement(scale-helper(cadr element) val )))))

(define(scale-helper element val)
  (if (null? element)
      '()
      (cons (scaling (car element) val) (scale-helper(cdr element) val))))

(define(transposes element val)
  (cond
    ((pause? element) (error "Can't change pitch on a pause"))
    ((note? element) (note (+(get-pitch element) val) (get-duration element) (get-instrument element)))
    ((sequentialMusicElement? element) (apply sequentialMusicElement(transposes-helper(cdr element) val)))
    ((parallelMusicElement? element) (apply parallelMusicElement(transposes-helper(cadr element) val)))))

(define(transposes-helper element val)
  (if (null? element)
      '()
      (cons (transposes (car element) val) (transposes-helper(cdr element) val))))
    
(define(re-instrument element instrument)
  (cond
    ((pause? element) (error "Can't change instrument on a pause"))
    ((note? element) (note (get-pitch element) (get-duration element) instrument))
    ((sequentialMusicElement? element) (apply sequentialMusicElement(instrument-helper(cadr element) instrument)))
    ((parallelMusicElement? element) (apply parallelMusicElement(instrument-helper(cadr element) instrument)))))

(define(instrument-helper element instrument)
  (if (null? element)
      '()
      (cons(re-instrument (car element) instrument) (instrument-helper(cdr element) instrument))))
    
(define(get-duration element)
  (cond
    ((pause? element) (cdr element))
    ((note? element) (car(cdddr element)))
    ((sequentialMusicElement? element) (get-duration-sequential(cadr element)))
    ((parallelMusicElement? element) (get-duration-parallel (cadr element)))))
    
(define(get-instrument element)
  (if(note? element) (car(cdr element))))

(define(get-pitch element)
  (if (note? element) (car(cddr element))))

(define(get-duration-sequential element)
  (if (null? element)
      0
      (+ (get-duration (car element)) (get-duration-sequential(cdr element)))))

(define(get-duration-parallel element)
  (if (null? element)
      0
      (max (get-duration (car element)) (get-duration-parallel(cdr element)))))

(define(linearizes element time)
  (cond
    ((pause? element) '() )
    ((note? element) (note-abs-time-with-duration time (channelToInstrument(get-instrument element)) (get-pitch element) 88 (get-duration element))) 
    ((sequentialMusicElement? element) (apply sequentialMusicElement(linearizes-helper-sequential(cadr element) time)))
    ((parallelMusicElement? element)  )))

(define(linearizes-helper-sequential element time)
  (if (null? element)
      '()
      (append (linearizes(car element) (+ (get-duration (car element)) time)) (linearizes-helper-sequential (cddr element) (+ (get-duration (cddr element)) time)))))

(define(sum element time)
  (+ (get-duration element) time))

(define(linearizes-helper-parallel element)
  (if (null? element)
      '()
      (append)))

(define(channelToInstrument instrument)
  (cond
    ((eq? instrument 'piano) 1)
    ((eq? instrument 'organ) 2)
    ((eq? instrument 'guitar) 3)
    ((eq? instrument 'violin) 4)
    ((eq? instrument 'flute) 5)
    ((eq? instrument 'trumpet) 6)
    ((eq? instrument 'helicopter) 7)
    ((eq? instrument 'telephone) 8)))

(define(monophonic? element)
  (cons
   ((pause? element) )
   ((node? element) )
   ((seqentialMusicElement? element) )
   ((parallelMusicElement? element) )))

;Definetions helping with coding
(define p(pause 50))
(define n(note 22 50 'Piano))
(define sequentialLst(sequentialMusicElement n n n n n n n n n n n))
(define parallelLst(parallelMusicElement n n n n n n n n n n n))