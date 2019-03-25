(#%require (only racket/base random))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())

(define stream-null? null?)

(define-syntax stream
   (syntax-rules ()
     ((stream) the-empty-stream)
     ((stream x y ...) (cons-stream x (stream y ...)))))

; referring to the nth element of a stream

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))


(define (stream-for-each proc s)
  (if (not (stream-null? s))
      (begin
	(proc (stream-car s))
	(stream-for-each proc (stream-cdr s)))))

(define (stream-reverse s)
  (define (aux remaining so-far)
    (cond ((stream-null? remaining) so-far)
          (else (aux
                 (stream-cdr remaining)
                 (cons-stream (stream-car remaining)
                              so-far)))))
  (aux s the-empty-stream))

(define (stream-append s t)
  (cond ((stream-null? s) t)
        (else (cons-stream
               (stream-car s)
               (stream-append (stream-cdr s) t)))))

(define (take n s)
  (cond ((zero? n) '())
        (else (cons (stream-car s)
                    (take (- n 1) (stream-cdr s))))))

(define (drop n s)
  (cond ((zero? n) s)
        ((stream-null? s) the-empty-stream)
        (else (drop (- n 1) (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
          (cons-stream (stream-car stream)
                       (stream-filter pred
                         (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (list->stream objs)
  (define list->stream
    (lambda (objs)
      (if (null? objs)
          the-empty-stream
          (cons-stream (car objs) (list->stream (cdr objs))))))
  (if (not (list? objs))
      '(non-list argument)
      (list->stream objs)))

;Double-Ended Queue
(define (make-deque front rear lenF lenR)
  (define c 2)
  (define (dispatch m)
    (cond ((eq? m 'empty?) (make-deque the-empty-stream the-empty-stream 0 0))
          ((eq? m 'isEmpty?) (zero? (+ lenF lenR)))

          ((eq? m 'display) (display-stream (stream-append front rear)))

          ;insert, inspect, and remove the front element
          ((eq? m 'cons) (lambda (x) (make-deque (cons-stream x front) rear (+ lenF 1) lenR)))
          ((eq? m 'head) (cond ((and (stream-null? front) (stream-null? rear)) (display 'error:empty-stream))
                               ((and (stream-null? front) ((not (stream-null? rear)) (stream-car rear))))
                               (else (stream-car front))))
          ((eq? m 'tail) (cond ((and (stream-null? front) (stream-null? rear)) (display 'error:empty-stream))
                               ((and (stream-null? front) ((not (stream-null? rear)) the-empty-stream)))
                               (else (make-deque (stream-cdr front) rear (- lenF 1) lenR))))

          ;insert, inspect, and remove the rear element
          ((eq? m 'snoc) (lambda (x) (make-deque front (cons-stream x rear) lenF (+ lenR 1))))
          ((eq? m 'last) (cond ((and (stream-null? front) (stream-null? rear)) (display 'error:empty-stream))
                               ((and (not (stream-null? front)) (stream-null? rear)) (stream-car front))
                               (else (stream-car rear))))
          ((eq? m 'init) (cond ((and (stream-null? front) (stream-null? rear)) (display 'error:empty-stream))
                               ((and (not (stream-null? front)) (stream-null? rear)) the-empty-stream)
                               (else (make-deque front (stream-cdr rear) lenF (- lenR 1)))))))
          
  (cond ((> lenF (+ (* c lenR) 1)) (make-deque (take (/ (+ lenF lenR) 2) front)
                                               (stream-append rear (stream-reverse (drop (/ (+ lenF lenR) 2) front)))
                                               (/ (+ lenF lenR) 2) (- (+ lenF lenR) (/ (+ lenF lenR) 2))))
        ((> lenR (+ (* c lenF) 1)) (make-deque (stream-append front (stream-reverse (drop (- (+ lenF lenR) (/ (+ lenF lenR) 2)) rear)))
                                               (take (- (+ lenF lenR) (/ (+ lenF lenR) 2)) rear) (/ (+ lenF lenR) 2) (- (+ lenF lenR) (/ (+ lenF lenR) 2))))
        (else dispatch)))

;convert char to integer or integer to char
(define (convert c)
  (cond ((char? c) (char->integer c))
        (else (integer->char c))))

;letter is shifted 13 places according to ASCII table
;uppercase 65-90
;lowercase 97-122
(define (shift-rot13 c n)
  (cond ((char-alphabetic? c)
         (cond ((> (+ (convert c) 13) n) (convert (- (+ (convert c) 13) 26)))
                (else (convert (+ (convert c) 13)))))))

(define (shift-caesar c n)
  (cond ((char-alphabetic? c)
         (cond ((char-upper-case? c) (convert (+ (modulo (+ (- (convert c) 65) n) 26) 65)))
               ((char-lower-case? c) (convert (+ (modulo (+ (- (convert c) 97) n) 26) 97)))))
         (else c))) 

(define (shift-atbash c n)
  (cond((char-alphabetic? c)
        (char-downcase (convert (+ (modulo (* -1 (+ (- (convert c) n) 1)) 26) n))))))
;                                                   |________________|     
;                                       convert to ascii and map to 0....26       
;                                          |___________________________|
;                                                 same as -(x+1)
;                                  |________________________________________|
;                                         taking -(x+1) mod m
;                               |______________________________________________|
;                                 map it back to ascii
;                     |_________________________________________________________|
;                      convert back to the character from ascii

(define (case-rot13 c)
  (cond ((char-upper-case? c) (shift-rot13 c 90))
         ((char-lower-case? c) (shift-rot13 c 122))
         (else c)))

(define (case-atbash c)
  (cond ((char-upper-case? c) (shift-atbash c 65))
         ((char-lower-case? c) (shift-atbash c 97))
         (else c)))
          
(define (rot13 s)
  (define (rot13-aux s result)
    (cond ((stream-null? s) result)
          (else (rot13-aux (stream-cdr s) (cons-stream (case-rot13 (stream-car s)) result)))))
  (rot13-aux s the-empty-stream))

(define (caesar s proc n)
  (define (caesar-aux s ptoc result)
    (cond ((stream-null? s) result)
          (else (cond ((or (eq? proc -) (eq? proc +)) (caesar-aux (stream-cdr s) proc (cons-stream (shift-caesar (stream-car s) (proc n)) result)))
                      (else 'Please_enter_minus_or_plus_sign.)))))
  (caesar-aux s proc the-empty-stream))

(define (atbash s)
  (define (atbash-aux s result)
    (cond ((stream-null? s) result)
          (else (atbash-aux (stream-cdr s) (cons-stream (case-atbash (stream-car s)) result)))))
  (atbash-aux s the-empty-stream))

(define (display-line x)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))
   
(define (read-file filename)
  (let ((p (open-input-file filename)))
    (let f ((x (read-char p)))
      (if (eof-object? x)
          (begin
            (close-input-port p)
            '())
          (cons x (f (read-char p)))))))

(define (process lst)
  (apply append (map (lambda (e)
                       (if (char? e)
                           (list e)
                           e))
                     lst)))                       


(define (pick n)
  (cond ((= (random n) 0) 'atbash)
        ((= (random n) 1) 'caesar)
        ((= (random n) 2) 'rot13)
        (else (pick n))))

(define (proc n)
  (if (= (random n) 1) - +))

(define (rand n)
  (define (rand-aux n result)
    (cond ((> result 0) result)
          (else (rand n))))
  (rand-aux n (random n)))
  
(define (encrypt str)
  (define q1 (make-deque (stream (pick 3)) (stream (pick 3)) 2 1))
  (cond ((eq? (q1 'head) 'atbash)
         (cond ((eq? (q1 'last) 'caesar) (display-stream (caesar (atbash str) (proc 2) (rand 26))))
               ((eq? (q1 'last) 'rot13) (display-stream (rot13 (atbash str))))
               (else (display-stream (atbash str)))))           
        ((eq? (q1 'head) 'caesar)
         (cond ((eq? (q1 'last) 'atbash) (display-stream (atbash (caesar str (proc 2) (rand 26)))))
               ((eq? (q1 'last) 'rot13) (display-stream (rot13 (caesar str (proc 2) (rand 26)))))
               (else (display-stream (caesar str (proc 2) (random 26))))))
        ((eq? (q1 'head) 'rot13)
         (cond ((eq? (q1 'last) 'atbash) (display-stream (atbash (rot13 str))))
               ((eq? (q1 'last) 'caesar) (display-stream (caesar (rot13 str) (proc 2) (rand 26))))
               (else (display-stream (rot13 str)))))))

(encrypt (list->stream (process (read-file "file.txt"))))

