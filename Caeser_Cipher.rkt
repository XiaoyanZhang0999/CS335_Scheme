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

(define (list->stream objs)
  (define list->stream
    (lambda (objs)
      (if (null? objs)
          the-empty-stream
          (cons-stream (car objs) (list->stream (cdr objs))))))
  (if (not (list? objs))
      '(non-list argument)
      (list->stream objs)))

;length of stream
(define (mylength s)
  (cond ((stream-null? s) 0)
	(else (+ 1 (mylength (stream-cdr s))))))

;convert char to integer or integer to char
(define (convert c)
  (cond ((char? c) (char->integer c))
        (else (integer->char c))))


;letter is shifted a places according to ASCII table
;uppercase 65-90
;lowercase 97-122
(define (shift c a)
  (cond ((char-alphabetic? c)
         (cond ((char-upper-case? c)
                (cond ((> (+ (convert c) a) 90) (convert (- (+ (convert c) a) 26)))
                      (else (convert (+ (convert c) a)))))
               ((char-lower-case? c)
                (cond ((> (+ (convert c) a) 122) (convert (- (+ (convert c) a) 26)))
                      (else (convert (+ (convert c) a)))))))
         (else c)))    

(define (caeser s a)
  (define (caeser-aux s result)
    (cond ((stream-null? s) result)
          (else (caeser-aux (stream-cdr s) (cons-stream (shift (stream-car s) a) result)))))
  (caeser-aux s the-empty-stream))

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

(define test (read-file "file.txt"))
(define str (list->stream (process test)))
(display-stream (stream-reverse (caeser str 7)))
