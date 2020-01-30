(define (stream-ref stream n)
	(if (= n 0)
		(stream-car stream)
		(stream-ref (stream-cdr stream) (- n 1))))

(define (stream-map proc stream)
	(if (stream-null? stream)
		the-empty-stream
		(cons-stream (proc (stream-car stream))
								 (stream-map proc (stream-cdr stream)))))

(define (stream-filter test stream)
	(cond ((stream-null? stream) the-empty-stream)
				((test (stream-car stream)) (cons-stream (stream-car stream)
																								 (stream-filter test
																																(stream-cdr stream))))
				(else (stream-filter test (stream-cdr stream)))))

(define (stream-enumerate a b)
	(if (= a b)
		(cons-stream a the-empty-stream)
		(cons-stream a (stream-enumerate (+ a 1) b))))

(define (stream-for-each proc stream)
	(if (stream-null? stream)
		'done
		(begin (proc (stream-car stream))
					 (stream-for-each proc (stream-cdr stream)))))

(define (display-stream stream)
	(stream-for-each (lambda (x) (display x) (newline))
									 stream))

(define ones (cons-stream 1 ones))
(define (integers-starting-from n)
	(cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
