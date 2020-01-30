(define (install-rational-package export)
	(define (tag data) (attach-tag 'rational data))

	; constructor
	(define (make-rat n d)
		(let ((g (gcd n d)))
			(if (= 0 d)
				(error "Denominator cannot be 0")
				(tag (cons (/ n g) (/ d g))))))

	; selectors
	(define (numer r) (car r))
	(define (denom r) (cdr r))

	; generic procedure definitions
	(define (add . r)
		(let ((args r))
			(let ((new-denom (apply lcm (map denom r))))
				(make-rat (apply + (map (lambda (r)
																	(/ (* (numer r) new-denom) (denom r)))
																args))
									new-denom))))
	
	(define (sub r1 . r)
		(let ((args (cons r1 r)))
			(let ((new-denom (apply lcm (map denom r))))
				(make-rat (apply - (map (lambda (r)
																	(/ (* (numer r) new-denom) (denom r)))
																args))
									new-denom))))
	
	(define (mul . r)
		(let ((args r))
			(make-rat (apply * (map numer args))
								(apply * (map denom args)))))
	
	(define (div r1 . r)
		(make-rat (* (numer r1) (apply * (map denom r)))
							(* (denom r1) (apply * (map numer r)))))

	(define (equ? r1 r2)
		(= (* (numer r1) (denom r2))
			 (* (numer r2) (denom r1))))

	(define (print r)
		(display (numer r))
		(display "/")
		(display (denom r)))

	; export generic procedures
	(export 'numer 'rational numer)
	(export 'denom 'rational denom)
	(export 'add 'rational add)
	(export 'sub 'rational sub)
	(export 'mul 'rational mul)
	(export 'div 'rational div)
	(export 'equ? 'rational equ?)
	(export 'print 'rational print)
	(export 'make 'rational make-rat)

	(display "rational package installed")
	(newline)
	'ok)
