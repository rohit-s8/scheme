(define (install-complex-package export)
	; local proc table to be used for generic operations
	(define proc-table (make-table 2))

	(define (put op arg-types proc)
		(insert-table! proc-table (list op arg-types) proc))

	(define (get op arg-types)
		(lookup-table proc-table (list op arg-types)))

	(define (generic-op op . args)
		(let ((arg-type (car (map get-tag args))))
			(let ((proc (get op arg-type)))
				(if proc
					(apply proc (map get-content args))
					(error "Required operation does not exist" op arg-type)))))

	(define (tag data) (attach-tag 'complex data))

	; Install polar and rectangular packages
	(load "complex-polar")
	(install-polar-package put)
	(load "complex-rectangular")
	(install-rectangular-package put)

	; import constructors
	(define make-from-mag-ang (get 'make 'polar))
	(define make-from-real-imag (get 'make 'rectangular))

	(define (make-polar mag ang)
		(tag (make-from-mag-ang mag ang)))

	(define (make-rectangular real imag)
		(tag (make-from-real-imag real imag)))

	; generic selector definitions
	(define (real-part z) (generic-op 'real-part z))
	(define (imag-part z) (generic-op 'imag-part z))
	(define (magnitude z) (generic-op 'magnitude z))
	(define (angle z) (generic-op 'angle z))

	; generic procedure definitions
	(define (add . z)
		(let ((args z))
			(make-rectangular (apply + (map real-part args))
												(apply + (map imag-part args)))))

	(define (sub z1 . z)
		(let ((args (cons z1 z)))
			(make-rectangular (apply - (map real-part args))
												(apply - (map imag-part args)))))

	(define (mul . z)
		(let ((args z))
			(make-polar (apply * (map magnitude args))
									(apply + (map angle args)))))

	(define (div z1 . z)
		(let ((args (cons z1 z)))
			(make-polar (apply / (map magnitude args))
									(apply - (map angle args)))))

	(define (equ? z1 z2)
		(and (= (real-part z1) (real-part z2))
				 (= (imag-part z1) (imag-part z2))))

	(define (print z)
		(display (real-part z))
		(display " + i*")
		(display (imag-part z))
		(newline))

	; export generic procedures
	(export 'real-part 'complex real-part)
	(export 'imag-part 'complex imag-part)
	(export 'magnitude 'complex magnitude)
	(export 'angle 'complex angle)
	(export 'add 'complex add)
	(export 'sub 'complex sub)
	(export 'mul 'complex mul)
	(export 'div 'complex div)
	(export 'equ? 'complex equ?)
	(export 'print 'complex print)
	(export 'make-polar 'complex make-polar)
	(export 'make-rectangular 'complex make-rectangular)

	(display "complex package installed")
	(newline)
	'ok)
