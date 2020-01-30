(define (install-rectangular-package export)
	(define (tag data) (attach-tag 'rectangular data))
	(define (real-part z) (car z))
	(define (imag-part z) (cdr z))
	(define (magnitude z)
		(sqrt (+ (square (real-part z)) (square (imag-part z)))))
	(define (angle z) (atan (imag-part z) (real-part z)))

	(export 'make 'rectangular
					(lambda (real imag)
						(tag (cons real imag))))
	(export 'magnitude 'rectangular magnitude)
	(export 'angle 'rectangular angle)
	(export 'real-part 'rectangular real-part)
	(export 'imag-part 'rectangular imag-part)

	(display "rectangular package installed")
	(newline)
	'ok)
