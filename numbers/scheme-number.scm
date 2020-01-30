(define (install-scheme-number-package export)

	(define (add . n)
		(let ((args n))
			(apply + args)))

	(define (sub n1 . n)
		(let ((args (cons n1 n)))
			(apply - args)))

	(define (mul . n)
		(let ((args n))
			(apply * args)))

	(define (equ? n1 n2) (= n1 n2))
	(define (print n) (display n) (newline))

	(export 'add 'scheme-number add)
	(export 'sub 'scheme-number sub)
	(export 'mul 'scheme-number mul)
	(export 'equ? 'scheme-number equ?)
	(export 'print 'scheme-number print)

	(display "scheme number package installed")
	(newline)
	'ok)
