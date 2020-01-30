(load "table")
(load "tag")

(define (identity x) x)

;; initialize procedure table
(define proc-table (make-table 2))
(define (put op arg-types proc)
	(insert-table! proc-table (list op arg-types) proc))

(define (get op arg-types)
	(lookup-table proc-table (list op arg-types)))

;; initializing type system
(load "types")
(define ts (make-type-system))

; wrappers
(define (put-type type priority)
	(add-type-ts! ts type priority))

(define (put-raise-proc init-type target-type proc)
	(add-raise-proc-ts! ts init-type target-type proc))

(define (put-drop-proc type proc)
	(add-drop-proc-ts! ts type proc))

(define (raise-arg arg)
	(raise-arg-ts ts arg))

(define (homogenize list-of-args)
	(homogenize-ts ts list-of-args))

(define (simplify data)
	(simplify-ts ts data))

; installing types
(put-type 'boolean 0)
(put-type 'scheme-number 1)
(put-type 'rational 2)
(put-type 'complex 3)

; raise procedures
(define (number->rational n) (make-rat n 1))
(define (number->complex n) (make-rectangular n 0))
(define (rational->complex r)
	(make-rectangular (/ (numerator r) (denominator r)) 0))

; install raise procedures
(put-raise-proc 'scheme-number 'rational number->rational)
(put-raise-proc 'scheme-number 'complex number->complex)
(put-raise-proc 'rational 'complex rational->complex)

; drop procedures
(define (drop-complex z)
	(if (= 0 (imag-part z))
		(real-part z)
		z))

(define (drop-rational r)
	(if (= 1 (denominator r))
		(numerator r)
		r))

; install drop procedures
(put-drop-proc 'complex drop-complex)
(put-drop-proc 'rational drop-rational)
(put-drop-proc 'scheme-number identity)
(put-drop-proc 'boolean identity)

;; generic operation interface
(define (generic-op op . args)
	(let ((hom-args (homogenize args)))
		(let ((arg-type (if (null? hom-args)
											'scheme-number
											(get-tag (car hom-args)))))
			(let ((proc (get op arg-type)))
				(if proc
					(simplify (apply proc (map get-content hom-args)))
					(if (not (eq? arg-type 'complex))
						(apply generic-op op (cons (raise-arg (car hom-args))
																			 (cdr hom-args)))
						(error "Required operation does not exist" op arg-type)))))))

;; standard generic operations
(define (add . n)
	(apply generic-op 'add n))

(define (sub n1 . n)
	(apply generic-op 'sub n1 n))

(define (mul . n)
	(apply generic-op 'mul n))

(define (div n1 . n)
	(apply generic-op 'div n1 n))

(define (equ? n1 n2) (generic-op 'equ? n1 n2))
(define (print n) (generic-op 'print n))


;; install complex number package
(load "complex")
(install-complex-package put)

; complex constructors
(define make-polar
	(lambda (mag ang)
		(if (and (scheme-number? mag) (scheme-number? ang))
			((get 'make-polar 'complex) mag ang)
			(error "MAKE-POLAR: arguments must be scheme-numbers" mag ang))))

(define make-rectangular 
	(lambda (real imag)
		(if (and (scheme-number? real) (scheme-number? imag))
			((get 'make-rectangular 'complex) real imag)
			(error "MAKE-RECTANGULAR arguments must be scheme-numbers"
						 real imag))))

(define (real-part z) (generic-op 'real-part z))
(define (imag-part z) (generic-op 'imag-part z))
(define (magnitude z) (generic-op 'magnitude z))
(define (angle z) (generic-op 'angle z))
(define (complex? z) (eq? (get-tag z) 'complex))

;; install rational number package
(load "rational")
(install-rational-package put)
(define make-rat
	(lambda (n d)
		(if (and (scheme-number? n) (scheme-number? d))
			((get 'make 'rational) n d)
			(error "MAKE-RAT: arguments must be scheme-numbers" n d))))

(define (numerator r) (generic-op 'numer r))
(define (denominator r) (generic-op 'denom r))
(define (rational? r) (eq? (get-tag r) 'rational))

;; install ordinary number package
(load "scheme-number")
(install-scheme-number-package put)
(put 'div 'scheme-number (lambda (n1 n2)
													 (simplify (make-rat n1 n2))))

(define (scheme-number? n) (eq? (get-tag n) 'scheme-number))
