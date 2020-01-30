; returns a list of numbers from low-high recursively
(define (enumerate-rec low high)
	(if (> low high) ()
			(cons low (enumerate (+ low 1) high))))

; enumerates iteratively
(define (enumerate-iter low high)
	(define (iter i result)
		(if (< i low) result
				(iter (- i 1) (cons i result))))
	(iter high ()))

(define enumerate enumerate-iter)

; accumulator for a list
(define (accumulate op start seq)
	(if (null? seq) start
			(op (car seq) (accumulate op start (cdr seq)))))

; accumulator for a list of lists. applies the operation to the corresponding elements of each list
(define (accumulate-n op start seqs)
	(if (null? (car seqs))
			()
			(cons (accumulate op start (map car seqs))
						(accumulate-n op start (map cdr seqs)))))

; vector is a sequence
; matrix is a sequence of sequences
; eg. ((1 2 3)(4 5 6)(7 8 9))

; dot product of two vectors
(define (dot-product v w)
	(accumulate + 0 (map * v w)))

; multiply a matrix and vector using the relation:
; result-row[i] = dot-product m-row[i] v
(define (matrix-*-vector m v)
	(map (lambda (r) (dot-product r v)) m))

; transposing a matrix
(define (transpose m)
	(accumulate-n cons () m))

; matrix multiplication:
; 
(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
		(map (lambda (r) (map (lambda (c) (dot-product r c)) cols)) m)))

; flatten a nested list by one level
(define (flatten l)
	(accumulate append () l))

(define (list-max l)
	(accumulate max (car l) l))
