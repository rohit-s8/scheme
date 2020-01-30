; Definitions for Points
(define (point x y) (cons x y))
(define (x-coord p) (car p))
(define (y-coord p) (cdr p))

; Definition for line segment
(define (line-segment p1 p2) (cons p1 p2))
(define (first-point l) (car l))
(define (second-point l) (cdr l))

; General procedures for lines
(define (mid-point l)
	(point (/ (+ (x-coord (first-point l))
						 	 (x-coord (second-point l)))
						2)
				 (/ (+ (y-coord (first-point l))
						   (y-coord (second-point l)))
						2)
					))

(define p1 (point 1 2))
(define p2 (point 0 0))
(define l1 (line-segment p1 p2))
(mid-point l1)
