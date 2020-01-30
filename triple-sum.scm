(define (sum? triple s)
	(= s (accumulate + 0 triple)))

(define (generate-triples n)
	(accumulate append
							()
							(accumulate append
													()
													(map (lambda (i)
																 (map (lambda (j)
																				(map (lambda (k)
																							 (list k j i))
																						 (enumerate 1 (- j 1))))
																			(enumerate 1 (- i 1))))
															 (enumerate 1 n)))))

(define (triplet-sum s n)
	(filter (lambda (x) (sum? x s))
					(generate-triples n)))
					
