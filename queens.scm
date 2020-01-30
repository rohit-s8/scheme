(load "seqop")

; top-level solution
(define (queens n)
	(define (solutions k)
		(if (= k 0)
			(list empty-board)
			(filter (lambda (position) (safe? k position))
							(flatten (map (lambda (prev-sol)
															(map (lambda (new-row)
																		 (adjoin-position
																			 new-row k prev-sol))
																	 (enumerate 1 n)))
														(solutions (- k 1)))))))
	(solutions n))

(define empty-board (list))

(define (new-queen row col) (list row col))
(define (get-row queen) (car queen))
(define (get-col queen) (cadr queen))

(define (add-queen row col position)
	(append position (list (new-queen row col))))

(define (get-row-queens ref-queen position)
	(let ((r (get-row ref-queen)))
		(filter (lambda (q) (= r (get-row q)))
						position)))

(define (get-col-queen col position)
	(car (filter (lambda (q) (= col (get-col q)))
							 position)))

(define (present? row col position)
	(not (null? (filter (lambda (q) (and (= row (get-row q))
																			 (= col (get-col q))))
											position))))

(define (diag-search row row-limit-exceeded? row-op 
										 col col-limit-exceeded? col-op
										 position)
	(cond ((or (row-limit-exceeded? row) (col-limit-exceeded? col))
				 ())
				((not (present? row col position))
				 (diag-search (row-op row 1) row-limit-exceeded? row-op
											(col-op col 1) col-limit-exceeded? col-op
											position))
				(else (append (list (new-queen row col))
											(diag-search (row-op row 1) row-limit-exceeded?
																	 row-op
																	 (col-op col 1) col-limit-exceeded?
																	 col-op
																	 position)))))

(define (top-left start-row start-col position)
	(diag-search start-row
							 (lambda (r) (<= r 0))
							 -
							 start-col
							 (lambda (c) (<= c 0))
							 -
							 position))

(define (last-significant-row position)
	(list-max (map (lambda (q) (get-row q)) position)))

(define (bottom-left start-row start-col position)
	(let ((m (last-significant-row position)))
		(diag-search start-row
								 (lambda (r) (> r m))
								 +
								 start-col
								 (lambda (c) (<= c 0))
								 -
								 position)))

(define (get-diag-queens ref-queen position)
	(let ((r (get-row ref-queen))
				(c (get-col ref-queen)))
		(append (top-left (- r 1) (- c 1) position)
						(bottom-left (+ r 1) (- c 1) position)
						(list ref-queen))))

(define (one-queen? queen-list)
	(= (length queen-list) 1))

(define (row-safe? ref-queen position)
	(one-queen? (get-row-queens ref-queen position)))

(define (diag-safe? ref-queen position)
	(one-queen? (get-diag-queens ref-queen position)))

(define (safe? queen-col-no position)
	(let ((ref-queen (get-col-queen queen-col-no position)))
		(and (row-safe? ref-queen position)
				 (diag-safe? ref-queen position))))

(define (adjoin-position row-num col-num position)
	(add-queen row-num col-num position))
