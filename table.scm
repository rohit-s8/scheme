(define (make-table dim)
	(let ((table (list '*table*))
				(dimension dim))
		(define (lookup list-of-keys)
			(define (lookup-recur keys subtable)
				(if (null? keys)
					(car subtable)
					(let ((row (assoc (car keys) subtable)))
						(if row
							(lookup-recur (cdr keys) (cdr row))
							(begin (display "Non existent subkey ")
										 (display keys)
										 false)))))
			(if (= (length list-of-keys) dimension)
				(lookup-recur list-of-keys (cdr table))
				(begin (display "Invalid key length ")
							 (display list-of-keys)
							 false)))
		(define (insert! list-of-keys item)
			(define (insert-recur! keys subtable-head item)
				(if (null? keys)
					(set-cdr! subtable-head (list item))
					(let ((row (assoc (car keys) (cdr subtable-head))))
						(if row
							(insert-recur! (cdr keys) row item)
							(let ((new-head (list (car keys))))
								(set-cdr! subtable-head
													(cons new-head (cdr subtable-head)))
								(insert-recur! (cdr keys) new-head item))))))
			(if (= (length list-of-keys) dimension)
				(begin (insert-recur! list-of-keys table item)
							 true)
				(begin (display "Invalid key length ")
							 (display list-of-keys)
							 false)))
		(define (print) 
			(display table)
			(newline))
		(define (get-table) table)
		(define (dispatch m)
			(cond ((eq? m 'insert) insert!)
						((eq? m 'lookup) lookup)
						((eq? m 'print) print)
						((eq? m 'get) get-table)
						(else (error "Unknown method" m))))
		dispatch))

(define (insert-table! table keys item)
	((table 'insert) keys item))
(define (lookup-table table keys)
	((table 'lookup) keys))
(define (print-table table)	((table 'print)))
(define (get-table t) ((t 'get)))
