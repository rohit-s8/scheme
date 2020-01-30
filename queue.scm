(define (make-queue)
	(let ((front '())
				(rear '()))
		(define (set-front! item) 
			(set! front item))
		(define (set-rear! item)
			(set! rear item))
		(define (empty?) (null? front))
		(define (front-item)
			(if (empty?)
					(error "Queue is empty")
					(car front)))
		(define (insert! item)
			(let ((new-pair (cons item '())))
				(cond ((empty?)
							 (set-front! new-pair)
							 (set-rear! new-pair)
							 front)
							(else
								(set-cdr! rear new-pair)
								(set-rear! new-pair)
								front))))
		(define (delete!)
			(cond ((empty?)
						 (error "Queue is empty"))
						(else
							(set-front! (cdr front))
							front)))
		(define (print) front)
		(define (dispatch m)
			(cond ((eq? m 'empty) empty?)
						((eq? m 'front) front-item)
						((eq? m 'insert) insert!)
						((eq? m 'delete) delete!)
						((eq? m 'print) print)
						(else (error "Unknown operation" m))))
		dispatch))

(define (empty-queue? q) ((q 'empty)))
(define (front-queue q) ((q 'front)))
(define (insert-queue! q item)
	((q 'insert) item))
(define (delete-queue! q)
	((q 'delete)))
(define (print-queue q) ((q 'print)))
