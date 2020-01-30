(define (attach-tag tag data) (cons tag data))

(define (get-tag tagged-data)
	(cond ((number? tagged-data) 'scheme-number)
				((boolean? tagged-data) 'boolean)
				((pair? tagged-data) (car tagged-data))
				(else (error "Bad tagged data: TAG" tagged-data))))

(define (get-content tagged-data)
	(cond ((number? tagged-data) tagged-data)
				((boolean? tagged-data) 'boolean)
				((pair? tagged-data) (cdr tagged-data))
				(else (error "Bad tagged data: CONTENT" tagged-data))))
