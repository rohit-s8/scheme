(define (insert a sorted-l)
	(cond ((null? sorted-l) (list a))
				((< a (car sorted-l)) (cons a sorted-l))
				(else (cons (car sorted-l) (insert a (cdr sorted-l))))))

(define (sort l)
	(if (null? l) l
			(insert (car l) (sort (cdr l)))))
