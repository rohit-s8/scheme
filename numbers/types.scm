(define (make-type-system)
	(let ((type-table (make-table 1))
				(raise-table (make-table 2))
				(drop-table (make-table 1)))

		;;; type table utilities
		(define (add-type! type priority)
			(insert-table! type-table (list type) priority)
			(insert-table! type-table (list priority) type))

		(define (get-type-priority type)
			(lookup-table type-table (list type)))

		(define (get-type-for-priority priority)
			(lookup-table type-table (list priority)))

		(define (above? type1 type2)
			(> (get-type-priority type1) (get-type-priority type2)))

		(define (same? type1 type2) (eq? type1 type2))
	
		;; get the next higher type
		(define (next-type type)
			(let ((cur-pri (get-type-priority type)))
				(let ((new-type (get-type-for-priority (+ cur-pri 1))))
					(if new-type
						new-type
						type))))

		;; return the type with higher priority
		(define (higher-type type1 type2)
			(if (above? type1 type2)
				type1
				type2))

		;; get the highest priority type from a list of args
		(define (highest-type args)
			(if (null? (cdr args))
				(get-tag (car args))
				(higher-type (get-tag (car args)) (highest-type (cdr args)))))


		;;; raise table utilities
		(define (add-raise-proc! init-type target-type proc)
			(insert-table! raise-table (list init-type target-type) proc))

		(define (get-raise-proc init-type target-type)
			(let ((proc (lookup-table raise-table
																(list init-type target-type))))
				(if proc
					proc
					(error "No valid type conversion method found "
								 init-type "-->" target-type))))

		;; raise an argument to the target type
		(define (raise-arg-target arg target-type)
			(if (or (above? (get-tag arg) target-type)
							(same? (get-tag arg) target-type))
				arg
				(let ((raise-proc (get-raise-proc (get-tag arg) target-type)))
					(raise-proc arg))))

		;; raise an argument to the next higher type
		(define (raise-arg arg)
			(let ((target-type (next-type (get-tag arg))))
				(raise-arg-target arg target-type)))

		;; homogenize a list of args to the highest type
		(define (homogenize args)
			(let ((target-type (highest-type args)))
				(map (lambda (arg) (raise-arg-target arg target-type))
						 args)))

		;;; drop table utilities
		(define (add-drop-proc! type proc)
			(insert-table! drop-table (list type) proc))

		(define (get-drop-proc type)
			(lookup-table drop-table (list type)))


		;; simplify data to the lowest type possible
		(define (simplify data)
			(let ((type (get-tag data)))
				(let ((simplify-proc (get-drop-proc type)))
					(if simplify-proc
						(simplify-proc data)
						data))))
		
		;; external interface
		(define (dispatch m)
			(cond ((eq? m 'add-type) add-type!)
						((eq? m 'add-raise-proc) add-raise-proc!)
						((eq? m 'add-drop-proc) add-drop-proc!)
						((eq? m 'raise-arg) raise-arg)
						((eq? m 'homogenize) homogenize)
						((eq? m 'simplify) simplify)
						(else "TYPE SYSTEM: Invalid method called: " m)))

		dispatch))

(define (add-type-ts! ts type priority)
	((ts 'add-type) type priority))

(define (add-raise-proc-ts! ts init-type target-type proc)
	((ts 'add-raise-proc) init-type target-type proc))

(define (add-drop-proc-ts! ts type proc)
	((ts 'add-drop-proc) type proc))

(define (raise-arg-ts ts arg)
	((ts 'raise-arg) arg))

(define (homogenize-ts ts list-of-args)
	((ts 'homogenize) list-of-args))

(define (simplify-ts ts data)
	((ts 'simplify) data))
