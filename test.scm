(define a (stream 1 2))
(define b (stream-map (lambda (x) (* 2 x)) a))
