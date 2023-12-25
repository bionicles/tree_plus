;; test_scheme.scm from https://cookbook.scheme.org/topological-sort/
(define (topological-sort nodes eq)
  (define table (map (lambda (n) (cons (car n) 0)) nodes))
  (define queue '())
  (define result '())

  (define (set-up)
    ;; Compute the number of nodes that each node depends on.
    (for-each
      (lambda (node)
        (for-each
          (lambda (to)
            (let ((p (assoc to table eq)))
              (if p
                  (set-cdr! p (+ 1 (cdr p)))
                  (set! table (cons (cons to 1) table)))))
          (cdr node)))
      nodes))

  (define (traverse)
    (unless (null? queue)
      (let ((nq (car queue)))
        (set! queue (cdr queue))
        (let ((n0 (assoc nq nodes eq)))
          (when n0
            (for-each
              (lambda (to)
                (let ((p (assoc to table eq)))
                  (when p
                    (let ((cnt (- (cdr p) 1)))
                      (when (zero? cnt)
                        (set! result (cons to result))
                        (set! queue (cons to queue)))
                      (set-cdr! p cnt)))))
              (cdr n0)))
          (traverse)))))

  (set-up)
  (set! queue (map car (filter (lambda (p) (zero? (cdr p))) table)))
  (set! result queue)
  (traverse)
  (let ((rest (filter (lambda (e) (not (zero? (cdr e)))) table)))
    (unless (null? rest)
      (error "Graph has circular dependency" (map car rest))))
  (reverse result))