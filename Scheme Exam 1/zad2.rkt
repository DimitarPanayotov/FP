(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (filter p l)
  (cond ((null? l) l)
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr1 op (cdr l)))))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))




(define (run-machine instructions)
  (define (is-number? x) (or (integer? x) (real? x)))
  (define (apply-function func stack)
    (map (lambda (x) (if (is-number? x) (func x) x)) stack))

  (define (apply-pair op n stack)
    (define (apply-iter count stack)
      (if (or (<= count 0) (< (length stack) 2) (symbol? (car stack)))
          stack
          (let ((a (car stack))
                (b (cadr stack))
                (rest (cddr stack)))
            (apply-iter (- count 1) (cons (op a b) rest)))))
    (apply-iter n stack))

  (define (process stack instruction)
    (cond ((is-number? instruction) (cons instruction stack))
          ((symbol? instruction) (cons instruction stack))
          ((procedure? instruction) (apply-function instruction stack))
          ((and (pair? instruction) (procedure? (car instruction)) (is-number? (cdr instruction)))
           (apply-pair (car instruction) (cdr instruction) stack))
          (else stack)))

  (foldl process '() instructions))
